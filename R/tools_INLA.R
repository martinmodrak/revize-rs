inla_pipeline <- function(base_data, kategorie, formula_base, uzite_mc_sloupce, n_samples = 200, kompetence_to_run = kompetence,
                          n_cores = parallel::detectCores()) {
  data_for_inla <- make_data_for_inla(base_data, kategorie, uzite_mc_sloupce)

  formula <- update(formula_base, as.formula(paste0(". ~ . ", data_for_inla$mc_formula_str)))

  regression_inputs <- list()
  for(k in kompetence_to_run) {
    regression_inputs[[k]] <- data_for_inla$data_for_inla %>%
      filter(kompetence == k) %>% mutate(row_id = 1:n())
  }

  fit_dir <- here::here("local_data","inla_fits")
  if(!dir.exists(fit_dir)) {
    dir.create(fit_dir)
  }

  cache_filename <- paste0(fit_dir,"/results_", kategorie, "_", formula_to_cache_name(formula),".rds")


  result <- NULL
  if(file.exists(cache_filename)) {
    message("Cache file exists")
    cache_contents <- readRDS(cache_filename)
    if(identical(cache_contents$kategorie, kategorie) &&
       identical(cache_contents$formula, formula) &&
       isTRUE(all.equal(cache_contents$regression_inputs, regression_inputs, check.attributes = FALSE))) {
      if(cache_contents$n_samples < n_samples) {
        warning("N_samples larger, recomputing")
      } else {
        result <- cache_contents
      }
    } else {
      warning("Cache file is not compatible, recomputing")
    }
  }

  # Potrebuju spocitat
  if(is.null(result)) {

    cl <- parallel::makeCluster(min(n_cores, length(kompetence_to_run)))

    r_basedir <- here::here("R")
    parallel::clusterExport(cl,
                            c("formula", "fit_dir", "kategorie","r_basedir"),
                            envir = environment())
    parallel::clusterEvalQ(cl, {
      library(INLA); library(tidyverse); library(purrr); library(formula.tools);
      source(paste0(r_basedir,"/tools_INLA.R"), encoding = "UTF-8")
      source(paste0(r_basedir,"/tools_kompetence.R"), encoding = "UTF-8")

    })

    inputs_and_names <- regression_inputs %>% imap( ~ list(name = .y, input = .x))

    processed_fits <- parallel::parLapplyLB(cl, inputs_and_names, function(x) {
    #processed_fits <- lapply(inputs_and_names, function(x) {
      fit_cache_filename <- paste0(fit_dir, "/fit_", kategorie,"_",x$name,"_", formula_to_cache_name(formula), ".rds")
      fit <- NULL
      if(file.exists(fit_cache_filename)) {
        cache_contents <- readRDS(fit_cache_filename)
        if(identical(cache_contents$formula, formula) &&
           isTRUE(all.equal(cache_contents$input, x$input, check.attributes = FALSE))) {
          fit <- cache_contents$fit
          cache_result <- "Loaded"
        } else {
          cache_result <- "Mismatch"
        }
      } else {
        cache_result <- "Not found"
      }

      if(is.null(fit)) {
        fit <- my_inla_fit(formula, x$input, num.threads = 1)
        saveRDS(list(formula = formula, input = x$input, fit = fit), file = fit_cache_filename)
      }

      list(
        pp_samples_matrix = inla_samples_to_matrix(inla.posterior.sample(n_samples, fit)),
        marginals_summary = marginals_summary_from_fit(fit),
        summary.hyperpar = fit$summary.hyperpar,
        cache_result = cache_result
      )
    })
    names(processed_fits) <- names(regression_inputs)
    stopCluster(cl)

    result <- list(kategorie = kategorie,
                   formula = formula,
         regression_inputs = regression_inputs,
         n_samples = n_samples,
         processed_fits = processed_fits)

    saveRDS(result, cache_filename)
  }

  # Samply pocitam vzdy, nevyplati se ukladat ani prenaset
  result$predicted_pp_checks <- list()
  for(k in kompetence_to_run) {
    result$predicted_pp_checks[[k]] <-
      pp_samples_from_matrix(formula, result$processed_fits[[k]]$pp_samples_matrix, regression_inputs[[k]])
  }

  result
}

formula_to_cache_name <- function(formula) {
  openssl::md5(as.character(formula))
}

marginals_summary_from_fit <- function(fit) {
  summary_random <-
    map_dfr(fit$marginals.random,
              ~  map_dfr(.,  marginals_summary_single, .id = "index"),
        .id = "marginal")

  summary_fixed <-
    map_dfr(fit$marginals.fixed, marginals_summary_single, .id = "marginal") %>%
    mutate(index = "")

  summary_hyperpar <-
    map_dfr(fit$marginals.hyperpar, marginals_summary_single, .id = "marginal") %>%
    mutate(index = "")

  rbind(summary_random, summary_fixed)
}


marginals_summary_single <- function(marginal) {
  probs <- c(0.025, 0.05, 0.1, 0.5, 0.9, 0.95, 0.975)
  cdf_at_0 <- inla.pmarginal(0, marginal)
  as.data.frame(
    matrix(
      inla.qmarginal(p = probs, marginal),
      ncol = length(probs),
      dimnames = list(NULL, paste0("q",probs))
    )
  ) %>% mutate(widest_ci_sign = 2 * (0.5 - cdf_at_0))
}

make_data_for_inla <- function(base_data, kategorie, uzite_mc_sloupce) {
  data_for_inla <- base_data %>%
    filter(kategorie_kompetence == kategorie) %>%
    group_by(session) %>%
    odvozena_meritka_kompetenci() %>%
    ungroup() %>%
    mutate(age_norm = (age - 20.5) / 2.75,
           age_ar = age - min(age) + 1,
           kategorie_respondenta_full = factor(kategorie_respondenta_full)
           )

  mc_formula_str <- ""
  for(sloupec in uzite_mc_sloupce) {
    obsah_sloupce <- data_for_inla %>% pull(!!sloupec)
    mc_matrix <- rozsir_mc_matrix(data_for_inla, sloupec)
    if(any(is.na(obsah_sloupce))) {
      na_matrix <- matrix(data = is.na(obsah_sloupce), ncol = 1, nrow = nrow(data_for_inla))
      colnames(na_matrix) <- paste0(as_label(sloupec),"_NA")
      mc_matrix <- cbind(mc_matrix, na_matrix)
    }

    ind_matrix <- matrix(1:ncol(mc_matrix), nrow = nrow(data_for_inla), ncol = ncol(mc_matrix), byrow = TRUE)
    colnames(ind_matrix) <- paste0("id.", colnames(mc_matrix))
    colnames(ind_matrix)[1] <- paste0("id.", as_label(sloupec))

    mc_formula_str_sloupec <- paste0(
      ' + f(', colnames(ind_matrix)[1], ', ', colnames(mc_matrix)[1],
#      ', model = "generic3", Cmatrix = list(diag(', ncol(mc_matrix), ')))')
    ', model = "generic3", Cmatrix = list(diag(', ncol(mc_matrix),
    ')), hyper = list(theta1 = list(prior = log_sqrt_inv_hn(1))))')

    for(i in 2:ncol(mc_matrix)) {
      mc_formula_str_sloupec <- paste0(
        mc_formula_str_sloupec,
        ' + f(', colnames(ind_matrix)[i], ', ', colnames(mc_matrix)[i],
        ', copy = "',colnames(ind_matrix)[1],'")'
      )
    }

    # Use as fixed effects
    #mc_formula_str_sloupec <- paste0(' + ', colnames(mc_matrix), collapse = "")


    mc_formula_str <- paste0(mc_formula_str, mc_formula_str_sloupec)
    data_for_inla <- cbind(data_for_inla, as_tibble(mc_matrix), as_tibble(ind_matrix))
  }
  list(data_for_inla = data_for_inla, mc_formula_str = mc_formula_str)
}



inla_samples_to_matrix <- function(samples) {
  names <- c(names(samples[[1]]$hyperpar), rownames(samples[[1]]$latent), "logdens.hyperpar","logdens.latent","logdens.joint")
  data <- matrix(NA, nrow = length(samples), ncol = length(names))
  colnames(data) <- names
  for(i in 1:length(samples)) {
    data[i,] <- c(samples[[i]]$hyperpar, samples[[i]]$latent,
                  ifelse(is.null(samples[[i]]$logdens$hyperpar), -Inf, samples[[i]]$logdens$hyperpar),
                  samples[[i]]$logdens$latent, samples[[i]]$logdens$joint)
  }

  data
}

samples_colnames <- function(base, ids) {
  pattern <- paste0(base,":%d")
  sprintf(pattern, ids)
}

my_inla_fit <- function(model_formula, data, num.threads = inla.getOption("num.threads")) {
  meritko_nazev <- as.character(formula.tools::lhs(model_formula))
  meritko_info <- meritka_kompetence[[meritko_nazev]]
  if(is.null(meritko_info)) {
    stop("Nezname meritko")
  }

  # Zatim approximuji ordinal jako beta binomial
  if(meritko_info$type == "ordinal") {
    additional.args = list(family = "betabinomial", Ntrials = 7)
  } else if(meritko_info$type == "interval") {
    additional.args = list(family = "gaussian")
  } else if(meritko_info$type == "bool") {
    additional.args = list(family = "binomial", Ntrials = 1)
    data[[meritko_nazev]] <- as.integer(data[[meritko_nazev]])
  } else {
    stop("Neznamy typ meritka")
  }

  all_args <- c(list(formula = model_formula, data = data, control.compute = list(config=TRUE)),
                additional.args, num.threads = num.threads)

  do.call(inla, all_args)
}

pp_samples_from_matrix <- function(model_formula, pp_samples_matrix, regression_input) {
  n_samples <- nrow(pp_samples_matrix)
  pp_linpred <- pp_samples_matrix[, samples_colnames("Predictor", regression_input$row_id)] %>%
    as_tibble() %>%
    mutate(sample_id = 1:n_samples) %>%
    pivot_longer(-sample_id, names_to = "row_id", names_prefix = "Predictor:", values_to = "linpred") %>%
    mutate(row_id = as.integer(row_id))

  meritko_nazev <- as.character(lhs(model_formula))
  meritko_info <- meritka_kompetence[[meritko_nazev]]
  if(is.null(meritko_info)) {
    stop("Nezname meritko")
  }

  if(meritko_info$type == "ordinal") {
    overdisp_samples <- pp_samples_matrix[,"overdispersion for the betabinomial observations"]
    res <- pp_linpred %>%
      inner_join(tibble(sample_id = 1:n_samples, overdispersion = overdisp_samples),
                 by = c("sample_id" = "sample_id")) %>%
      mutate(response_prob_mean = 1/(1 + exp(-linpred)),
             prec = 1/overdispersion + 1,
             response_prob = rbeta(n(), response_prob_mean * prec, (1 - response_prob_mean) * prec),
             response = rbinom(n(), size = 7, prob = response_prob))
  } else if(meritko_info$type == "interval") {
    precision_samples <- pp_samples_matrix[,"Precision for the Gaussian observations"]
    res <- pp_linpred %>%
      inner_join(tibble(sample_id = 1:n_samples, precision = precision_samples),
                 by = c("sample_id" = "sample_id")) %>%
      mutate(response = rnorm(n(),mean = linpred, sd = sqrt(1/precision)))
  } else if(meritko_info$type == "bool") {
    res <- pp_linpred %>%
      mutate(response_prob = 1/(1 + exp(-linpred)),
             response = rbinom(n(), size = 1, prob = response_prob))
  } else {
    stop("Neznamy typ meritka")
  }

  res
}

my_pp_check <- function(pipeline_result, group, stat = mean, label = as_label({{ group }}), to_baseline = TRUE, subset_kompetence = NULL) {
  meritko_nazev <- as.character(lhs(pipeline_result$formula))
  meritko_info <- meritka_kompetence[[meritko_nazev]]
  if(is.null(meritko_info)) {
    stop("Nezname meritko")
  }

  group <- enquo(group)

  regression_inputs <- pipeline_result$regression_inputs
  predicted_pp_checks <-  pipeline_result$predicted_pp_checks

  data_predicted <- list()
  data_observed <- list()
  for(k in names(regression_inputs)) {
    # Odstranit otravny warning
    regression_inputs[[k]] <- regression_inputs[[k]] %>% mutate(group_char = !!group)

    data_predicted[[k]] <- predicted_pp_checks[[k]] %>%
      inner_join(regression_inputs[[k]] %>% select(row_id, kompetence, group_char), by = c("row_id" = "row_id")) %>%
      group_by(sample_id, kompetence,  group_char) %>%
      summarise(response_agg = stat(response)) %>%
      group_by(kompetence, group_char) %>%
      summarise(mid = mean(response_agg), low = quantile(response_agg, 0.025), high = quantile(response_agg, 0.975))

    regression_inputs[[k]]$meritko <- regression_inputs[[k]][[meritko_nazev]]
    data_observed[[k]] <- regression_inputs[[k]] %>% group_by(kompetence, group_char)  %>%
      summarise(mid = stat(meritko))
  }

  data_predicted_all <- do.call(rbind, data_predicted)
  data_observed_all <- do.call(rbind, data_observed)

  if(to_baseline) {
    data_predicted_all <- data_predicted_all %>%
      inner_join(data_observed_all, by = c("kompetence" = "kompetence", "group_char" = "group_char"),
                 suffix = c("",".observed")) %>%
      mutate(mid = mid - mid.observed, low = low - mid.observed, high = high - mid.observed)

    data_observed_all$mid <- 0
  }

  if(!is.null(subset_kompetence)) {
    data_predicted_all <- data_predicted_all %>% filter(kompetence %in% subset_kompetence)
    data_observed_all <- data_observed_all %>% filter(kompetence %in% subset_kompetence)
  }

  data_predicted_all %>% ggplot(aes(x = group_char, y = mid)) +
    geom_linerange(aes(ymin = low, ymax = high)) +
    geom_point() +
    geom_line(aes(y = mid, group = kompetence), color = revize_cols(2), data = data_observed_all) +
    scale_x_discrete(label) +
    facet_wrap(~kompetence) + ggtitle(paste0(pipeline_result$kategorie, " - ", lhs(pipeline_result$formula))) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 1))

}

#Log - Sqrt(Inverse(Half normal))prior, implies Half normal prior on sd
log_sqrt_inv_hn <- function(sigma = 1) {
  paste0("expression:
sigma = ", sigma, ";
x = exp(log_x);
logdens = - 1.5 * log_x - log(sigma) - 0.5 * (log(2) + log(pi)) -1/(2 * sigma * sigma * x);
log_jacobian = log_x;
return(logdens + log_jacobian);
")
}
