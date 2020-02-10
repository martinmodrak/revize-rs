library(formula.tools)

inla_pipeline <- function(base_data, kategorie, formula, n_samples = 200, kompetence_to_run = kompetence, save_cache = TRUE) {
  data_for_inla <- base_data %>%
    filter(kategorie_kompetence == kategorie) %>%
    group_by(session) %>%
    odvozena_meritka_kompetenci() %>%
    ungroup() %>%
    mutate(age_norm = (age - 20.5) / 2.75,
           age_ar = age - min(age) + 1)

  mc_matrices <- list()
  for(sloupec in mc_sloupce) {
    na_matrix <- matrix(data = is.na(data_for_inla %>% pull(!!sloupec)), ncol = 1, nrow = nrow(data_for_inla))
    colnames(na_matrix) <- paste0(as_label(sloupec),"_NA")
    mc_matrix <- cbind(mc_to_matrix(data_for_inla, sloupec), na_matrix)
    mc_matrices[[as_label(sloupec)]] <- mc_matrix
  }

  regression_inputs <- list()
  for(k in kompetence_to_run) {
    regression_inputs[[k]] <- data_for_inla %>% filter(kompetence == k) %>% mutate(row_id = 1:n())
  }

  fit_dir <- here::here("local_data","inla_fits")
  if(!dir.exists(fit_dir)) {
    dir.create(fit_dir)
  }

  cache_filename <- paste0(fit_dir,"/fit_", kategorie, "_", openssl::md5(as.character(formula)),".rds")


  result <- NULL
  if(file.exists(cache_filename)) {
    message("Cache file exists")
    cache_contents <- readRDS(cache_filename)
    if(identical(cache_contents$kategorie, kategorie) &&
       identical(cache_contents$formula, formula) &&
       isTRUE(all.equal(cache_contents$regression_inputs, regression_inputs, check.attributes = FALSE))) {
      if(cache_contents$n_samples < n_samples) {
        message("N_samples larger, computing more samples")
        cache_contents$n_samples = n_samples
        cache_contents$pp_samples_matrices <- matrix_samples_from_fits(cache_contents$inla_fits, n_samples)
        if(save_cache)  {
          saveRDS(cache_contents, cache_filename)
        }
      }
      result <- cache_contents
    } else {
      warning("Cache file is not compatible, recomputing")
    }
  }

  # Potrebuju spocitat
  if(is.null(result)) {

    # cl <- parallel::makeCluster(parallel::detectCores(), useXDR = FALSE)
    #
    # parallel::clusterExport(cl, c("meritka_kompetence", "formula", "my_inla_fit"), envir = environment())
    # parallel::clusterEvalQ(cl, { library(INLA) })
    # inla_fits <- parallel::parLapplyLB(cl, regression_inputs, function(input) {
    #   my_inla_fit(formula, input, num.threads = 1)
    # })
    # stopCluster(cl)
    inla_fits <- list()
    for(k in kompetence_to_run) {
      inla_fits[[k]] <- my_inla_fit(formula, regression_inputs[[k]])
    }

    pp_samples_matrices <- matrix_samples_from_fits(inla_fits, n_samples)

    result <- list(kategorie = kategorie,
         formula = formula,
         regression_inputs = regression_inputs,
         n_samples = n_samples,
         inla_fits = inla_fits,
         pp_samples_matrices = pp_samples_matrices)

    if(save_cache) {
      saveRDS(result, cache_filename)
    }
  }

  # Samply pocitam vzdy, nevyplati se ukladat
  result$predicted_pp_checks <- list()
  for(k in kompetence_to_run) {
    result$predicted_pp_checks[[k]] <-
      pp_samples_from_matrix(formula, result$pp_samples_matrices[[k]], regression_inputs[[k]])
  }

  result
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

matrix_samples_from_fits <- function(fits, n_samples) {
  # cl <- parallel::makeCluster(parallel::detectCores(), useXDR = FALSE)
  # parallel::clusterExport(cl, c("inla_samples_to_matrix", "n_samples"), envir = environment())
  # parallel::clusterEvalQ(cl, { library(INLA) })
  # ret <- parallel::parLapplyLB(cl, fits, function(fit) {
  #   inla_samples_to_matrix(inla.posterior.sample(n_samples, fit))
  # })
  # stopCluster(cl)
  ret <- list()
  for(k in names(fits)) {
    ret[[k]] <- inla_samples_to_matrix(inla.posterior.sample(n_samples, fits[[k]]))
  }
  ret
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

my_pp_check <- function(pipeline_result, group, stat = mean, label = as_label({{ group }})) {
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
  for(k in kompetence) {
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

  data_predicted_all %>% ggplot(aes(x = group_char, y = mid)) +
    geom_linerange(aes(ymin = low, ymax = high)) +
    geom_point() +
    geom_line(aes(y = mid, group = kompetence), color = "lightblue", data = data_observed_all) +
    scale_x_discrete(label) +
    facet_wrap(~kompetence)

}

