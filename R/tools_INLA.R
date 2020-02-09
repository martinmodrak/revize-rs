library(formula.tools)

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

my_inla_fit <- function(model_formula, data) {
  meritko_nazev <- as.character(lhs(model_formula))
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
                additional.args)

  do.call(inla, all_args)
}

matrix_samples_from_fits <- function(fits, n_samples) {
  cl <- parallel::makeCluster(parallel::detectCores(), useXDR = FALSE)
  parallel::clusterExport(cl, c("inla_samples_to_matrix", "n_samples"))
  parallel::clusterEvalQ(cl, { library(INLA) })
  ret <- parallel::parLapply(cl, fits, function(fit) {
    inla_samples_to_matrix(inla.posterior.sample(n_samples, fit))
  })
  stopCluster(cl)
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

my_pp_check <- function(model_formula, regression_inputs, predicted_pp_checks, group, stat = mean) {
  meritko_nazev <- as.character(lhs(model_formula))
  meritko_info <- meritka_kompetence[[meritko_nazev]]
  if(is.null(meritko_info)) {
    stop("Nezname meritko")
  }


  data_predicted <- list()
  data_observed <- list()
  for(k in kompetence) {
    # Odstranit otravny warning
    if(regression_inputs[[k]] %>% pull({{group}}) %>% inherits("haven_labelled")) {
      regression_inputs[[k]] <- regression_inputs[[k]] %>% mutate({{group}} := as.character({{group}}))
    }

    data_predicted[[k]] <- predicted_pp_checks[[k]] %>%
      inner_join(regression_inputs[[k]] %>% select(row_id, kompetence, {{group}}), by = c("row_id" = "row_id")) %>%
      group_by(sample_id, kompetence,  {{group}}) %>%
      summarise(response_agg = stat(response)) %>%
      group_by(kompetence, {{group}}) %>%
      summarise(mid = mean(response_agg), low = quantile(response_agg, 0.025), high = quantile(response_agg, 0.975))

    regression_inputs[[k]]$meritko <- regression_inputs[[k]][[meritko_nazev]]
    data_observed[[k]] <- regression_inputs[[k]] %>% group_by(kompetence, {{group}})  %>%
      summarise(mid = stat(meritko))
  }

  data_predicted_all <- do.call(rbind, data_predicted)
  data_observed_all <- do.call(rbind, data_observed)

  data_predicted_all %>% ggplot(aes(x = {{group}}, y = mid)) +
    geom_linerange(aes(ymin = low, ymax = high)) +
    geom_point() +
    geom_line(aes(y = mid, group = kompetence), color = "lightblue", data = data_observed_all) +
    scale_x_discrete() +
    facet_wrap(~kompetence)

}

