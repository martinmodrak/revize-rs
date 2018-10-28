library(bayesplot)

run_pp_checks <- function(fit, data,
                          types = c("celkem","vek","pohlavi","rok"),
                          prediction_filter = NULL,
                          out_func = print) {

  predicted <- posterior_predict(fit, nsamples = 1000)

  if(!is.null(prediction_filter)) {
    predicted <- predicted[,prediction_filter]
    data <- data[prediction_filter,]
  }

  if("celkem" %in% types) {
    ppc_stat(data$n_ukonceno, predicted, stat = "sum") %>% out_func
    #my_ppc_bars(data$n_ukonceno, predicted) %>% out_func
  }
  if("vek" %in% types) {
    ppc_stat_grouped(data$n_ukonceno, predicted, stat = "sum", group = data$vek) %>% out_func
  }
  if("pohlavi" %in% types) {
    ppc_stat_grouped(data$n_ukonceno, predicted, stat = "sum", group = data$ID_Sex) %>% out_func
  }

  if("rok_narozeni" %in% types) {
    ppc_stat_grouped(data$n_ukonceno, predicted, stat = "sum", group = data$rok_narozeni) %>% out_func
  }
  if("rok" %in% types) {
    ppc_stat_grouped(data$n_ukonceno, predicted, stat = "sum", group = data$Year) %>% out_func
  }
}
