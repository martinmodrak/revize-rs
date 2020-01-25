residual_clmm <- function(data, fit, x_aesthethic, color_aesthetic = NULL, shape_aesthetic = NULL) {
  thresholds_low <- c(-Inf, fit$alpha)
  thresholds_high <- c(fit$alpha, Inf)
  data %>%
    mutate(fitted = fit$fitted.values,
           low_actual = thresholds_low[as.integer(kompetence_odpoved)],
           high_actual = thresholds_high[as.integer(kompetence_odpoved)],
           residual = case_when(fitted < low_actual ~ fitted - low_actual,
                                fitted > high_actual ~ fitted - high_actual,
                                TRUE ~ 0)) %>%
    ggplot(aes(x = {{ x_aesthethic }}, y = residual, shape = {{ shape_aesthetic }}, color = {{ color_aesthetic }})) + geom_boxplot() + geom_jitter(width = 0.3, alpha = 0.4)
}
