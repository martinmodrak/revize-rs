plot_spokojenost <- function(spokojenost, group, plot = "hist") {
  spokojenost$group <- spokojenost[[group]]
  spokojenost <- spokojenost %>% filter(!is.na(group))
  pocet_skupiny <- spokojenost %>% group_by(group) %>% summarise(pocet_skupiny = length(id))

  spokojenost_dle_group <- spokojenost %>% group_by(celkova_spokojenost_s_programem_rs_kmenu, group) %>% summarise(pocet_spokojenych = length(id))

  spokojenost_dle_group <- spokojenost_dle_group %>%
    inner_join(pocet_skupiny, by = c("group" = "group")) %>%
    mutate(podil_spokojenych = pocet_spokojenych / pocet_skupiny)

  if(plot == "heatmap") {
    spokojenost_dle_group %>% ggplot(aes(x = group, y = celkova_spokojenost_s_programem_rs_kmenu, fill = podil_spokojenych)) + geom_bin2d(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_fill_scico(palette = "roma") + xlab(group)

  } else if (plot == "hist") {
    mean_spokojenost <- spokojenost %>% group_by(group) %>%
      summarise(mean_spokojenost = mean(as.integer(celkova_spokojenost_s_programem_rs_kmenu)))

    spokojenost_dle_group %>% ggplot(aes(x = celkova_spokojenost_s_programem_rs_kmenu, y = podil_spokojenych)) + geom_bar( stat = "identity") +
      geom_label(data = pocet_skupiny, aes(label = pocet_skupiny), x = 1, y = 0.5, inherit.aes = FALSE) +
      geom_vline(data = mean_spokojenost, aes(xintercept = mean_spokojenost), color = "blue", size = 2) +
      facet_wrap(~group) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab(group)
  } else {
    stop("Unrecognized plot")
  }
}

recode_frekvence <- function(x) {
  #Davame default "Nikdy"
  recode(x, .missing = 1,
         `Nikdy` = 1,
         `Méně často než 1 za rok` = 2,
         `Asi jednou za rok` = 3,
         `Asi jednou za půl roku` = 4,
         `Asi jednou za čtvrt roku` = 5,
         `Asi jednou měsíčně` = 6,
         `Asi jednou za 14 dní` = 7,
         `Každý týden nebo častěji` = 8
         ) %>% as.integer()
}

order_by_response_positive <- function(orig_data, var_name, var_values = levels(orig_data[[var_name]])) {
  poradi_df = orig_data %>%
    mutate(group_var = orig_data[[var_name]]) %>%
    group_by(group_var) %>%
    summarise(mean_resp = mean(response_positive)) %>%
    ungroup() %>%
    mutate(poradi = order(mean_resp))

  poradi <-  poradi_df$poradi
  names(poradi) <- poradi_df$group_var

  poradi[var_values]
}

effects_by_prediction <- function(fit, orig_data, var_name, var_values, var_labels = var_values,
                                  response_title, response_to_value_func, n_samples = 500, max_lines = 200, plot_type = "all") {

  data_for_prediction_all_values <- var_values %>% imap_dfr(function(var_value, var_value_index) {
    data_for_prediction <- orig_data
    data_for_prediction[, var_name] <- var_value
    data_for_prediction$var_value_index <- var_value_index
    data_for_prediction
  })

  prediction_array <- posterior_linpred(fit, newdata = data_for_prediction_all_values, nsamples = n_samples, transform = TRUE)
  prediction <- apply(prediction_array, MARGIN = c(1,2), FUN = response_to_value_func)

  predicted_data <- prediction %>% t() %>% as.tibble() %>%
    mutate(id = data_for_prediction_all_values$id,
           var_value = factor(data_for_prediction_all_values[[var_name]], levels = var_values, labels = var_labels),
           var_value_index = data_for_prediction_all_values$var_value_index) %>%
    gather("sample", "response", -id, -var_value, -var_value_index) %>%
    mutate(sample = factor(sample), id_sample = factor(paste(id, sample, sep = "__")))

  predicted_data_for_lines <- predicted_data %>%
    filter(id_sample %in% sample(unique(id_sample), max_lines))


  positive_label = "kladná"
  negative_label = "záporná"

  data_sign <- predicted_data_for_lines %>% filter(var_value_index < length(var_values)) %>%
    mutate(next_var_value_index = var_value_index + 1) %>%
    inner_join(predicted_data_for_lines %>% select(id_sample, response, var_value_index) %>% rename(next_response = response, next_var_value_index = var_value_index),
               by = c("id_sample" = "id_sample", "next_var_value_index" = "next_var_value_index")) %>%
    mutate(sign = if_else(response < next_response, positive_label,negative_label) %>%
             factor(levels = c(negative_label, positive_label))) %>%
    select(var_value_index, sign, id_sample)

  predicted_data_for_lines_sign <- predicted_data_for_lines %>%
    left_join(data_sign, by = c("var_value_index","id_sample")) %>%
    mutate(sign = if_else(is.na(sign), factor(positive_label, levels = c(negative_label,positive_label)),sign))

  orig_data$var_value = orig_data[[var_name]]
  if(is.numeric(orig_data$var_value)) {
    orig_data <- orig_data %>%
      rowwise() %>%
      mutate(var_value = var_values[which.min(abs(var_values-var_value))]) %>% #Find the nearest value
      ungroup()
  }

  orig_data_for_plot <- orig_data %>%
    mutate(var_value = factor(var_value, levels = var_values, labels = var_labels)) %>%
    group_by(var_value) %>%
    summarise(p_vetsi = mean(response_positive),
              pocet = length(response_positive))


  if(plot_type == "all") {
    sample_geom = geom_line(data = predicted_data_for_lines_sign,
                            mapping = aes(x = var_value, y = response, group = id_sample, color = sign),
                            inherit.aes = FALSE,
                            alpha = 0.2)
  } else {
    sample_geom = NULL
  }

  if(plot_type == "all" || plot_type == "orig_estimate") {
    estimate_geom1 =  geom_linerange(aes(ymin = lower50, ymax = upper50), size = 2)
    estimate_geom2 =  geom_linerange(aes(ymin = lower, ymax = upper))
  } else {
    estimate_geom1 = NULL
    estimate_geom2 = NULL
  }

  predicted_data %>%
    group_by(var_value) %>%
    summarise(Estimate = median(response),
              lower = quantile(response, 0.025),
              upper = quantile(response, 0.975),
              lower50 = quantile(response, 0.25),
              upper50 = quantile(response, 0.75)
    ) %>%
    ggplot(aes(x = var_value)) +
    sample_geom +
    geom_point(data = orig_data_for_plot, mapping = aes(y = p_vetsi, size = pocet), color = "#2ca25f",
               position = position_nudge(x = 0.05) ) +
    estimate_geom1 + estimate_geom2 +
    scale_color_discrete("Asociace", drop = FALSE) +
    scale_y_continuous(response_title, limits = c(0,1)) +
    scale_x_discrete(var_name) +
    scale_size_continuous(range = c(2,8)) +
    guides(color = guide_legend(override.aes = list(alpha=1))) +
    if(length(var_values) > 4 && max(nchar(var_labels) > 4)) {
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
    } else {
      NULL
    }

}
