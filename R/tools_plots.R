dark_blue_color <- "#0040ae"
darkest_fill <- "#668cce"
midd_fill <- "#b2c5e6"


set_theme_revizers <- function() {
  theme_set(cowplot::theme_cowplot())
  theme_update(text = element_text(family = "Roboto", color = "white"),
               plot.background = element_rect(fill = dark_blue_color),
               line = element_line(color = "white"),
               rect = element_rect(color = "white"),
               axis.text = element_text(color = "white", face = "bold"),
               axis.title = element_text(color = "white"),
               axis.line = element_line(color = "white"), axis.ticks = element_line(color = "white"),
               strip.background = element_rect(color = "white", fill = darkest_fill))
  windowsFonts("SKAUT" = windowsFont("SKAUT Bold"))
  windowsFonts("Roboto" = windowsFont("Roboto"))
  #sysfonts::font_add_google("Roboto")
  #sysfonts::font_add(family = "SKAUT Bold", paste0(Sys.getenv("APPDATA"),"/../Local/Microsoft/Windows/Fonts/skaut-bold-webfont.otf"))
  #showtext_auto()

  update_geom_defaults("bar",   list(fill = "white"))
}

popis_pro_plot <- function(data, sloupec) {
  sloupec_val <- cela_data %>% pull( {{sloupec }})
  if(inherits(sloupec_val, "haven_labelled")) {
    raw_popis <- attributes(sloupec_val)$label
    gsub("*","", raw_popis, fixed = TRUE) %>%
      gsub("`[^`]*`", "", .)
  } else {
    # Hack protoze neumim quasiquotation
    names(cela_data %>% select( {{sloupec}}))
  }
}


plot_summary_mc <- function(cela_data, sloupec, title = popis_pro_plot(cela_data, {{ sloupec }}),
                            title_hjust = 1, order_by_podil = TRUE, invert_color_threshold = 0.06 ) {
  data_to_plot <- summarise_multiple_choice(cela_data, {{ sloupec }})

  wrap_width <- 45
  if(order_by_podil) {
    data_to_plot <- data_to_plot %>%
      mutate(nazev_volby =  fct_reorder(str_wrap(nazev_volby, wrap_width), podil_ano))
  } else {
    labels <- attributes(cela_data %>% pull( {{ sloupec }}))$labels
    data_to_plot <- data_to_plot %>%
      mutate(nazev_volby = factor(id_volby, levels = labels, labels = str_wrap(names(labels), wrap_width)))
  }
  data_to_plot %>%
    ggplot(aes(x = nazev_volby, y = podil_ano, label = paste0(round(podil_ano * 100),"%"))) +
    geom_bar(stat = "identity") +
    geom_text(aes(color = podil_ano > invert_color_threshold, y = if_else(podil_ano > invert_color_threshold,0.01, podil_ano + 0.01)), hjust = 0, family = "SKAUT") +
    scale_color_manual(values = c("white", dark_blue_color), guide = FALSE) +
    expand_limits(color = c(FALSE, TRUE)) +
    coord_flip() +
    theme(axis.title = element_blank(), axis.text.x = element_blank(),
          axis.ticks.x = element_blank(), axis.line.x = element_blank(),
          plot.title = element_text(hjust = title_hjust)) +
    ggtitle(paste0(title, " (", sum(!is.na(cela_data %>% pull( {{ sloupec }}))) ," odpovědí)"))
}
