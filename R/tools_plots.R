#V palcich
default_plot_width <- 12
default_plot_height <- (default_plot_width / 16) * 9

revize_colors <- c(
  white = "white",
  green = "#3bff6b",
  orange = "#f47913",
  pink = "#ff0083",
  dark_blue = "#002b74",
  darkest_fill = "#668cce",
  midd_fill = "#b2c5e6"
)

revize_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (revize_colors)

  revize_colors[cols]
}

revize_palettes <- list(
  `main`  = revize_cols("white", "orange", "pink", "green"),

  `continuous`  = revize_cols("white", "darkest_fill")
)

revize_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- revize_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}

scale_color_revize <- function(discrete = TRUE, reverse = FALSE, ...) {

  if (discrete) {
    pal <- revize_pal(palette = "main", reverse = reverse)
    discrete_scale("colour", "revize_main", palette = pal, ...)
  } else {
    pal <- revize_pal(palette = "continuous", reverse = reverse)
    scale_color_gradientn(colours = pal(256), ...)
  }
}

scale_fill_revize <- function(discrete = TRUE, reverse = FALSE, ...) {

  if (discrete) {
    pal <- revize_pal(palette = "main", reverse = reverse)
    discrete_scale("fill", paste0("revize_main"), palette = pal, ...)
  } else {
    pal <- revize_pal(palette = "continuous", reverse = reverse)
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

theme_revizers <- function() {
  default_margin <- 2
  my_margin <- function(t = default_margin, r = default_margin, b = default_margin, l = default_margin) {
    margin(t = t, r = r, b = b, l = l)
  }

  theme_void() +
    theme(
      text = element_text(family = "Roboto", color = "white", size = 12,
                          face = "plain", hjust = 0, vjust = 0, angle = 0, lineheight = 1, margin = my_margin(), debug = FALSE),
      line = element_line(color = "white", size = 0.5, linetype = "solid", lineend = "square"),
      rect = element_rect(color = "white", size = 1, linetype = "solid", fill = FALSE),

      plot.background = element_rect(fill = revize_cols("dark_blue")),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 15),
      plot.title = element_text(family = "SKAUT", size = 35, hjust = 1),
      plot.subtitle = element_text(family = "Roboto", size = 16, face = "bold", hjust = 1, margin = my_margin(b = 8)),

      panel.grid = element_blank(),
      panel.background = NULL,
      panel.border = element_blank(),
      panel.spacing = unit(5, "pt"),
      panel.spacing.x = NULL,
      panel.spacing.y = NULL,
      panel.grid.minor = NULL,

      legend.position = "right",
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.margin = NULL,
      legend.spacing = unit(3, "pt"),
      legend.spacing.x = NULL,
      legend.spacing.y = NULL,
      legend.key.height = NULL,
      legend.key.width = NULL,
      legend.text.align = 0,
      legend.title.align = 0,
      legend.direction = "vertical",
      legend.justification = "center",
      legend.box.margin = NULL,
      legend.box.background = NULL,
      legend.box.spacing = NULL,


      axis.text = element_text(face = "bold"),
      axis.title = element_text(hjust = 0.5, margin = my_margin(t = 5)),
      axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, inherit.blank = TRUE),
      axis.title.x = NULL,
      axis.title.x.top = NULL,
      axis.title.y.right = NULL,
      axis.text.x = NULL,
      axis.text.x.top = NULL,
      axis.text.y = NULL,
      axis.text.y.right = NULL,
      axis.ticks = NULL,
      axis.ticks.length = unit(2, units = "pt"),
      axis.line = NULL,
      axis.line.x = NULL,
      axis.line.y = NULL,

      strip.background = element_rect(color = "white", fill = revize_cols("darkest_fill")),
      strip.text = element_text(hjust = 0.5),
      strip.text.x = NULL,
      strip.text.y = NULL,
      strip.placement = "inside"
    )
}

set_theme_revizers <- function() {

  theme_set(theme_revizers())
  windowsFonts("SKAUT" = windowsFont("SKAUT Bold"))
  windowsFonts("Roboto" = windowsFont("Roboto"))

  update_geom_defaults("bar",   list(fill = "white"))
  update_geom_defaults("line", list(size = 2))
  update_geom_defaults("point", list(color = "white"))
}



popis_pro_plot <- function(data, sloupec) {
  popisek <- popisek_otazky(data, {{sloupec}})
  if(!is.null(popisek)) {
    gsub("*","", popisek, fixed = TRUE) %>%
      gsub("`[^`]*`", "", .)
  } else {
    # Hack protoze neumim quasiquotation
    names(data %>% select( {{sloupec}}))
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
    labels <- popisky_voleb(cela_data, {{ sloupec }})
    data_to_plot <- data_to_plot %>%
      mutate(nazev_volby = factor(id_volby, levels = labels, labels = str_wrap(names(labels), wrap_width)))
  }
  data_to_plot %>%
    ggplot(aes(x = nazev_volby, y = podil_ano, label = paste0(round(podil_ano * 100),"%"))) +
    geom_bar(stat = "identity") +
    geom_text(aes(color = podil_ano > invert_color_threshold, y = if_else(podil_ano > invert_color_threshold,0.01, podil_ano + 0.01)), hjust = 0, family = "SKAUT") +
    scale_color_manual(values = c("white", revize_cols("darkest_fill")), guide = FALSE) +
    expand_limits(color = c(FALSE, TRUE)) +
    coord_flip() +
    theme(axis.title = element_blank(), axis.text.x = element_blank(),
          axis.ticks.x = element_blank(), axis.line.x = element_blank(),
          plot.title = element_text(hjust = title_hjust)) +
    ggtitle(title, subtitle =  paste0(sum(!is.na(cela_data %>% pull( {{ sloupec }}))) ," odpovědí"))
}
