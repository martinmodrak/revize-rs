#V palcich
if(document_output) {
  plot_size_multiplier <- 1
} else {
  plot_size_multiplier <- 1
}

default_plot_width <- 12 * plot_size_multiplier
default_plot_height <- (default_plot_width / 16) * 9

revize_colors <- c(
  white = "white",
  orange = "#f47913",
  green = "#3bff6b",
  pink = "#ff0083",
  dark_blue = "#00225e",
  darkest_fill = "#003ca5",
  mid_fill = "#b2c5e6"
)

revize_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (revize_colors)

  ret <- revize_colors[cols]
  names(ret) <- NULL # Je potreba, jinak nelze pouzit ve scale_color_manual
  ret
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

title_family <- function() {
  # if ("pdf_document" %in% rmarkdown::all_output_formats(knitr::current_input())) {
  #   "Arial"
  # } else {
  #   "SKAUT Bold"
  # }
  "SKAUT Bold"
}

vodorovne_popisky_x <- theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5))
svisle_popisky_x <- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3))

theme_revizers <- function() {
  default_margin <- 2 * plot_size_multiplier
  my_margin <- function(t = default_margin, r = default_margin, b = default_margin, l = default_margin) {
    margin(t = t, r = r, b = b, l = l)
  }

  theme_void() +
    theme(
      text = element_text(family = "Roboto", color = "white", size = 4 + 9  * plot_size_multiplier,
                          face = "plain", hjust = 0, vjust = 0.5, angle = 0, lineheight = 1, margin = my_margin(), debug = FALSE),
      line = element_line(color = "white", size = 0.5, linetype = "solid", lineend = "square"),
      rect = element_rect(color = "white", size = 1, linetype = "solid", fill = FALSE),

      plot.background = element_rect(fill = revize_cols("dark_blue"), color = FALSE),
      plot.margin = margin(t = 20 * plot_size_multiplier, r = 20 * plot_size_multiplier, b = 20 * plot_size_multiplier, l = 15 * plot_size_multiplier),
      plot.title = element_text(family = title_family(), size = 35  * plot_size_multiplier, hjust = 0.5),
      plot.subtitle = element_text(family = "Roboto", size = 16 * plot_size_multiplier, face = "bold", hjust = 0.5, margin = my_margin(b = 8 * plot_size_multiplier)),

      panel.grid = element_blank(),
      panel.background = element_blank(),
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
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3, margin = my_margin(t = 8 * plot_size_multiplier), inherit.blank = TRUE),
      axis.text.x.top = NULL,
      axis.text.y = element_text(vjust = 0.3, hjust = 1, inherit.blank = TRUE),
      axis.text.y.right = NULL,
      axis.ticks = NULL,
      axis.ticks.length = unit(2, units = "pt"),
      axis.line = NULL,
      axis.line.x = NULL,
      axis.line.y = NULL,

      strip.background = element_rect(color = revize_cols("mid_fill"), fill = revize_cols("darkest_fill")),
      strip.text = element_text(hjust = 0.5),
      strip.text.x = NULL,
      strip.text.y = NULL,
      strip.placement = "inside"
    )
}

set_theme_revizers <- function() {
  theme_set(theme_revizers())
  windowsFonts("SKAUT Bold" = windowsFont("SKAUT Bold"))
  windowsFonts("Roboto" = windowsFont("Roboto"))

  update_geom_defaults("bar",   list(fill = "white"))
  update_geom_defaults("line", list(size = 2, color = "white"))
  update_geom_defaults("path", list(size = 2, color = "white"))
  update_geom_defaults("linerange", list(size = 2, color = "white"))
  update_geom_defaults("vline", list(color = revize_cols(2), size = 2, linetype = "dashed"))
  update_geom_defaults("hline", list(color = revize_cols(2), size = 2, linetype = "dashed"))
  update_geom_defaults("density", list(size = 2, color = "white"))
  update_geom_defaults("smooth", list(size = 2, color = "white", fill = revize_cols("mid_fill")))
  update_geom_defaults("ribbon", list(fill = revize_cols("mid_fill")))
  update_geom_defaults("point", list(color = "white"))
  update_geom_defaults("text", list(color = "white"))

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

plot_summary_mc <- function(cela_data, sloupec,
                            title = popis_pro_plot(cela_data, {{ sloupec }}), subtitle = NULL,
                            order_by_podil = TRUE, invert_color_threshold = 0.06,
                            max_podil = Inf, min_podil = -Inf, exclude_values = NULL, include_values = NULL) {

  mc_summary <- summarise_multiple_choice(cela_data, {{ sloupec }})
  if(is.null(include_values)) {
    include_values <- mc_summary %>% pull(id_volby)
  }
  if(!is.null(exclude_values)) {
    include_values <- setdiff(include_values, exclude_values)
  }

  data_to_plot <- mc_summary %>%
    filter(podil_ano > min_podil, podil_ano < max_podil, id_volby %in% include_values)

  n_odpovedi <- unique(data_to_plot$pocet_total)
  if(!length(n_odpovedi) == 1) {
    stop("Vice pocet_total")
  }

  wrap_width <- 55
  if(order_by_podil) {
    data_to_plot <- data_to_plot %>%
      mutate(nazev_volby =  fct_reorder(str_wrap(nazev_volby, wrap_width), podil_ano))
  } else {
    labels <- popisky_voleb(cela_data, {{ sloupec }})
    data_to_plot <- data_to_plot %>%
      mutate(nazev_volby = factor(id_volby, levels = labels, labels = str_wrap(names(labels), wrap_width)))
  }

  if(is.null(subtitle)) {
    full_subtitle <- paste0(n_odpovedi ," odpovědí")
  } else {
    full_subtitle <- paste0(subtitle, ", ", n_odpovedi ," odpovědí")
  }

  data_to_plot %>%
    ggplot(aes(x = nazev_volby, y = podil_ano, label = scales::percent(podil_ano, accuracy = 1))) +
    geom_bar(stat = "identity") +
    geom_text(aes(color = podil_ano > invert_color_threshold, y = if_else(podil_ano > invert_color_threshold,0.01, podil_ano + 0.01)), hjust = 0, family = title_family(), size = 6 * plot_size_multiplier) +
    scale_color_manual(values = c("white", revize_cols("dark_blue")), guide = FALSE) +
    expand_limits(color = c(FALSE, TRUE)) +
    coord_flip() +
    theme(axis.title = element_blank(), axis.text.x = element_blank(),
          axis.ticks.x = element_blank(), axis.line.x = element_blank(),
          axis.text.y = element_text(lineheight = 0.8)) +
    plot_annotation(title = title, subtitle =  full_subtitle)
}

plot_binarni_s_nejistotou <- function(data, binarni_sloupce_nazev, by, names_prefix = "", legend_label = if_else(flip, "Skupina", "Měřítko"), na.rm = FALSE, flip = FALSE, facet = FALSE) {
  if(length(binarni_sloupce_nazev) == 1 || facet) {
    if(flip & length(binarni_sloupce_nazev) == 1) {
      stop("Flip dava smysl jen kdyz je více sloupců")
    }
    my_aes <- aes(x = {{by}}, y = podil_ano, ymin = dolni, ymax = horni, group = 1)
    my_color_scale <- NULL
    my_fill_scale <- NULL
    if(facet) {
      if(flip) {
        my_facet <- facet_wrap(~ {{by}})
      } else {
        my_facet <- facet_wrap(~meritko)
      }
    } else {
      my_facet <- NULL
    }
  } else {
    if(flip) {
      my_aes <- aes(x = meritko, y = podil_ano, ymin = dolni, ymax = horni, color = {{by}}, group = {{by}}, fill = {{by}})
    } else {
      my_aes <- aes(x = {{by}}, y = podil_ano, ymin = dolni, ymax = horni, color = meritko, group = meritko, fill = meritko)
    }
    my_color_scale <- scale_color_revize(name = legend_label)
    my_fill_scale <- scale_fill_revize(name = legend_label)
    my_facet <- NULL
  }

  if(names_prefix == "") {
    meritko_levels <- binarni_sloupce_nazev
  } else {
    meritko_levels <- gsub(paste0("^", names_prefix), "", binarni_sloupce_nazev)
  }

  data %>% filter(!is.na({{by}})) %>%
    pivot_longer(all_of(binarni_sloupce_nazev), names_to = "meritko", values_to = "ano", names_prefix = names_prefix) %>%
    #mutate(meritko = factor(meritko, levels = binarni_sloupce_nazev)) %>%
    group_by({{by}}, meritko) %>%
    summarise(podil_ano = mean(ano, na.rm = na.rm), dolni = nejistota_binarni(0.025, ano, na.rm = na.rm), horni = nejistota_binarni(0.975, ano, na.rm = na.rm)) %>%
    mutate(meritko = factor(meritko, levels = meritko_levels)) %>%
    ggplot(my_aes) + geom_ribbon(alpha = 0.5, color = FALSE) + geom_line() + vodorovne_popisky_x +
    my_color_scale  + my_fill_scale + my_facet +
    scale_y_continuous("Podíl", labels = scales::percent)
}

plot_ciselne_s_nejistotou <- function(data, ciselne_sloupce_nazev, by, names_prefix = "", legend_label = "Měřítko") {
  if(length(ciselne_sloupce_nazev) == 1) {
    my_aes <- aes(x = {{by}}, y = prumer, ymin = dolni, ymax = horni, group = 1)
    my_color_scale <- NULL
    my_fill_scale <- NULL
  } else {
    my_aes <- aes(x = {{by}}, y = prumer, ymin = dolni, ymax = horni, color = meritko, group = meritko, fill = meritko)
    my_color_scale <- scale_color_revize(name = legend_label)
    my_fill_scale <- scale_fill_revize(name = legend_label)
  }
  data %>% filter(!is.na({{by}})) %>%
    pivot_longer(all_of(ciselne_sloupce_nazev), names_to = "meritko", values_to = "hodnota", names_prefix = names_prefix) %>%
    #mutate(meritko = factor(meritko, levels = ciselne_sloupce_nazev)) %>%
    mutate(hodnota = as.double(hodnota)) %>%
    group_by({{by}}, meritko) %>%
    summarise(prumer = mean(hodnota), sem = sd(hodnota)/sqrt(length(hodnota)),
              dolni = qnorm(0.025, prumer, sem), horni = qnorm(0.975, prumer, sem)) %>%
    ggplot(my_aes) + geom_ribbon(alpha = 0.5, color = FALSE) + geom_line() + vodorovne_popisky_x +
    my_color_scale  + my_fill_scale +
    scale_y_continuous("Průměr")
}

save_list_of_plots <- function(plot_list, local_data_subdir) {
  plot_dir <- here::here("local_data",local_data_subdir)
  if(!dir.exists(plot_dir)) {
    dir.create(plot_dir, recursive = TRUE)
  }
  for(plot_name in names(plot_list)) {
    # for(format in c(".svg",".wmf",".png")) {
    for(format in c(".png")) {
        ggsave(paste0(plot_dir, "/", plot_name, format), plot_list[[plot_name]], width = default_plot_width, height = default_plot_height)
    }
  }

}

plot_frekvence_by <- function(cela_data, nazev_freknce_sloupce, group) {
  vyplnena_data <- cela_data %>% filter(!is.na({{group}}), !is.na(!!nazev_freknce_sloupce))
  frekvence_matrix <- rozsir_mc_matrix(vyplnena_data, nazev_freknce_sloupce, zachovat_NA = FALSE)
  frekvence_cummulative <- 1 -  t(apply(frekvence_matrix, 1,  cummax)) + frekvence_matrix

  names_prefix <- paste0(nazev_freknce_sloupce, ".")
  frekvence_names <- gsub(names_prefix, "", colnames(frekvence_cummulative), fixed = TRUE)

  frekvence_cummulative %>% cbind(vyplnena_data %>% select({{group}})) %>%
    pivot_longer(cols = colnames(frekvence_cummulative), names_to = "frekvence", names_prefix = names_prefix, values_to = "frekvence_ano") %>%
    mutate(frekvence = factor(frekvence, levels = frekvence_names, labels = frekvence_names)) %>%
    group_by({{group}}, frekvence) %>%
    summarise(prumer_ano = mean(frekvence_ano),
              dolni = nejistota_binarni(0.025, frekvence_ano),
              horni = nejistota_binarni(0.975, frekvence_ano)) %>%
    ggplot(
      aes(x = frekvence, y = prumer_ano, ymin = dolni, ymax = horni,
          fill = {{group}}, color = {{group}}, group = {{group}},
          shape = {{group}})) +
    geom_line() +
    geom_point() +
    geom_ribbon(alpha = 0.5, color = FALSE) +
    scale_color_revize() + scale_fill_revize() +
    scale_x_discrete(nazev_freknce_sloupce) +
    scale_y_continuous("Podíl kmenů s danou nebo vyšší frekvencí", labels = scales::percent)
}

