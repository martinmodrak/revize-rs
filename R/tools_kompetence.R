expand_kompetence <- function(cela_data) {
  cela_data_backup <- cela_data

  kompetence_vybrane <- cela_data %>%
    dplyr::select(one_of(kompetence_nazvy_sloupcu$nazev)) %>%
    names()

  nevybrane <- setdiff(kompetence_nazvy_sloupcu$nazev, kompetence_vybrane)
  if(length(nevybrane) > 0) {
    stop(paste0("Nevybrane kompetence: ", paste0(nevybrane, collapse = ", ")))
  }

  neexistujici <- setdiff(kompetence_vybrane, kompetence_nazvy_sloupcu$nazev)
  if(length(nevybrane) > 0) {
    stop(paste0("Neexistujici kompetence: ", paste0(neexistujici, collapse = ", ")))
  }

  for(sloupec in kompetence_nazvy_sloupcu$nazev) {
    cela_data[[sloupec]] <- as.integer(cela_data[[sloupec]])
  }

  expanded <- cela_data %>%
    pivot_longer(cols = one_of(kompetence_nazvy_sloupcu$nazev),
                 names_sep = "\\.",
                 names_to = c("kompetence","kategorie_kompetence"),
                 values_to = "kompetence_odpoved")


  for(i in 1:nrow(kompetence_nazvy_sloupcu)) {
    expanded_values <- expanded %>%
      filter(kompetence == kompetence_nazvy_sloupcu$kompetence[[i]], kategorie_kompetence == kompetence_nazvy_sloupcu$kategorie_kompetence[[i]]) %>%
      pull(kompetence_odpoved)

    if(!identical(as.integer(cela_data_backup[[kompetence_nazvy_sloupcu$nazev[i]]]), as.integer(expanded_values)) ) {
      stop(paste0("Spatny expand pro ", kompetence_nazvy_sloupcu$nazev[i]))
    }
  }

  if(nrow(expanded) != nrow(cela_data_backup) * nrow(kompetence_nazvy_sloupcu)) {
    stop("Spatny pocet radku")
  }

  expanded
}


odvozena_meritka_kompetenci <- function(data_long) {
  if(any(is.na(data_long$kompetence_odpoved))) {
    stop("data_long nesmi mit NA v kompetencich")
  }
  data_long %>%
    mutate(kompetence_pozitivni = kompetence_odpoved > 4) %>%
    group_by(kategorie_kompetence, session) %>%
    mutate(kompetence_nad_median = kompetence_odpoved > min(6,median(kompetence_odpoved)),
           kompetence_nejvyse = kompetence_odpoved == max(kompetence_odpoved),
           kompetence_nejvyse_vazene = kompetence_nejvyse / sum(kompetence_odpoved == max(kompetence_odpoved)),
           kompetence_nejnize = kompetence_odpoved == min(kompetence_odpoved),
           kompetence_nejnize_vazene = kompetence_nejnize / sum(kompetence_odpoved == min(kompetence_odpoved)),
           kompetence_relativne_k_sobe = kompetence_odpoved - mean(kompetence_odpoved)
    ) %>%
    group_by(kategorie_kompetence, kompetence) %>%
    mutate(
      kompetence_relativne_k_populaci = kompetence_odpoved - mean(kompetence_odpoved),
      kompetence_nad_prumer_populace = kompetence_relativne_k_populaci > 0
    ) %>%
    ungroup()
}

meritka_kompetence <- list(kompetence_odpoved = list(type = "ordinal", popis = "Odpověď číselně"),
                           kompetence_pozitivni = list(type = "bool", popis = "Je pozitivní"),
                           kompetence_nad_median = list(type = "bool", popis = "Nad medián respondenta"),
                           kompetence_nejvyse = list(type = "bool", popis = "Nejvýše pro respondenta"),
                           kompetence_nejnize = list(type = "bool", popis = "Nejníže pro respondenta"),
                           kompetence_relativne_k_sobe = list(type = "interval", popis = "Číselně relativně ke svému průměru"),
                           kompetence_relativne_k_populaci = list(type = "interval", popis = "Číselně relativně k průměru populace"),
                           kompetence_nad_prumer_populace = list(type = "bool", popis = "Nad průměr populace")
)

popis_meritka <- function(meritko_nazev) {
  gsub(pattern = "kompetence_", "", meritko_nazev, fixed = TRUE)
}

nejistota_binarni <- function(prob, hodnoty, na.rm = FALSE) {
  qbeta(prob, sum(hodnoty, na.rm = na.rm) + 1, sum(!hodnoty, na.rm = na.rm) + 1)
}

nejistota_meritka <- function(prob, meritko_nazev, hodnoty) {
  type <- meritka_kompetence[[meritko_nazev]]$type
  if(type == "bool") {
    nejistota_binarni(prob, hodnoty)
  } else if(type == "ordinal" || type == "interval") {
    sem <- sd(hodnoty)/sqrt(length(hodnoty))
    qnorm(prob, mean(hodnoty), sem)
  }
}

plot_kompetence_by <- function(data, kategorie, group, group2 = NULL, meritko = kompetence_odpoved, all_together = FALSE) {
  group_col <- data %>% pull({{group}})
  if(typeof(group_col) == "double") {
    my_scale_x <-  NULL
    my_theme <- vodorovne_popisky_x
  } else {
    my_scale_x <- scale_x_discrete()
    my_theme <- NULL
    data <- data %>% mutate({{group}} := fct_reorder(factor({{group}}), {{meritko}}, .fun = mean))
  }

  if(all_together) {
    my_facet <- NULL
    #if(is.null(group2) == 1) {
    kompetence_group <-  quo(1)
  } else {
    my_facet <- facet_wrap(~ popis_pro_grafy)
    kompetence_group <- quo(popis_pro_grafy)
  }


  meritko_nazev <- names(data %>% select({{meritko}})) #Blby hack, protoze neumim tidy a jsem liny se to ucit
  data %>%
    filter(kategorie_kompetence %in% kategorie) %>%
    group_by(!!kompetence_group, {{ group }}, {{ group2 }}) %>%
    summarise(prumer = mean({{meritko}}, na.rm = TRUE),
              dolni = nejistota_meritka(0.025, meritko_nazev, {{meritko}}),
              horni = nejistota_meritka(0.975, meritko_nazev, {{meritko}}), .groups = "drop"
    ) %>%
    ungroup() %>%
    ggplot(aes(x = {{group}}, y = prumer, ymin = dolni, ymax = horni,
               color = {{group2}}, fill = {{group2}}, group = {{group2}})) +
    geom_ribbon(alpha = 0.4, color = FALSE) + geom_line(alpha = 0.8) + my_facet +
    my_scale_x + my_theme + scale_y_continuous("Průměr") +
    scale_color_revize() + scale_fill_revize() +
    ggtitle(kategorie, subtitle =  meritka_kompetence[[meritko_nazev]]$popis)
}

plot_kompetence_by_smooth <- function(data, kategorie, group, group2 = NULL, meritko = kompetence_odpoved, all_together = FALSE) {
  group_col <- data %>% pull({{group}})
  if(typeof(group_col) == "double") {
    my_scale_x <-  NULL
    my_theme <- vodorovne_popisky_x
  } else {
    my_scale_x <- scale_x_discrete()
    my_theme <- NULL
    data <- data %>% mutate({{group}} := fct_reorder(factor({{group}}), {{meritko}}, .fun = mean))
  }

  if(all_together) {
    my_facet <- NULL
  } else {
    my_facet <- facet_wrap(~ popis_pro_grafy)
  }

  meritko_nazev <- names(data %>% select({{meritko}})) #Blby hack, protoze neumim tidy a jsem liny se to ucit
  data %>%
    filter(kategorie_kompetence == kategorie) %>%
    ggplot(aes(x = {{group}}, y = {{ meritko }}, group = {{group2}}, color = {{group2}}, fill = {{group2}})) +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), alpha = 0.5) + my_facet +
    my_scale_x + my_theme + scale_y_continuous("Průměr") +
    scale_color_revize() + scale_fill_revize() +
    ggtitle(kategorie, subtitle =  paste0(meritka_kompetence[[meritko_nazev]]$popis, ", vyhlazeno"))
}

