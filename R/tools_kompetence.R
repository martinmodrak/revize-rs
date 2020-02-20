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


odvozena_meritka_kompetenci <- function(expanded_no_NA) {
  if(any(is.na(expanded_no_NA$kompetence_odpoved))) {
    stop("expanded_no_NA nesmi mit NA v kompetencich")
  }
  expanded_no_NA %>%
    group_by(kategorie_kompetence, session) %>%
    mutate(kompetence_nad_median = kompetence_odpoved > min(6,median(kompetence_odpoved)),
           kompetence_nejvyse = kompetence_odpoved == max(kompetence_odpoved),
           kompetence_relativne_k_sobe = kompetence_odpoved - mean(kompetence_odpoved)
    ) %>%
    group_by(kategorie_kompetence, kompetence) %>%
    mutate(
      kompetence_relativne_k_populaci = kompetence_odpoved - mean(kompetence_odpoved),
      kompetence_nad_prumer_populace = kompetence_relativne_k_populaci > 0
    ) %>%
    ungroup()
}

meritka_kompetence <- list(kompetence_odpoved = list(type = "ordinal"),
                           kompetence_nad_median = list(type = "bool"),
                           kompetence_nejvyse = list(type = "bool"),
                           kompetence_relativne_k_sobe = list(type = "interval"),
                           kompetence_relativne_k_populaci = list(type = "interval"),
                           kompetence_nad_prumer_populace = list(type = "bool")
)
