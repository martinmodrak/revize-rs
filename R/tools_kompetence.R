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
