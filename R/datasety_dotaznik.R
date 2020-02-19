raw_data <- nacti_dotaznik()



datasety_wide <- list()

datasety_wide$pouzitelne <- raw_data %>%
  vyfiltruj_pouzitelne() %>%
  preprocess_dat(verbose = FALSE)
datasety_wide$hlavni <- datasety_wide$pouzitelne %>%
  filter(age >= 15, age <= 26, dokoncil_hlavni)
datasety_wide$pouzitelne_dokoncene <- datasety_wide$pouzitelne %>%
  filter(dokoncil_hlavni)

datasety_long <- list()
datasety_long$pouzitelne <- datasety_wide$pouzitelne %>%
  expand_kompetence() %>%
  filter(!is.na(kompetence_odpoved))
datasety_long$hlavni <- datasety_long$pouzitelne %>%
  filter(age >= 15, age <= 26, dokoncil_hlavni) %>%
  group_by(session, kategorie_kompetence) %>%
  filter(n() >= 5) %>%
  ungroup()

datasety_wide <- datasety_wide %>% purrr::map(rozsir_vsechna_mc)

hlavni_data <- datasety_wide$hlavni
