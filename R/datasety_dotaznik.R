# Vytvori objekty s daty tak, abychom vsude analyzovali to same

raw_data <- nacti_dotaznik()

# "Wide" data - co radek to respondent, ruzne profiltrovana
datasety_wide <- list()

preprocesovana_data <- raw_data %>%
  vyfiltruj_pouzitelne() %>%
  preprocess_dat(verbose = FALSE)

zaloha_labels <- zalohuj_labels(preprocesovana_data)

datasety_wide$pouzitelne <- preprocesovana_data %>% preved_haven_na_factory()

rm(preprocesovana_data)

datasety_wide$hlavni <- datasety_wide$pouzitelne %>%
  filter(age >= 15, age <= 26, dokoncil_hlavni)
datasety_wide$pouzitelne_dokoncene <- datasety_wide$pouzitelne %>%
  filter(dokoncil_hlavni)

# "Long" data - expandovane kompetence, ruzne profiltrovana
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
