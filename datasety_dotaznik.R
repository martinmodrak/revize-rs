# Vytvori objekty s daty tak, abychom vsude analyzovali to same

anonymized_file <- here::here("private_data/odpovedi_anonymizovane/preprocessed.rds")

if(exists("raw_data") && exists("datasety_wide") && exists("datasety_long")) {
  warning("Data uz ve workspace existuji, preskakuji nacitani. Pro vynuceni znovunacteni spust\nrm(raw_data)")
} else if(file.exists(here::here("private_data/hlavni_dotaznik.json"))) {
  preprocessed_file <- here::here("local_data", "preprocessed_cache.rds")
  if(file.exists(preprocessed_file)) {
    message(paste0("Nacitam predzpracovana data z ", preprocessed_file, ". Pro prepocitani je potreba soubor odstranit"))
    cache_contents <- readRDS(preprocessed_file)
    raw_data <- cache_contents$raw_data
    datasety_wide <- cache_contents$wide
    datasety_long <- cache_contents$long
  } else {
    raw_data <- nacti_dotaznik()

    # "Wide" data - co radek to respondent, ruzne profiltrovana
    datasety_wide <- list()

    datasety_wide$pouzitelne <- raw_data %>%
      vyfiltruj_pouzitelne() %>%
      preprocess_dat(verbose = FALSE)

    datasety_wide$hlavni <- datasety_wide$pouzitelne %>%
      filter(age >= 15, age <= 26, dokoncil_hlavni) %>%
      nastav_podivne_odpovedi_na()
    datasety_wide$pouzitelne_dokoncene <- datasety_wide$pouzitelne %>%
      filter(dokoncil_hlavni)

    # "Long" data - expandovane kompetence, ruzne profiltrovana
    datasety_long <- list()
    datasety_long$pouzitelne <- datasety_wide$pouzitelne %>%
      expand_kompetence() %>%
      filter(!is.na(kompetence_odpoved)) %>%
      odvozena_meritka_kompetenci()

    nrow_before <- nrow(datasety_long$pouzitelne)
    datasety_long$pouzitelne <- datasety_long$pouzitelne %>%
      inner_join(kompetence_otazky, by = c("kompetence"))

    if(nrow(datasety_long$pouzitelne) != nrow_before) {
      stop("Špatný join")
    }

    datasety_long$hlavni <- datasety_long$pouzitelne %>%
      filter(age >= 15, age <= 26, dokoncil_hlavni) %>%
      group_by(session, kategorie_kompetence) %>%
      filter(n() >= 5) %>%
      ungroup()

    datasety_wide <- datasety_wide %>%
      purrr::map(rozsir_vsechna_mc)

    cache_contents <- list(raw_data = raw_data, wide = datasety_wide, long = datasety_long)
    saveRDS(cache_contents, preprocessed_file)
  }

  hlavni_data <- datasety_wide$hlavni
  hlavni_data_long <- datasety_long$hlavni

  # Vezmu kus a neprevedu na faktory ani nic podobneho a tim zachovam metadata
  zaloha_labels <- raw_data %>% head(50) %>%
    odstran_zbytecne_sloupce(verbose = FALSE) %>%
    oprav_fuckup_kategorie_kompetence() %>%
    sluc_hlavni_a_doplnek(verbose = FALSE) %>%
    prejmenuj_spatne_pojmenovane() %>%
    aplikuj_manual_codings(verbose = FALSE) %>%
    spocitej_kategorii_respondenta() %>%
    zalohuj_labels()
} else if(file.exists(anonymized_file)) {
  warning("Nenalezana uplna data, nacitam anonymizovane, urcene ke zverejneni.")

  cache_contents <- readRDS(anonymized_file)
  datasety_wide <- cache_contents$wide
  datasety_long <- cache_contents$long
  zaloha_labels <- cache_contents$zaloha_labels
  hlavni_data <- datasety_wide$hlavni
  hlavni_data_long <- datasety_long$hlavni
} else {
  stop("Nenalezena data. Anonymizovana verze dat je prilozena k clanku na krizovatce, pripadne si o ne muzete napsat Orlovi (orel@derwen.info).")
}
