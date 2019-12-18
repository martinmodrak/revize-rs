nacti_formr_dotaznik <- function(nazev) {
  as.data.frame(jsonlite::fromJSON((here("private_data",paste0(nazev, ".json")))))
}

nacti_formr_items <- function(nazev) {
  formr::formr_items(path = here("public_data",paste0("items_", nazev, ".json")))
}

nacti_dotaznik <- function() {
  rozcestnik <- nacti_formr_dotaznik("seberozvoj_rozcestnik")
  hlavni <- nacti_formr_dotaznik("hlavni_dotaznik")
  doplnek <- nacti_formr_dotaznik("hlavni_dotaznik_doplnek")

  rozcestnik_items <- nacti_formr_items("seberozvoj_rozcestnik")
  hlavni_items <- nacti_formr_items("hlavni_dotaznik")
  doplnek_items <- nacti_formr_items("hlavni_dotaznik_doplnek")

  rozcestnik <- formr::formr_recognise(item_list = rozcestnik_items, results = rozcestnik)
  hlavni <- formr::formr_recognise(item_list = hlavni_items, results = hlavni)
  doplnek <- formr::formr_recognise(item_list = doplnek_items, results = doplnek)

  rozcestnik <- rozcestnik %>% mutate(zdroj = case_when(zdroj == "neuvedeno_redirect" ~  "nezname",
                                                      zdroj == "" ~ "primo formr.org",
                                                      TRUE ~ zdroj))

  cela_data <- rozcestnik %>%
    inner_join(hlavni, by = c("session" = "session"), suffix = c("",".hlavni")) %>%
    left_join(doplnek, by = c("session" = "session"), suffix = c("",".doplnek")) %>%
    rename(ended.rozcestnik = ended)


  #Delka vyplneni
  dates <- cela_data$ended.hlavni %>% ymd_hms()
  dotaznik_start <- ymd_hms("2019-11-23 00:00:00")

  cela_data$pocet_hodin_od_startu <- as.numeric(dates - dotaznik_start)

  #Kraje
  kraje_dle_id <- hlavni_items$kraj$choices %>% unlist()
  cela_data <- cela_data %>% mutate(kraj = if_else(is.na(kraj), kraj.doplnek, kraj),
                                    kraj_nazev = kraje_dle_id[kraj])


  cela_data
}

# Pouzitelne == nejsou zjevne roboti ci nesmysly a prosli aspon pres prvni stranku
#
vyfiltruj_pouzitelne <- function(cela_data) {
  cela_data %>%
    filter(!is.na(cela_data$ended.rozcestnik),
           !is.na(session),
           age >= 10, age <= 50,  #TODO
           !grepl("XXX", session, fixed = TRUE) #Test sessions

    )
}

check_vysledky <- function(cela_data) {
  session_opakovane <- cela_data %>%
    group_by(session) %>%
    summarise(pocet = n()) %>%
    filter(pocet > 1) %>%
    nrow()

  if(session_opakovane > 0) {
    stop("Nejaka session se opakuje")
  }
}

summarise_multiple_choice <- function(cela_data, items, sloupec) {
  nazev_sloupce <- rlang::as_name(enquo(sloupec))
  volby_df <- items[[nazev_sloupce]]$choices %>% unlist() %>% data.frame(nazev_volby = .) %>% rownames_to_column("id_volby")

  volby_df %>% crossing(cela_data %>% filter(!is.na({{sloupec}}))) %>%
    group_by(id_volby, nazev_volby) %>%
    mutate(volba_ano = {{sloupec}} %contains_word% id_volby) %>%
    summarise(pocet_ano = sum(volba_ano), podil_ano = mean(volba_ano))

}
