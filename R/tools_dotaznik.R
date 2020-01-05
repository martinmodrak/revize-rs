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

  cela_data %>% as_tibble()
}

preprocess_dat <- function(cela_data, verbose = TRUE) {
  cela_data <-  cela_data %>%
    mutate(zvolena_delka_dotazniku = factor(kolik_casu, levels = c(1,2), labels = c("kratka", "plna")))

  # Odstranit background
  for(sloupec in names(cela_data)) {
    if(grepl("^background", sloupec)) {
      #cela_data <- cela_data %>% mutate(select(- !!sloupec))
      if(verbose) {
        cat("Odstranuji ", sloupec, "\n")
      }
      cela_data <- within(cela_data, rm(list = sloupec))
    }
  }


  neslucovane_sloupce <- c("created", "modified", "ended","expired", "kompetence_k_zobrazeni")
  kopirovane_sloupce <- c("kolik_casu", "kategorie_respondenta", "bez_zkusenosti_mladsi")

  # Sloucit .doplnek a puvodni sloupec
  cela_data_backup <- cela_data
  for(sloupec in names(cela_data)) {
    sloupec_doplnek <- paste0(sloupec, ".doplnek")
    if(sloupec_doplnek %in% names(cela_data)) {
      if(any(!is.na(cela_data[[sloupec_doplnek]]) & cela_data$zvolena_delka_dotazniku == "delsi")) {
        stop(paste0("Vyplneno ", sloupec_doplnek, " i když si vybral delší verzi"))
      }

      if(sloupec %in% neslucovane_sloupce) {
        next;
      } else if(sloupec %in% kopirovane_sloupce) {
        if(any(!is.na(cela_data[[sloupec]]) & !is.na(cela_data[[sloupec_doplnek]]) &
               cela_data[[sloupec]] != cela_data[[sloupec_doplnek]])) {
          stop("Kopirovany sloupec ", sloupec, " se neshoduje.")
        }
        if(verbose) {
          cat("Odstranuji kopirovany", sloupec_doplnek, "\n")
        }
        cela_data <- within(cela_data, rm(list = sloupec_doplnek))
      } else {
        if(any(!is.na(cela_data[[sloupec]]) & !is.na(cela_data[[sloupec_doplnek]]))) {
          stop(paste0("Sloupec ", sloupec, " je vyplnen dvakrat."))
        }
        if(verbose) {
          cat("Slucuji", sloupec, "a", sloupec_doplnek, "\n")
        }

        cela_data[[sloupec]] <- if_else(is.na(cela_data[[sloupec]]), cela_data[[sloupec_doplnek]], cela_data[[sloupec]])

        cela_data <- within(cela_data, rm(list = sloupec_doplnek))
      }
    }
  }

  # Separatni kontrola, ze slouceni fungovalo
  n_tests <- 0
  for(sloupec_doplnek in names(cela_data_backup)) {
    if(grepl("\\.doplnek$", sloupec_doplnek)) {
      sloupec <- gsub("\\.doplnek$", "", sloupec_doplnek)
      if(sloupec %in% neslucovane_sloupce || sloupec %in% kopirovane_sloupce) {
        next;
      }
      na_divne <- is.na(cela_data[[sloupec]]) !=
        (is.na(cela_data_backup[[sloupec]])
         & is.na(cela_data_backup[[sloupec_doplnek]]))

      if(verbose) {
        cat("Kontroluji", sloupec,"\n")
      }
      if(any( na_divne)) {
        stop(paste0("Chybná NA struktura pro ", sloupec, " radky ", paste0(which(na_divne), collapse = ", ")))
      }

      if(!all(is.na(cela_data[[sloupec]]) | cela_data[[sloupec]] == cela_data_backup[[sloupec]] |
              cela_data[[sloupec]] == cela_data_backup[[sloupec_doplnek]])) {
        stop(paste0("Divné hodnoty pro ", sloupec))
      }
      n_tests <- n_tests + 1
    }
  }

  if(n_tests < 10) {
    stop("Tesovací kód nefunguje")
  }


  #Delka vyplneni
  dates <- cela_data$ended.hlavni %>% ymd_hms()
  dotaznik_start <- ymd_hms("2019-11-23 00:00:00")

  cela_data$pocet_hodin_od_startu <- as.numeric(dates - dotaznik_start)

  #Kraje
  cela_data <- cela_data %>% mutate(
    kraj_nazev = as.factor(kraj))

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

summarise_multiple_choice <- function(cela_data, sloupec) {
  nazev_sloupce <- rlang::as_name(enquo(sloupec))
  volby_vec <- attributes(cela_data[[nazev_sloupce]])$labels
  volby_df <- data.frame(id_volby = volby_vec, nazev_volby = names(volby_vec))

  volby_df %>% crossing(cela_data %>% filter(!is.na({{sloupec}}))) %>%
    group_by(id_volby, nazev_volby) %>%
    mutate(volba_ano = {{sloupec}} %contains_word% id_volby) %>%
    summarise(pocet_ano = sum(volba_ano), podil_ano = mean(volba_ano))

}
