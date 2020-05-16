nacti_formr_dotaznik <- function(nazev) {
  as.data.frame(jsonlite::fromJSON(txt = here::here("private_data",paste0(nazev, ".json"))))
}

nacti_formr_items <- function(nazev) {
  formr::formr_items(path = here::here("public_data",paste0("items_", nazev, ".json")))
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

  cela_data <- rozcestnik %>%
    inner_join(hlavni, by = c("session" = "session"), suffix = c("",".hlavni")) %>%
    left_join(doplnek, by = c("session" = "session"), suffix = c("",".doplnek")) %>%
    rename(ended.rozcestnik = ended)


  cela_data %>% as_tibble()
}

preprocess_dat <- function(cela_data, verbose = FALSE, vyhodit_otevrene_jine_otazky = TRUE) {
  cela_data %>%
    odstran_zbytecne_sloupce(verbose = verbose) %>%
    oprav_fuckup_kategorie_kompetence() %>%
    sluc_hlavni_a_doplnek(verbose = verbose) %>%
    prejmenuj_spatne_pojmenovane() %>%
    aplikuj_manual_codings(verbose = verbose) %>%
    spocitej_delku_vyplneni() %>%
    spocitej_kategorii_respondenta() %>%
    vytvor_fa_role() %>%
    vycisti_registracni_cisla() %>%
    dopln_rucni_registracni_cisla() %>%
    spocitej_lss() %>%
    vyhod_texty_jine(vyhodit_otevrene_jine_otazky) %>%
    nastav_nepozorne_mc_jako_na(verbose = verbose) %>%
    prejmenuj_sloupce_kompetenci() %>%
    preved_haven_na_factory() %>%
    spocitej_odvozene_kategorie() %>%
    as_tibble()
}

odstran_zbytecne_sloupce <- function(cela_data, verbose = FALSE) {
  # Odstranit background
  for(sloupec in names(cela_data)) {
    if(grepl("^background", sloupec)) {
      if(verbose) {
        cat("Odstranuji ", sloupec, "\n")
      }
      cela_data <- within(cela_data, rm(list = sloupec))
    }
  }

  cela_data
}

# Opravit fuckup: prohozeny popisky "zvladam" a "dulezite"
oprav_fuckup_kategorie_kompetence <- function(cela_data) {
  for(k in kompetence) {
    zvladam <- cela_data[[paste0(k,"_zvladam")]]
    cela_data[[paste0(k,"_zvladam")]] <- cela_data[[paste0(k,"_dulezite")]]
    cela_data[[paste0(k,"_dulezite")]] <- zvladam

    zvladam_doplnek <- cela_data[[paste0(k,"_zvladam.doplnek")]]
    cela_data[[paste0(k,"_zvladam.doplnek")]] <- cela_data[[paste0(k,"_dulezite.doplnek")]]
    cela_data[[paste0(k,"_dulezite.doplnek")]] <- zvladam_doplnek
  }
  cela_data
}

sluc_hlavni_a_doplnek <- function(cela_data, verbose = FALSE) {
  neslucovane_sloupce <- c("created", "modified", "ended","expired", "kompetence_k_zobrazeni")
  kopirovane_sloupce <- c("kolik_casu", "kategorie_respondenta", "bez_zkusenosti_mladsi")
  ruzne_atributy_resolution <- list(co_zazil = "doplnek", sluzba = "puvodni")

  if(!any(cela_data$kolik_casu == 2)) {
    stop("Asi je nize spatny check na delku dotazniku")
  }

  # Sloucit .doplnek a puvodni sloupec
  cela_data_backup <- cela_data
  for(sloupec in names(cela_data)) {
    sloupec_doplnek <- paste0(sloupec, ".doplnek")
    if(sloupec_doplnek %in% names(cela_data)) {
      if(any(!is.na(cela_data[[sloupec_doplnek]]) & cela_data$kolik_casu == 2)) {
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

        # Odignorovat item_order pro check atributu
        attributes_puvodni <- attributes(cela_data[[sloupec]])
        attributes_doplnek <- attributes(cela_data[[sloupec_doplnek]])
        # Odignorovat item_order show_if pro check atributu
        if(!is.null(attributes_puvodni$item)) {
          attributes_doplnek$item$item_order <- attributes_puvodni$item$item_order
          attributes_doplnek$item$showif <- attributes_puvodni$item$showif
        }


        cela_data[[sloupec]] <- if_else(is.na(cela_data[[sloupec]]), cela_data[[sloupec_doplnek]], cela_data[[sloupec]])

        if(!identical(attributes_puvodni, attributes_doplnek)) {
          if(is.null(ruzne_atributy_resolution[[sloupec]])) {
            attributes(cela_data[[sloupec]]) <- attributes_puvodni
            warning(paste0(sloupec, " a ", sloupec_doplnek, " maji ruzne atributy\n"))
          } else if(ruzne_atributy_resolution[[sloupec]] == "puvodni") {
            attributes(cela_data[[sloupec]]) <- attributes_puvodni
          } else if(ruzne_atributy_resolution[[sloupec]] == "doplnek") {
            attributes(cela_data[[sloupec]]) <- attributes_doplnek
          } else {
            stop("Neplatne ruzne_atributy_resolution")
          }
        } else {
          attributes(cela_data[[sloupec]]) <- attributes_puvodni
        }


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

  cela_data
}

prejmenuj_spatne_pojmenovane <- function(cela_data) {
  cela_data %>%
    rename(bez_zkusenosti_velke_akce = bez_zkuesnosti_velke_akce,
           spokojenost_clenstvim_v_rs = spokojenost_s_roverskym_programem)
}

aplikuj_manual_codings <- function(cela_data, verbose = FALSE) {
  for(sloupec in names(manual_codings)) {
    if(verbose) {
      cat("Prejmenovavam hodnoty v ", sloupec, "\n")
    }
    cela_data[[sloupec]] <- replace_coding(cela_data[[sloupec]], manual_codings[[sloupec]])
  }

  cela_data
}

spocitej_delku_vyplneni <- function(cela_data) {
  dates <- cela_data$ended.hlavni %>% ymd_hms()
  dotaznik_start <- ymd_hms("2019-11-23 00:00:00")

  cela_data$pocet_hodin_od_startu <- as.numeric(dates - dotaznik_start)

  cela_data
}

zalohuj_labels <- function(data) {
  zaloha <- list()
  for(sloupec in names(data)) {
    if(inherits(data[[sloupec]], "haven_labelled")) {
      zaloha[[sloupec]] <- list(label = attributes(data[[sloupec]])$label,
                                labels = attributes(data[[sloupec]])$labels)
    }
  }
  zaloha
}

# Vybrane haven prevest na faktory. Faktory nazachovaji metedata,
# ale ta už jsou uložena v záloze
preved_haven_na_factory <- function(cela_data) {
  # TODO vymyslet jak zachovat u faktoru atributy pri dalsich operacich
  for(sloupec in factor_sloupce) {
    if(sloupec %in% ordered_sloupce) {
      ordered_levels <- c(explicit_na_level, attributes(cela_data[[sloupec]])$labels %>% as.character())
      cela_data[[sloupec]] <- cela_data[[sloupec]] %>%
        factor(ordered = TRUE, levels = ordered_levels) %>%
        forcats::fct_explicit_na(explicit_na_level)

      #cela_data[[sloupec]][is.na(cela_data[[sloupec]])] <- explicit_na_level
    } else {
       cela_data[[sloupec]] <- cela_data[[sloupec]] %>% factor() %>%
         forcats::fct_explicit_na(explicit_na_level) %>%
         droplevels()
    }

  }

  cela_data %>%
    mutate(
      kraj = haven::as_factor(kraj),
      kraj_nazev = as.character(kraj) #jeden relikt
    )
}

spocitej_kategorii_respondenta <- function(cela_data) {
  cela_data <- cela_data %>% mutate(kategorie_respondenta_full = if_else(kategorie_respondenta != "nikdy_spolecenstvi", kategorie_respondenta,
                                                                         if_else(bez_zkusenosti_mladsi == "ano", "nikdy_spolecenstvi_mladsi", "nikdy_spolecenstvi_starsi")))
  attributes(cela_data$kategorie_respondenta_full)$labels <-
    c(attributes(cela_data$kategorie_respondenta_full)$labels[c(1,2)],
      `Nikdy jsem nebyla součástí roverského společenství (mladší členi)` = "nikdy_spolecenstvi_mladsi",
      `Nikdy jsem nebyla součástí roverského společenství (starší členi)` = "nikdy_spolecenstvi_starsi")
  cela_data
}

spocitej_odvozene_kategorie <- function(cela_data) {
  test_contains_any_word(cela_data$co_zazil, "roversky_kurz")
  test_contains_any_word(cela_data$co_zazil, "roversky_kurz", "radcovsky_kurz", "cekatelky")
  test_contains_any_word(cela_data$role_skauting, "vedouci_zastupce_oddilu", "oddilovy_radce")
  cela_data <- cela_data %>%
    mutate(byl_na_rs_kurzu = co_zazil %contains_word% "roversky_kurz",
           byl_na_kurzu = co_zazil %contains_any_word%
             c("roversky_kurz", "radcovsky_kurz", "cekatelky", "jiny_kurz", "vudcovky"),
           neni_organizovan =  organizace_spolecenstvi %contains_any_word% c("vsichni", "nikdo", "neaktivni"),
           je_organizovan = organizace_spolecenstvi %contains_any_word%
             c("formalni_vudce_zhury", "formalni_vudce_demokraticky", "formalni_rada_zhury",
               "formalni_rada_demokraticky", "neformalni_tahoun", "neformalni_rada"),
           je_rover = role_skauting %contains_any_word% c("clen_roveru", "tahoun_roveru", "rover_sam", "clen_rady_roveru"),
           ma_roverskou_roli = je_rover | role_skauting %contains_word% "vedouci_roveru",
           ma_vudcovskou_roli = role_skauting %contains_any_word% c("vedouci_zastupce_oddilu",
                                                                    "clen_vedeni_oddilu","oddilovy_radce"),
           dokoncil_hlavni = !is.na(ended.hlavni),
           stav_vyplneni = case_when(is.na(ended.hlavni) ~ "nedokoncil",
                                     kolik_casu == "delsi" ~ "dokoncil_delsi",
                                     jeste_pokracovat == "ne" ~ "dokoncil_kratsi",
                                     is.na(ended.doplnek) ~ "rozpracoval_doplnek",
                                     TRUE ~ "dokoncil_doplnek"
                                     ),
           kmen_aktivni_zaklad =
             frekvence_kratkych_akci >= "mesicne" |
             (frekvence_kratkych_akci >= "nekolik_rocne" & frekvence_vicedennich_akci >= "nekolik_rocne") |
             (frekvence_velkych_akci >= "rocne" & (frekvence_kratkych_akci >= "nekolik_rocne" | frekvence_vicedennich_akci >= "nekolik_rocne")),
           kmen_aktivni_velmi = frekvence_velkych_akci >= "rocne" & frekvence_kratkych_akci >= "mesicne" & frekvence_vicedennich_akci >= "nekolik_rocne"
  )

  cela_data <- cela_data %>% mutate(zdroj = case_when(zdroj == "neuvedeno_redirect" ~  "nezname",
                                                             zdroj == "" ~ "primo formr.org",
                                                             TRUE ~ zdroj))

  cela_data
}

# Vytvor nove promenne z FA analyzy pro role
vytvor_fa_role <- function(cela_data) {
  role_fa <- cela_data %>% rozsir_mc("role_skauting") %>% select(starts_with("role_skauting."))
  role_fa_res <- psych::fa(role_fa , nfactors = 6, rotate = "varimax")

  cela_data <- vytvor_promenne_dle_fa(cela_data,role_fa,role_fa_res, var_name = "roleFA")

  cela_data %>% rename(roleFA_roverSam=roleFA6,
                                    roleFA_technickoOrganizacni = roleFA5,
                                    roleFA_tymAkci = roleFA4,
                                    roleFA_rover = roleFA3,
                                    roleFA_vedouci = roleFA2,
                                    roleFA_rover_radce = roleFA1)
}

vycisti_registracni_cisla <- function(cela_data) {
  # nektera data maji spatne reg.cislo (vyplnili ICO namisto toho), provedeme upravu

  ico_reg_cislo_pth <- here::here("public_data/ico_reg_cislo.csv")
  if(!file.exists(ico_reg_cislo_pth)) {
    stop("Chybí překladní tabulka IČO na reg.číslo")
  }
  ico_reg_cislo <- read_csv(ico_reg_cislo_pth, col_types = cols(
    ic = col_character(),
    ev_c = col_character()
  ))

  cela_data <-
    cela_data %>%
    mutate(reg_c_strediska = str_trim(reg_c_strediska)) %>%
    left_join(ico_reg_cislo, by =c("reg_c_strediska"="ic")) %>%
    mutate(reg_c_strediska = if_else(!is.na(ev_c), ev_c,reg_c_strediska)) %>%
    select(-ev_c)

  # jina data maji  u reg.cisla chybejici tecku. Kontroloval jsem to oproti psc a u techto neexistuje psc, takze muzeme predpokladat, ze jen chybela tecka

  psc_reg_cislo_pth <- here::here("public_data/psc_reg_cislo.csv")
  if(!file.exists(psc_reg_cislo_pth)) {
    stop("Chybí překladní tabulka chybějící tečky na reg.číslo")
  }
  psc_reg_cislo <- read_csv(psc_reg_cislo_pth, col_types = cols(
    reg_c_strediska = col_character(),
    reg_c_spravne = col_character()
  ))

  # nekde tam je bug, nevim kde zatim, ale musmi pracovat
  # Martin: to je asi jedno, máme už ručně dohledané reg. č. kde to šlo

  # cela_data_backup <- cela_data
  # cela_data <- cela_data %>%
  #   left_join(psc_reg_cislo, by =c("reg_c_strediska")) %>%
  #   mutate(reg_c_strediska = if_else(!is.na(reg_c_spravne), reg_c_spravne,reg_c_strediska)) %>%
  #   select(-reg_c_spravne)
  #
  # if(nrow(cela_data_backup) != nrow(cela_data)) {
  #   stop("Spatny join psc")
  # }
  #
  cela_data
}


dopln_rucni_registracni_cisla <- function(cela_data) {
  nrow_before <- nrow(cela_data)
  reg_c_orig <- cela_data$reg_c_strediska
  rucni_reg_cisla <- read_csv(here::here("public_data/rucne_sparovana_strediska.csv"), col_types = cols(.default = col_character()))
  cela_data <- cela_data %>%
    mutate(nazev_oddilu = str_trim(nazev_oddilu), nazev_strediska = str_trim(nazev_strediska)) %>%
    left_join(rucni_reg_cisla %>% distinct(), by = c("reg_c_strediska",	"nazev_strediska", "nazev_oddilu"), na_matches = "na") %>%
    mutate(reg_c_strediska_orig = reg_c_strediska,
          reg_c_strediska = if_else(is.na(reg_c_manualne), reg_c_strediska, reg_c_manualne),
           reg_c_doplneno_manualne = !is.na(reg_c_manualne)) %>%
    select(-reg_c_manualne)

  if(nrow(cela_data) != nrow_before) {
    stop("Spatny join")
  }

  if(sum(reg_c_orig != cela_data$reg_c_strediska, na.rm = TRUE) != nrow(rucni_reg_cisla)) {
    stop("Spatny pocet zmenenych reg.c")
  }

  cela_data
}

spocitej_lss <- function(cela_data) {
  cela_data %>%
    mutate(lss = mc_lss1 + mc_lss2 + mc_lss3 + mc_lss4 + mc_lss5)
}

# hazelo mi to error na tim, ze u vetsiny tech mc otazek je i moznost jine, ktera se pak dubluje s tema otevrenyma otazkama
# existuje tedy separatni skript, ktery preuklada jen otevrene otazky, tady se pak muzou vyhodit
vyhod_texty_jine <- function(cela_data, vyhodit = TRUE) {
  if(vyhodit) {
    cela_data %>% select(-`spolecenstvi_registrace_jine`, -`vychovne_nastroje_jine`, -`co_pomaha_roveringu_jine`,- `komunikacni_kanaly_hypoteticke_jine`, -`proc_neni_rover_jine`)
  } else {
    cela_data
  }
}

# Prejmenuje sloupce kompetenci tak, aby mezi nazvem a kompetenci vzdy byla tecka
# A tudiz to slo snadno dleit
prejmenuj_sloupce_kompetenci <- function(cela_data) {
  for(i in 1:nrow(kompetence_nazvy_sloupcu)) {
    cela_data[[kompetence_nazvy_sloupcu$nazev[i]]] <- as.integer(cela_data[[kompetence_nazvy_sloupcu$nazev_raw[i]]])
    attributes(cela_data[[kompetence_nazvy_sloupcu$nazev[i]]]) <- attributes(cela_data[[kompetence_nazvy_sloupcu$nazev_raw[i]]])
  }
  cela_data %>% dplyr::select(- one_of(kompetence_nazvy_sloupcu$nazev_raw))
}


# Pouzitelne == nejsou zjevne roboti ci nesmysly a dostali se aspon ke kategorii respondenta
#
vyfiltruj_pouzitelne <- function(cela_data) {
  cela_data %>%
    filter(!is.na(cela_data$ended.rozcestnik),
           !is.na(cela_data$kategorie_respondenta),
           !is.na(session),
           age >= 10, age <= 50,  #TODO
           is.na(let_v_kmeni) | let_v_kmeni != 42,
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



replace_coding <- function(x, new_values) {
  if(class(x) != "haven_labelled") {
    stop("Neni labelled")
  }
  all_attributes <- attributes(x)
  old_values <- all_attributes$labels %>% as.character()
  if(!identical(old_values, as.character(1:length(old_values)))) {
    stop("Objekt uz ma neciselne kodovani.")
  }
  if(length(old_values) != length(new_values)) {
    stop("Nespravny pocet hodnot pro ")
  }

  new_labels <- new_values
  names(new_labels) <- names(all_attributes$labels)

  if(is.numeric(x) || is.integer(x)) {
    ret <- haven::labelled(new_values[as.integer(x)], labels = new_labels, label = all_attributes$label)
    for(v in 1:length(new_values)) {
      sedi <- (ret == new_values[v]) == (x == v)
      if(!all(sedi, na.rm = TRUE)) {
        stop(paste0("Nesedi ",v, " == ", new_values[v]))
      }
    }
  } else if(is.character(x) && any(grepl(",", x, fixed = TRUE))) {
    split_val <- as.character(x) %>% str_split(", ")
    ret_val <- split_val %>% purrr::map_chr(
      function(x) {
        if(length(x) == 1 && is.na(x)) {
          NA_character_
        } else if (length(x) == 1 && x[1] == "") {
          ""
        } else {
          new_values[as.integer(x)] %>% paste(collapse = ", ")
        }
      })
    ret <- haven::labelled(ret_val, labels = new_labels, label = all_attributes$label)

    for(v in 1:length(new_values)) {
      sedi <- (ret %contains_word% new_values[v]) == (x %contains_word% v)
      if(!all(sedi, na.rm = TRUE)) {
        priklad <- which(!sedi)[1]
        print(ret[priklad])
        print(x[priklad])
        stop(paste0("Nesedi ",v, " == ", new_values[v]))
      }
    }
    ma_na_string <- grepl("NA", ret, fixed = TRUE, ignore.case = FALSE)
    if(any(ma_na_string)) {
      priklad <- which(ma_na_string)[1]
      print(ret[priklad])
      print(x[priklad])
      stop(paste0("Radek ", priklad, " obsahuje NA moznost."))
    }
  }
  else {
    print(head(x))
    print(unique(x))
    print(is.character(x))
    print(any(grepl(",", x, fixed = TRUE)))
    stop("Neplatny typ")
  }

  sedi_na <- is.na(ret) == is.na(x)
  if(!all(sedi_na)) {
    priklad <- which(!sedi_na)[1]
    print(priklad)
    print(ret[priklad])
    print(x[priklad])
    stop("Nesedi NA")
  }
  ret
}


# umozni rozsekat mc odpovedi ulozene ve stringu do n sloupcu (true/false)
rozsir_mc_matrix <- function(df, nazev_sloupce, zachovat_NA = TRUE) {
  mc_obsahuje <- function(v,polozka) {
    if(zachovat_NA && is.na(v)) {
      return(NA)
    }
    return(any(v %in% polozka))
  }
  col_names <- popisky_voleb_nazev(df, nazev_sloupce)
  polozky <- df[[nazev_sloupce]] %>% str_split(", ")

  result <- matrix(NA, nrow = nrow(df), ncol = length(col_names))
  colnames(result) <- paste0(nazev_sloupce,".",col_names)

  for (i in 1:length(col_names)) {
    obsah_sloupce <- map_lgl(polozky,mc_obsahuje,col_names[i])
    result[, i] <- obsah_sloupce

    # Check
    obsah_contains_word <- df[[nazev_sloupce]] %contains_word% col_names[i]
    if(!all(obsah_sloupce == obsah_contains_word, na.rm = TRUE)) {
      stop(paste0("rozsir_mc:Not equal - ", nazev_sloupce))
    }

    if(zachovat_NA) {
      if(!all(is.na(obsah_sloupce) == is.na(obsah_contains_word))) {
        stop(paste0("rozsir_mc:NA mismatch - ", nazev_sloupce))
      }
    } else {
      if(any(is.na(obsah_sloupce)) || any(is.na(obsah_contains_word) & obsah_sloupce)) {
        stop(paste0("rozsir_mc:NA spatne prelozeno - ", nazev_sloupce))
      }
    }

  }

  result
}

# umozni rozsekat mc odpovedi ulozene ve stringu do n sloupcu (true/false)
rozsir_mc <- function(df, nazev_sloupce, zachovat_NA = TRUE) {
  df <- cbind(df, as_tibble(rozsir_mc_matrix(df, nazev_sloupce, zachovat_NA = zachovat_NA)))
}



rozsir_vsechna_mc <- function(data, zachovat_NA = TRUE) {
  for(sloupec in names(mc_sloupce)) {
    data <- rozsir_mc(data, sloupec, zachovat_NA = zachovat_NA)
  }
  data
}


# Tam, kde pro kazdeho vyplneneho byla moznost k vyberu
# Vezmu prazdne a nastavim jako NA, protoze reposndent nedaval pozor
nastav_nepozorne_mc_jako_na <- function(data, verbose) {
  for(sloupec in names(mc_sloupce)) {
    moznost <- mc_sloupce[[sloupec]]$moznost_pro_kazdeho
    if(!is.null(moznost) && moznost) {
      prazdne = (!is.na(data[[sloupec]]) & data[[sloupec]] == "");
      if(verbose) {
        cat("Ve sloupci '",sloupec,"' je ", sum(prazdne), " prazdnych odpovedi nahrazeno NA\n")
      }
      data[[sloupec]][prazdne] <- NA
    }
  }
  data
}

# Nyni volano jen pro hlavni data, aby to pripadne slo dale analyzovat
nastav_podivne_odpovedi_na <- function(data) {
  data %>% mutate(
    # Kdo byl v kmeni moc brzy je podivny
    let_v_kmeni = if_else(!is.na(let_v_kmeni) & (age - let_v_kmeni >= 14), let_v_kmeni, NA_real_),
    # Kdo vstoupil do Junaka pred 4 narozeninami je podivny
    let_v_junaku = if_else(!is.na(let_v_junaku) & (age - let_v_junaku >= 5), let_v_junaku, NA_real_),
  )
}

psc_na_reg_cislo <- function(x) {
  psc_jako_vektor <- str_split(x,pattern="") %>% unlist()

  c(psc_jako_vektor[1:3],".",psc_jako_vektor[4:5]) %>% paste0(collapse = "")
}
