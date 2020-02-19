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

  rozcestnik <- rozcestnik %>% mutate(zdroj = case_when(zdroj == "neuvedeno_redirect" ~  "nezname",
                                                      zdroj == "" ~ "primo formr.org",
                                                      TRUE ~ zdroj))

  cela_data <- rozcestnik %>%
    inner_join(hlavni, by = c("session" = "session"), suffix = c("",".hlavni")) %>%
    left_join(doplnek, by = c("session" = "session"), suffix = c("",".doplnek")) %>%
    rename(ended.rozcestnik = ended)


  cela_data %>% as_tibble()
}

preprocess_dat <- function(cela_data, verbose = FALSE, vyhodit_otevrene_jine_otazky = T) {
  cela_data %>%
    odstran_zbytecne_sloupce(verbose = verbose) %>%
    oprav_fuckup_kategorie_kompetence() %>%
    sluc_hlavni_a_doplnek(verbose = verbose) %>%
    prejmenuj_spatne_pojmenovane() %>%
    aplikuj_manual_codings(verbose = verbose) %>%
    spocitej_delku_vyplneni() %>%
    spocitej_odvozene_kategorie() %>%
    vytvor_fa_role() %>%
    vycisti_registracni_cisla() %>%
    spocitej_lss() %>%
    vyhod_texty_jine(vyhodit_otevrene_jine_otazky) %>%
    prejmenuj_sloupce_kompetenci() %>%
    preved_haven_na_factory() %>%
    droplevels() %>%
    as_tibble()

  #TODO u otazek, kde je moznost nic vyfiltrovat (zamenit za NA ?) ty, kdo nezaskrtlni zadnou moznost, ani "nic"
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
           spokojenst_clenstvim_v_rs = spokojenost_s_roverskym_programem)
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

# Vybrane haven prevest na faktory
preved_haven_na_factory <- function(cela_data) {
  for(sloupec in factor_sloupce) {
    old_attributes <- attributes(cela_data[[sloupec]])
    cela_data[[sloupec]] <- cela_data[[sloupec]] %>%
      factor() %>%
      forcats::fct_explicit_na(explicit_na_level) %>%
      droplevels()

    #Zachovat info o otázce pro případné kontroly
    attributes(cela_data[[sloupec]])$label <- old_attributes$label
    attributes(cela_data[[sloupec]])$labels <- old_attributes$labels
  }

  cela_data %>%
    mutate(
      kraj_nazev = as.character(kraj) #jeden relikt
    )
}

spocitej_odvozene_kategorie <- function(cela_data) {
  # Kategorie respondenta
  cela_data <- cela_data %>% mutate(kategorie_respondenta_full = if_else(kategorie_respondenta != "nikdy_spolecenstvi", kategorie_respondenta,
                                                                         if_else(bez_zkusenosti_mladsi == "ano", "nikdy_spolecenstvi_mladsi", "nikdy_spolecenstvi_starsi")))
  attributes(cela_data$kategorie_respondenta_full)$labels <-
    c(attributes(cela_data$kategorie_respondenta_full)$labels[c(1,2)],
      `Nikdy jsem nebyla součástí roverského společenství (mladší členi)` = "nikdy_spolecenstvi_mladsi",
      `Nikdy jsem nebyla součástí roverského společenství (starší členi)` = "nikdy_spolecenstvi_starsi")

  cela_data <- cela_data %>%
    mutate(byl_na_rs_kurzu = co_zazil %contains_word% "roversky_kurz",
           byl_na_kurzu = co_zazil %contains_word% "roversky_kurz"
                       | co_zazil %contains_word% "radcovsky_kurz"
                       | co_zazil %contains_word% "cekatelky"
                       | co_zazil %contains_word% "jiny_kurz"
                       | co_zazil %contains_word% "vudcovky",
           dokoncil_hlavni = !is.na(ended.hlavni),
           druh_vyplneni = case_when(is.na(ended.hlavni) ~ "nedokoncil",
                                     kolik_casu == "delsi" ~ "dokoncil_delsi",
                                     jeste_pokracovat == "ne" ~ "dokoncil_kratsi",
                                     is.na(ended.doplnek) ~ "rozpracoval_doplnek",
                                     TRUE ~ "dokoncil_doplnek"
                                     )
  )


  cela_data
}

# Vytvor nove promenne z FA analyzy pro role
vytvor_fa_role <- function(cela_data) {
  role_fa <- cela_data %>% rozsir_mc(quo(role_skauting)) %>% select(starts_with("role_skauting."))
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
  }
  cela_data %>% dplyr::select(- one_of(kompetence_nazvy_sloupcu$nazev_raw))
}


# Pouzitelne == nejsou zjevne roboti ci nesmysly a prosli aspon pres prvni stranku
#
vyfiltruj_pouzitelne <- function(cela_data) {
  cela_data %>%
    filter(!is.na(cela_data$ended.rozcestnik),
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
rozsir_mc_matrix <- function(df, var, zachovat_NA = FALSE) {
  mc_obsahuje <- function(v,polozka) {
    if(zachovat_NA && is.na(v)) {
      return(NA)
    }
    return(any(v %in% polozka))
  }
  all_attributes <- df[[as_label(var)]] %>% attributes()
  col_names <- all_attributes$labels %>% as.character()
  polozky <- df[[as_label(var)]] %>% str_split(", ")

  result <- matrix(NA, nrow = nrow(df), ncol = length(col_names))
  colnames(result) <- paste0(as_label(var),".",col_names)

  for (i in 1:length(col_names)) {
    obsah_sloupce <- map_lgl(polozky,mc_obsahuje,col_names[i])
    result[, i] <- obsah_sloupce

    # Check
    obsah_contains_word <- df[[as_label(var)]] %contains_word% col_names[i]
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
rozsir_mc <- function(df, var, zachovat_NA = FALSE) {
  df <- cbind(df, as_tibble(rozsir_mc_matrix(df, var, zachovat_NA = FALSE)))
}



rozsir_vsechna_mc <- function(data, zachovat_NA = FALSE) {
  for(sloupec in mc_sloupce) {
    data <- rozsir_mc(data, sloupec, zachovat_NA = zachovat_NA)
  }
  data
}


psc_na_reg_cislo <- function(x) {
  psc_jako_vektor <- str_split(x,pattern="") %>% unlist()

  c(psc_jako_vektor[1:3],".",psc_jako_vektor[4:5]) %>% paste0(collapse = "")
}
