kompetence <- c("samostatny", "fyzicky_zdatny", "pripraven_krize", "pripraven_bezny_zivot","resit_problemy","taboreni_tym","tvorivost_zrucnost","sebepoznani","duchovni_zivot","slib_zakon","svedomi","rozvoj_osobnosti","vztahy","komunikace","pomaham","rodina","skautsky_zit","clen_tymu","aktivni_obcan","propojenost_sveta","tolerantni","pobyt_v_prirode","vztah_k_prirode_krajine","setrnost")
kategorie_kompetence <- c("zvladam","dulezite","rozvijim", "skauting")

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

preprocess_dat <- function(cela_data, verbose = TRUE) {
  cela_data <-  cela_data %>%
    mutate(zvolena_delka_dotazniku = factor(kolik_casu, levels = c(1,2), labels = c("kratka", "plna")))

  # Odstranit background
  for(sloupec in names(cela_data)) {
    if(grepl("^background", sloupec)) {
      if(verbose) {
        cat("Odstranuji ", sloupec, "\n")
      }
      cela_data <- within(cela_data, rm(list = sloupec))
    }
  }

  # Opravit fuckup: prohozeny popisky "zvladam" a "dulezite"
  for(k in kompetence) {
    zvladam <- cela_data[[paste0(k,"_zvladam")]]
    cela_data[[paste0(k,"_zvladam")]] <- cela_data[[paste0(k,"_dulezite")]]
    cela_data[[paste0(k,"_dulezite")]] <- zvladam

    zvladam_doplnek <- cela_data[[paste0(k,"_zvladam.doplnek")]]
    cela_data[[paste0(k,"_zvladam.doplnek")]] <- cela_data[[paste0(k,"_dulezite.doplnek")]]
    cela_data[[paste0(k,"_dulezite.doplnek")]] <- zvladam_doplnek
  }

  neslucovane_sloupce <- c("created", "modified", "ended","expired", "kompetence_k_zobrazeni")
  kopirovane_sloupce <- c("kolik_casu", "kategorie_respondenta", "bez_zkusenosti_mladsi")
  ruzne_atributy_resolution <- list(co_zazil = "doplnek", sluzba = "puvodni")


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

  #Prejmenovat multiple choice
  for(sloupec in names(manual_codings)) {
    if(verbose) {
      cat("Prejmenovavam hodnoty v ", sloupec, "\n")
    }
    cela_data[[sloupec]] <- replace_coding(cela_data[[sloupec]], manual_codings[[sloupec]])
  }

  #Delka vyplneni
  dates <- cela_data$ended.hlavni %>% ymd_hms()
  dotaznik_start <- ymd_hms("2019-11-23 00:00:00")

  cela_data$pocet_hodin_od_startu <- as.numeric(dates - dotaznik_start)

  #Kraje
  cela_data <- cela_data %>% mutate(
    kraj_nazev = as.factor(kraj))

  # Kategorie respondenta
  cela_data <- cela_data %>% mutate(kategorie_respondenta_full = if_else(kategorie_respondenta != "nikdy_spolecenstvi", kategorie_respondenta,
                                                                         if_else(bez_zkusenosti_mladsi == "ano", "nikdy_spolecenstvi_mladsi", "nikdy_spolecenstvi_starsi")))

  attributes(cela_data$kategorie_respondenta_full)$labels <-
    c(attributes(cela_data$kategorie_respondenta_full)$labels[c(1,2)],
      `Nikdy jsem nebyla součástí roverského společenství (mladší členi)` = "nikdy_spolecenstvi_mladsi",
      `Nikdy jsem nebyla součástí roverského společenství (starší členi)` = "nikdy_spolecenstvi_starsi")

  # Vytvor nove promenne z FA analyzy pro role
  role_fa <- cela_data %>% rozsir_mc(quo(role_skauting)) %>% select(starts_with("role_skauting_"))
  role_fa_res <- psych::fa(role_fa , nfactors = 6, rotate = "varimax")

  cela_data <- vytvor_promenne_dle_fa(cela_data,role_fa,role_fa_res, var_name = "roleFA")
  cela_data <- cela_data %>% rename(roleFA_roverSam=roleFA6,
                       roleFA_technickoOrganizacni = roleFA5,
                       roleFA_tymAkci = roleFA4,
                       roleFA_rover = roleFA3,
                       roleFA_vedouci = roleFA2,
                       roleFA_rover_radce = roleFA1)
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
  psc_reg_cislo <- read_csv2(psc_reg_cislo_pth, col_types = cols(
    reg_c_strediska = col_character(),
    reg_c_spravne = col_character()
  ))

  # nekde tam je bug, nevim kde zatim, ale musmi pracovat
  #cela_data <-
  #  cela_data %>%
  #  left_join(psc_reg_cislo, by =c("reg_c_strediska")) %>%
  #  mutate(reg_c_strediska = if_else(!is.na(reg_c_spravne), reg_c_spravne,reg_c_strediska)) %>%
  #  select(-reg_c_spravne)

  # spocitej lss
  cela_data <- cela_data %>%
    mutate(lss = mc_lss1 + mc_lss2 + mc_lss3 + mc_lss4 + mc_lss5)

  cela_data %>% as_tibble()
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

  data_vyplneno <- cela_data %>% filter(!is.na({{sloupec}}))
  volby_df %>% crossing(data_vyplneno) %>%
    group_by(id_volby, nazev_volby) %>%
    mutate(volba_ano = {{sloupec}} %contains_word% id_volby) %>%
    summarise(pocet_ano = sum(volba_ano), podil_ano = mean(volba_ano), pocet_total = length(volba_ano)) %>%
    ungroup()

}

expand_kompetence <- function(cela_data) {
  cela_data_backup <- cela_data

  kompetence_vse <- data.frame(kompetence) %>%
    crossing(data.frame(kategorie_kompetence)) %>%
    mutate(nazev = paste(kompetence, kategorie_kompetence, sep = "_"),
           nazev_new = paste(kompetence, kategorie_kompetence, sep = "."))

  for(i in 1:nrow(kompetence_vse)) {
    cela_data[[kompetence_vse$nazev_new[i]]] <- as.integer(cela_data[[kompetence_vse$nazev[i]]])
  }
  cela_data <- cela_data %>% dplyr::select(- one_of(kompetence_vse$nazev))

  kompetence_vybrane <- cela_data %>%
    dplyr::select(one_of(kompetence_vse$nazev_new)) %>%
    names()

  nevybrane <- setdiff(kompetence_vse$nazev_new, kompetence_vybrane)
  if(length(nevybrane) > 0) {
    stop(paste0("Nevybrane kompetence: ", paste0(nevybrane, collapse = ", ")))
  }

  neexistujici <- setdiff(kompetence_vybrane, kompetence_vse$nazev_new)
  if(length(nevybrane) > 0) {
    stop(paste0("Neexistujici kompetence: ", paste0(neexistujici, collapse = ", ")))
  }

  expanded <- cela_data %>%
    pivot_longer(cols = one_of(kompetence_vse$nazev_new),
                 names_sep = "\\.",
                 names_to = c("kompetence","kategorie_kompetence"),
                 values_to = "kompetence_odpoved")


  for(i in 1:nrow(kompetence_vse)) {
    expanded_values <- expanded %>%
      filter(kompetence == kompetence_vse$kompetence[[i]], kategorie_kompetence == kompetence_vse$kategorie_kompetence[[i]]) %>%
      pull(kompetence_odpoved)

    if(!identical(as.integer(cela_data_backup[[kompetence_vse$nazev[i]]]), as.integer(expanded_values)) ) {
      stop(paste0("Spatny expand pro ", kompetence_vse$nazev[i]))
    }
  }

  if(nrow(expanded) != nrow(cela_data_backup) * nrow(kompetence_vse)) {
    stop("Spatny pocet radku")
  }

  expanded
}



manual_codings <- list(
  kategorie_respondenta = c("nyni_spolecenstvi", "drive_spolecenstvi", "nikdy_spolecenstvi"),
  bez_zkusenosti_mladsi = c("ano","ne"),
  pocet_clenu_spolecenstvi = c("5_a_mene","6_10","11_20","21_30","31_a_vice"),
  frekvence_kratkych_akci = c("nikdy","rocne","nekolik_rocne","mesicne","nekolik_mesicne","tydne","nekolik_tydne"),
  frekvence_vicedennich_akci= c("nikdy","mene_nez_rocne","rocne","nekolik_rocne","mesicne","nekolik_mesicne"),
  frekvence_velkych_akci= c("nikdy","mene_nez_rocne","rocne","nekolik_rocne"),

  bez_zkusenosti_setkavam_se_s_vrstevniky = c("vubec","rocne","nekolik_rocne","casteji"),
  bez_zkusenosti_seberozvojovy_program = c("vubec","rocne","nekolik_rocne","casteji"),

  co_zazil = c("radce_podradce","radcovsky_kurz","cekatelky","vudcovky","roversky_kurz","jiny_kurz"),
  fungovani_skautskeho_oddilu = c("druzinovy_system", "radci_program_schuzky","radci_vedli_schuzky",
                                  "clenove_tvorili_program", "samostatne_schuzky", "samostatne_vypravy",
                                  "minimalni_dozor", "nebyl_clenem_druziny", "radcove_16let"),
  spolecenstvi_registrace = c("ruzna_strediska","kmen","klub_dospelych","clenove_kmen","clenove_u_oddilu",
                              "clenove_nereg", "nevim", "jine"),
  s_cim_spokojen = c("vztahy","program","cetnost_akci","kontakty","postoj_strediska"),
  s_cim_nespokojen = c("vztahy","program","cetnost_akci","kontakty","postoj_strediska"),
  organizace_spolecenstvi = c("formalni_vudce_zhury", "formalni_vudce_demokraticky", "formalni_rada_zhury",
                              "formalni_rada_demokraticky", "neformalni_tahoun", "neformalni_rada", "vsichni",
                              "nikdo", "neaktivni"),
  vyroky_o_roveringu_zazil = c("pro_jednu_gen", "podpora_strediska", "rovering_na_SR", "vedeni_je_rovering",
                               "koedukovany", "spoluprace_mimo_stredisko", "kurzy_akce"),
  vyroky_o_roveringu_zazil_2 = c("rover_automaticky", "vstupni_ritual", "snadny_prechod", "pri_prechodu_schopny",
                                 "mladsi_starsi", "prechodovy_ritual", "roversky_slib", "jasne_ukonceni",
                                 "jak_dlouho_chci"),
  problemy_roveringu = c("vytizeni_oddily", "odstehovani", "vytizeni_mimo", "nevidi_smysl", "bez_podpory_strediska",
                         "bez_vedeni", "spatne_vztahy", "bez_kvalitniho_programu", "pracovni_ceta",
                         "soupereni_mezi_skupinami", "nevime_jak"),
  bez_zkuesnosti_velke_akce = c("obrok","mixem","roverska_porada", "kurz_tabor", "regionalni_setkani",
                                "institut", "nic"),
  sluzba = c("stredisku", "skauting_mimo", "rodina","lidem", "prirode", "dobrovolnictvi", "aktivni_obcan",
             "sobe", "nic"),
  proc_nebyl_rover = c("nelakalo", "bez_spolecenstvi", "vedeni", "program", "vztahy", "smysl", "jine"),

  vyroky_o_roveringu_stredisko = c("je_spolecenstvi", "pravidelne", "vlastni_akce", "akce_pro_druhe",
                                   "rozviji_se","roverske_heslo", "vedou", "pomocna_sila","bez_ulohy",
                                   "nejsou", "nevim"),

  komunikacni_kanaly_existujici = c("email","facebook","instagram","krizovatka","SI", "vedouci", "diar",
                                    "knihy", "casopis_kmen", "rovernet")
)

manual_codings$problemy_roveringu_stredisko = manual_codings$problemy_roveringu

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
  colnames(result) <- paste0(as_label(var),"_",col_names)

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
  df <- cbind(df, as_tibble(mc_to_matrix(df, var, zachovat_NA = FALSE)))
}

mc_sloupce <- c(quo(role_skauting), quo(co_zazil), quo(fungovani_skautskeho_oddilu),
                quo(spolecenstvi_registrace), quo(s_cim_spokojen), quo(s_cim_nespokojen),
                quo(organizace_spolecenstvi), quo(vyroky_o_roveringu_zazil),
                quo(vyroky_o_roveringu_zazil_2), quo(vychovne_nastroje), quo(problemy_roveringu),
                quo(co_pomaha_roveringu), quo(komunikacni_kanaly_existujici),
                quo(komunikacni_kanaly_hypoteticke), quo(proc_neni_rover),
                quo(bez_zkuesnosti_velke_akce), quo(sluzba), quo(proc_nebyl_rover),
                quo(vyroky_o_roveringu_stredisko), quo(problemy_roveringu_stredisko))


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

zobraz_fa <- function(fa_res, nfac,cutoff = 0.3,str_to_remove) {
  m <- matrix(fa_res$loadings,ncol = nfac)
  row_names_fa <- rownames(fa_res$loadings)
  m[m<cutoff] <- NA

  m <- as_tibble(m, .name_repair = ~paste0("skupina",1:nfac)) %>% mutate(role = str_remove(row_names_fa,str_to_remove)) %>% dplyr::select(role,everything())
  m
}

vytvor_promenne_dle_fa <- function(df,role_fa,fa_res, var_name = "role") {
  for (i in 1:ncol(fa_res$loadings)) {
    xx <- (role_fa %>% as.matrix()) %*% fa_res$loadings[,i] %>% as.numeric()
    df[[paste0(var_name,i)]] <- xx # asi by to slo maticove, ale tohle bylo prmocare
  }
  df
}
