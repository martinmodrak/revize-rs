pripojit_csu_data <- function(data, rok = 2017) {

   #=============== Nactu data z CSU a PSC ========================
  okres_kraj <- read_csv(here("public_data","okres_kraj_csu.csv"), col_types = c(.default = col_character())) %>%
    transmute(nazev_okres = TEXT1, lau_okres = CHODNOTA1, nazev_kraj = TEXT2, nuts_kraj = CHODNOTA2)

  psc_obec_posta <- read_csv2(
    here("public_data","psc_okres_posta_utf8.csv"),
    col_types =
      cols(.default = col_character(),
           PSC = col_character(), KODOKRESU = col_integer())
  ) %>%
    transmute(nazev_obec = NAZOBCE, psc = PSC, kod_okres = KODOKRESU, nazev_casti = NAZCOBCE, nazev_posta = NAZPOST, nazev_okres = NAZOKRESU, nazev_okres = if_else(nazev_okres == "Hlavní město Praha", "Praha", nazev_okres))

  psc_all <- psc_obec_posta %>%
    mutate(psc_id = 1:nrow(.)) %>%
    inner_join(okres_kraj, by = c("nazev_okres" = "nazev_okres"))

  if(nrow(psc_all) != nrow(psc_obec_posta)) stop("Nepodarilo se namapovat PSC na okres")

  obce_okresy <- read_csv(here("public_data","obce_okresy_csu.csv"), col_types = cols(
    KODJAZ = col_character(),
    TYPVAZ = col_character(),
    AKRCIS1 = col_character(),
    KODCIS1 = col_integer(),
    CHODNOTA1 = col_integer(),
    TEXT1 = col_character(),
    AKRCIS2 = col_character(),
    KODCIS2 = col_integer(),
    CHODNOTA2 = col_character(),
    TEXT2 = col_character()
  )) %>%
    transmute(lau_okres = CHODNOTA2, lau_obec = CHODNOTA1, nazev_obec = TEXT1)

  if(rok == 2017) {
    obyvatele_obce <- read_csv(here("public_data","obyvatele_obce_csu_2017.csv"), col_types = cols(
      nuts_okres = col_character(),
      lau_obec = col_integer(),
      nazev_obec = col_character(),
      obyvatele = col_integer(),
      muzi = col_integer(),
      zeny = col_integer(),
      avg_vek = col_double(),
      avg_vek_muzi = col_double(),
      avg_vek_zeny = col_double()
    ), progress = FALSE)
  } else {
    #Vyfiltrovat z vetsiho datoveho souboru
    rok_select <- rok
    obyvatele_obce <- read_csv(here("public_data","obyvatele_obce_csu_2000_2017.csv"), col_types = cols(
      idhod = col_integer(),
      hodnota = col_integer(),
      stapro_kod = col_integer(),
      pohlavi_cis = col_integer(),
      pohlavi_kod = col_integer(),
      vuzemi_cis = col_integer(),
      vuzemi_kod = col_integer(),
      rok = col_integer(),
      casref_do = col_date(format = ""),
      pohlavi_txt = col_character(),
      vuzemi_txt = col_character()
    ), progress = FALSE) %>%
      filter(rok == rok_select) %>%
      transmute(lau_obec = vuzemi_kod, nazev_obec = vuzemi_txt,
                hodnota = hodnota,
                skupina = if_else(is.na(pohlavi_txt) | pohlavi_txt == "", "obyvatele",
                              if_else(pohlavi_txt == "muž", "muzi",
                                  if_else(pohlavi_txt == "žena", "zeny",as.character(NA))))
                ) %>%
      spread(skupina, hodnota)
  }

  #Nazev obce + okres identifikuji mesto? Skoro!
  #PSČ + název části obce identifikují obec? Skoro!

  non_identified_obec_okres <- obce_okresy %>% group_by(lau_okres, nazev_obec) %>% summarize(n_obec = length(lau_obec)) %>% filter(n_obec > 1)
  #non_identified_obec_okres

  non_identified_psc_cast <- psc_obec_posta %>% group_by(psc,nazev_casti) %>% summarize(n_obec = length(unique(nazev_obec))) %>% filter(n_obec > 1)
  #non_identified_psc_cast


  #================ Vlastni mapovani =======================

  #Namapovat lau obce na data - pres mesto + psc najdu standartizovany nazev obce, pres PSC dostanu okres, nazev obce + okres mi da obec (krome prikladu vyse, na ty si dam pozor)

  #Utility func - Najde jedno psc ke kazdemu nazvu obce
  find_best_psc <- function(nazev_obec, nazev_posta, psc_id) {
    if(any(nazev_obec == nazev_posta)) {
      as.integer(max(psc_id[nazev_obec == nazev_posta]))
    } else {
      contains = grepl(unique(nazev_obec), nazev_posta, fixed = TRUE)
      if(any(contains)) {
        as.integer(max(psc_id[contains]))
      } else {
        as.integer(max(psc_id))
      }
    }
  }

  data_augmented <- data


  psc_pres_nazev_obec_unique <- psc_all %>% group_by(psc, nazev_obec) %>%
    summarize(psc_id = find_best_psc(nazev_obec, nazev_posta, psc_id))

  rm(find_best_psc)

  psc_pres_nazev_casti_unique <- psc_all %>% group_by(psc, nazev_casti, nazev_obec) %>%
    summarise(psc_id = as.integer(max(psc_id)), n_psc_id = length(psc_id), .groups = "drop")

  if(any(psc_pres_nazev_casti_unique$n_psc_id > 1)) stop("psc + cast + obec neni unikatni")


  psc_id_pres_psc_nazev_casti <- data_augmented %>% select(mesto, psc) %>%
    left_join(psc_pres_nazev_casti_unique, by = c("psc" = "psc","mesto" = "nazev_casti")) %>%
    select(psc_id)


  psc_id_pres_psc_nazev_obce <- data_augmented %>% select(mesto, psc) %>%
    left_join(psc_pres_nazev_obec_unique, by = c("psc" = "psc","mesto" = "nazev_obec")) %>%
    select(psc_id)

  neidentifikovana_data <- data_augmented %>%
    select(mesto,psc) %>%
    semi_join(non_identified_psc_cast, by = c("psc" = "psc", "mesto" = "nazev_casti"))

  if(nrow(neidentifikovana_data) > 0) stop("Nektera mesta spadaji do nejednoznacnych PSC + obec/nazev casti")


  data_augmented = data_augmented %>% mutate(psc_id = if_else(is.na(psc_id_pres_psc_nazev_casti$psc_id),psc_id_pres_psc_nazev_obce$psc_id, psc_id_pres_psc_nazev_casti$psc_id))

  neidentifikovana_data <- data_augmented %>% filter(is.na(psc_id) && (!is.na(psc) || !is.na(mesto))) %>% select(mesto,psc) %>% unique()

  if(nrow(neidentifikovana_data) > 0) stop("Nektera mesta nelze identifikovat dle PSC + obec/nazev casti")

  data_augmented <- data_augmented %>% left_join(psc_all, by = c("psc_id" = "psc_id"), suffix = c("",".posta"))

  neidentifikovana_data <- data_augmented %>%
    select(nazev_obec, lau_okres) %>%
    semi_join(non_identified_obec_okres, by = c("nazev_obec" = "nazev_obec", "lau_okres" = "lau_okres"))

  if(nrow(neidentifikovana_data) > 0) stop("Nektera mesta spadaji do nejednoznacnych nazev obce + orkes")

  data_augmented <- data_augmented %>% left_join(obce_okresy, by = c("lau_okres" = "lau_okres", "nazev_obec" = "nazev_obec"))

  neidentifikovana_data <- data_augmented %>% filter(is.na(lau_obec) && (!is.na(psc) || !is.na(mesto))) %>% select(mesto,psc) %>% unique()

  if(nrow(neidentifikovana_data) > 0) stop("K nekterym mestum nelza najit LAU kod")

  data_augmented <- data_augmented %>%
    inner_join(obyvatele_obce, by = c("lau_obec" = "lau_obec"))

  if(nrow(data_augmented) != nrow(data)) stop("Nekde je problem")

  data_augmented
}
