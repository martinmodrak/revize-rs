source(here::here("setup_dotaznik.R"), encoding = "UTF-8")
source(here::here("datasety_dotaznik.R"), encoding = "UTF-8")

target_dir <- here::here("private_data", "odpovedi_anonymizovane")
if(!dir.exists(target_dir)) {
  dir.create(target_dir)
}

odstran_udaje <- function(x) {
  x %>% filter(souhlas_udaje_anonymizovane == 1) %>%
    select(-session, -starts_with("created"), -starts_with("modified"),
           -starts_with("ended"), -starts_with("expired"),
           -reg_c_strediska, -nazev_strediska, -reg_c_oddilu, -nazev_oddilu,
           -psc_strediska, -email, -otevrena, -reg_c_strediska_orig,
           -reg_c_doplneno_manualne)
}

odstran_udaje_a_uloz <- function(data, nazev) {
  anonymizovane <- odstran_udaje(data)
  write_csv(anonymizovane, paste0(target_dir, "/", nazev, ".csv"))
}


datasety_wide %>% iwalk(~ odstran_udaje_a_uloz(.x, .y))




cache_anonymizovane <- list(
  wide = datasety_wide %>% map(odstran_udaje),
  long = datasety_long %>% map(odstran_udaje),
  zaloha_labels = zaloha_labels
)

saveRDS(cache_anonymizovane, paste0(target_dir, "/preprocessed.rds"))
