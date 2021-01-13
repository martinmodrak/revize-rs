source(here::here("setup_dotaznik.R"), encoding = "UTF-8")

source(here::here("datasety_dotaznik.R"), encoding = "UTF-8")

library(formula.tools)
library(INLA) #Install via install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)


base_for_inla <- get_base_for_inla(hlavni_data_long)

meritka_k_testovani <- c("kompetence_odpoved", "kompetence_relativne_k_sobe")
#meritka_k_testovani <- c("kompetence_odpoved")



zakladni_adjustment <- list()
zakladni_adjustment$zaklad <- ~ 1 + f(age_ar, model = "rw2", hyper = list(theta = list(prior = log_sqrt_inv_hn(1)))) + sex
zakladni_adjustment$hodne <- update.formula(zakladni_adjustment$zaklad, . ~ . +
                                              f(kraj, model = "iid", hyper = list(theta = list(prior = log_sqrt_inv_hn(1)))) +
                                              f(zivotni_faze, model = "iid", hyper = list(theta = list(prior = log_sqrt_inv_hn(1)))) +
                                              f(kolik_casu, model = "iid", hyper = list(theta = list(prior = log_sqrt_inv_hn(1)))) +
                                              f(kategorie_respondenta_full, model = "iid", hyper = list(theta = list(prior = log_sqrt_inv_hn(1)))))

mc_sloupce_k_uziti <- list()
dalsi_sloupce <- list()

mc_sloupce_k_uziti$minimal <- c()
dalsi_sloupce$minimal <- ~ . + byl_na_jinem_nez_rs_kurzu + byl_na_rs_kurzu +
  f(organizace_strucne, model = "iid", hyper = list(theta = list(prior = log_sqrt_inv_hn(1))))

mc_sloupce_k_uziti$zaklad <- c("role_skauting", "co_zazil", "fungovani_skautskeho_oddilu", "organizace_spolecenstvi")
dalsi_sloupce$zaklad <- ~ .

mc_sloupce_k_uziti$hodne <- c(mc_sloupce_k_uziti$zaklad, "vyroky_o_roveringu_zazil", "vyroky_o_roveringu_zazil_2", "vychovne_nastroje", "problemy_roveringu")
dalsi_sloupce$hodne <- ~ . + f(pocet_clenu_spolecenstvi, model = "rw1", hyper = list(theta = list(prior = log_sqrt_inv_hn(1)))) +
  f(pocet_clenu_strediska, model = "rw1", hyper = list(theta = list(prior = log_sqrt_inv_hn(1))))







all_marginals_list <- list()
for(kategorie in kategorie_kompetence) {
  # for(zakladni in c("zaklad", "hodne") ) {
  #   for(dalsi in c("minimal", "zaklad", "hodne")) {
  for(zakladni in c("zaklad") ) {
    for(dalsi in c("minimal", "zaklad")) {
      for(meritko in meritka_k_testovani) {

        formula <- update.formula(zakladni_adjustment[[zakladni]],
                                  as.formula(paste0(meritko, " ", as.character(dalsi_sloupce[[dalsi]]))))
        cat("Fitting ", kategorie, " - ", meritko, ", ", zakladni, " adjustment a ", dalsi , " dalsi\n")
        print(formula)
        n_fits_parallel <- min(parallel::detectCores() - 1, 4)
        n_cores_per_fit <- max(floor((parallel::detectCores() - 1) / n_fits_parallel), 1)
        results <- inla_pipeline(base_for_inla, kategorie, formula, mc_sloupce_k_uziti[[dalsi]], n_fits_parallel = n_fits_parallel,
                                 n_cores_per_fit = n_cores_per_fit)
        for(kompetence in names(results$processed_fits)) {
          processed_fit <- results$processed_fits[[kompetence]]
          if(inherits(processed_fit, "error")) {
            cat("Error in ", kompetence, "\n")
            print(processed_fit)
          } else {
            all_marginals_list[[length(all_marginals_list) + 1]] <- processed_fit$marginals_summary %>%
              mutate(kategorie = kategorie, kompetence = kompetence, zakladni = zakladni, dalsi = dalsi, meritko = meritko)
          }
        }
      }
    }
  }
}

all_marginals <- do.call(rbind, all_marginals_list)
saveRDS(all_marginals,file = here::here("local_data", "all_marginals.rds"))
