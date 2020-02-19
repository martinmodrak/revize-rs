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


