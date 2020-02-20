summarise_multiple_choice <- function(cela_data, sloupec) {
  volby_vec <- popisky_voleb(cela_data, {{ sloupec }})
  volby_df <- data.frame(id_volby = volby_vec, nazev_volby = names(volby_vec))

  data_vyplneno <- cela_data %>% filter(!is.na({{sloupec}}))
  volby_df %>% crossing(data_vyplneno) %>%
    group_by(id_volby, nazev_volby) %>%
    mutate(volba_ano = {{sloupec}} %contains_word% id_volby) %>%
    summarise(pocet_ano = sum(volba_ano), podil_ano = mean(volba_ano), pocet_total = length(volba_ano)) %>%
    ungroup()

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

get_default_zaloha_labels <- function() {
  get("zaloha_labels", envir = topenv())
}

popisky_voleb <- function(data, sloupec, zaloha_labels = get_default_zaloha_labels()) {
  nazev_sloupce <- nazev_sloupce <- rlang::as_name(enquo(sloupec))
  labels_attr <- attributes(data[[nazev_sloupce]])$labels
  if(!is.null(labels_attr)) {
    labels_attr
  } else if(!is.null(zaloha_labels[[nazev_sloupce]])) {
    zaloha_labels[[nazev_sloupce]]$labels
  } else {
    NULL
  }
}

popisek_otazky <- function(data, sloupec, zaloha_labels = get_default_zaloha_labels()) {
  nazev_sloupce <- nazev_sloupce <- rlang::as_name(enquo(sloupec))
  label_attr <- attributes(data[[nazev_sloupce]])$label
  if(!is.null(label_attr)) {
    label_attr
  } else if(!is.null(zaloha_labels[[nazev_sloupce]])) {
    zaloha_labels[[nazev_sloupce]]$label
  } else {
    NULL
  }
}
