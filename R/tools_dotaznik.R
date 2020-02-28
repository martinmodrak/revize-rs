`%contains_any_word%` <- function(x, words) {
  partial_results <- matrix(nrow = length(x), ncol = length(words))
  for(i in 1:length(words)) {
    partial_results[,i] <- x %contains_word% words[i]
  }
  rowSums(partial_results) > 0
}

test_contains_any_word <- function(x, word1, word2 = FALSE, word3 = FALSE) {
  if(identical(word3, FALSE)) {
    if(identical(word3, FALSE)) {
      words_all <- word1
      correct_val <- x %contains_word% word1
    } else {
      words_all <- c(word1, word2)
      correct_val <- x %contains_word% word1 | x %contains_word% word2
    }
  } else {
    words_all <- c(word1, word2, word3)
    correct_val <- x %contains_word% word1 | x %contains_word% word2 | x %contains_word% word3
  }

  if(!identical(x %contains_any_word% words_all, correct_val)) {
    stop("contains_any_word is broken")
  }
}

summarise_multiple_choice <- function(cela_data, sloupec) {
  volby_vec <- popisky_voleb(cela_data, {{ sloupec }})
  volby_df <- data.frame(id_volby = volby_vec, nazev_volby = names(volby_vec))

  data_vyplneno <- cela_data %>% filter(!is.na({{sloupec}}), {{sloupec}} != explicit_na_level)
  ret <- volby_df %>% crossing(data_vyplneno) %>%
    group_by(id_volby, nazev_volby) %>%
    mutate(volba_ano = {{sloupec}} %contains_word% id_volby) %>%
    summarise(pocet_ano = sum(volba_ano), podil_ano = mean(volba_ano), pocet_total = length(volba_ano)) %>%
    ungroup()

  nazev_sloupce <- rlang::as_name(enquo(sloupec))
  if(!is.null(mc_sloupce[[nazev_sloupce]])) {
    moznost_pro_kazdeho <- mc_sloupce[[nazev_sloupce]]$moznost_pro_kazdeho
    if(is.null(moznost_pro_kazdeho) || !moznost_pro_kazdeho) {
      obsah = data_vyplneno %>% pull({{sloupec}})
      ret <- rbind(ret, data.frame(
          id_volby = "__nic", nazev_volby = "(nic nevybráno)",
          pocet_ano = sum(obsah == ""), podil_ano = mean(obsah == ""), pocet_total = length(obsah)))
    }
  }
  ret
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
