zacatky <- function(x, tolerovana_prestavka) {
  sorted <- sort(x)
  sorted_diff <- diff(sorted)
  sorted[c(TRUE, sorted_diff > tolerovana_prestavka + 1)]
}

konce <- function(x, tolerovana_prestavka) {
  sorted <- sort(x)
  sorted_diff <- diff(sorted)
  sorted[c(sorted_diff > tolerovana_prestavka + 1, TRUE)]
}

#Rychle testy, jestli funkce funguje
stopifnot(all(konce(c(2008,2005,2006,2007), 1) == 2008))
stopifnot(all(konce(c(2008,2005,2006,2007), 0) == 2008))
stopifnot(all(konce(c(2008,2005), 1) == c(2005,2008)))
stopifnot(all(konce(c(2008,2005), 2) == 2008))
stopifnot(all(konce(c(2008,2006), 1) == 2008))
stopifnot(all(konce(c(2008,2006), 0) == c(2006,2008)))
stopifnot(all(konce(c(2008,2001,2002,2005,2006,2007), 1) == c(2002,2008)))
stopifnot(all(konce(c(2008,2001,2002,2005,2006,2007,2011,2012), 1) == c(2002,2008,2012)))
stopifnot(all(konce(c(2008,2002,2003,2005,2001,2006,2007,2011,2012), 1) == c(2008,2012)))
stopifnot(all(konce(c(2008,2001,2002,2003,2005,2006,2007,2011,2012), 0) == c(2003,2008,2012)))


stopifnot(all(zacatky(c(2008,2006), 1) == 2006))
stopifnot(all(zacatky(c(2008,2001,2002,2005,2006,2007,2011,2012), 1) == c(2001,2005,2011)))
stopifnot(all(zacatky(c(2008,2002,2003,2005,2001,2006,2007,2011,2012), 1) == c(2001,2011)))
stopifnot(all(zacatky(c(2008,2001,2002,2003,2005,2006,2007,2011,2012), 0) == c(2001,2005,2011)))



rozsah_clenstvi_z_registrace <- function(registrace) {
  registrace %>%
    group_by(Person_PseudoID) %>%
    summarise(rok_narozeni = unique(rok_narozeni), Sex = unique(Sex),

              prvni_registrace = min(Year), posledni_registrace = max(Year),
              prvni_konec = min(konce(Year, tolerovana_prestavka = tolerovana_prestavka)),
              konec_do_12_let = any(konce(Year, tolerovana_prestavka = tolerovana_prestavka) - rok_narozeni <= 12),
              konec_13_az_16_let = any(between(konce(Year, tolerovana_prestavka = tolerovana_prestavka) - rok_narozeni, 13, 16)),
              prestavky_celkem_let =  (posledni_registrace - prvni_registrace + 1) - length(unique(Year)),
              pocet_prestavek = sum(diff(sort(Year)) > 1), .groups = "drop"
    )
}

sumar_odchodu <- function(odchazeni, by = "none") {
  if(by == "year") {
    group_var <- quo(Year)
  } else if(by == "delka_clenstvi") {
    odchazeni <- odchazeni %>% mutate(delka_clenstvi_cat = case_when(
      delka_clenstvi >= 4 ~ "4 a více let",
      delka_clenstvi >= 2 ~ "2-3 roky",
      TRUE ~ "1 rok"))
    group_var <- quo(delka_clenstvi_cat)
  } else if(by == "none") {
    group_var <- quo(1)
  } else {
    stop("Neplatné by.")
  }

  odchazeni %>%
    group_by(vek, Sex, !! group_var) %>%
    summarise(n_ukonceno = sum(ukonceno), pocet = length(Person_PseudoID), .groups = "drop") %>%
    ungroup() %>%
    mutate(podil_odchazejicich = n_ukonceno / pocet,
           lower = qbeta(0.025, n_ukonceno + 1, pocet - n_ukonceno + 1),
           upper = qbeta(0.975, n_ukonceno + 1, pocet - n_ukonceno + 1)
    )
}

plot_odchody <- function(odchazeni, by = "none") {

  if(by == "year") {
    facet <- facet_wrap( ~ Year)
  } else if(by == "delka_clenstvi") {
    facet <- facet_wrap( ~ delka_clenstvi_cat, nrow = 1)
  } else if(by == "none") {
    facet <- NULL
  } else {
    stop("Neplatné by.")
  }


  sumar_odchodu(odchazeni, by) %>%
    ggplot(aes(x = vek, y = podil_odchazejicich, ymin = lower, ymax = upper, fill = Sex)) +
    geom_ribbon(alpha = 0.5) +
    geom_vline(xintercept = 15, size = 2, linetype = "dashed", color = "white") +
    geom_line(aes(color = Sex)) +
    scale_x_continuous("Věk") + scale_y_continuous("Podíl odcházejících z ročníku") +
    expand_limits(y=0) +
    vodorovne_popisky_x +
    facet

}


sumar_prichody <- function(odchazeni, by = "none", response = podil_novych) {
  response <- enquo(response)
  if(by == "year") {
    group_var <- quo(Year)
  } else if(by == "none") {
    group_var <- quo(1)
  } else {
    stop("Neplatné by.")
  }

  odchazeni %>%
    group_by(vek, Sex, !! group_var) %>%
    summarise(n_novych = sum(prvni_registrace == Year), pocet = length(Person_PseudoID), .groups = "drop") %>%
    ungroup() %>%
    mutate(podil_novych = n_novych / pocet
    )
}

plot_prichody <- function(odchazeni, by = "none", response = podil_novych) {

  if(by == "year") {
    facet <- facet_wrap( ~ Year)
  } else if(by == "none") {
    facet <- NULL
  } else {
    stop("Neplatné by.")
  }

  response <- enquo(response)

  sumar_prichody(odchazeni, by, !! response) %>%
    ggplot(aes(x = vek, y = !! response)) +
    geom_vline(xintercept = 15, size = 2, linetype = "dashed", color = "white") +
    geom_line(aes(color = Sex)) +
    scale_x_continuous("Věk") +
    expand_limits(y=0) +
    vodorovne_popisky_x +
    facet

}

sumar_pocty <- function(odchazeni, by = "none") {
  if(by == "year") {
    group_var <- quo(Year)
  } else if(by == "none") {
    group_var <- quo(1)
  } else {
    stop("Neplatné by.")
  }

  odchazeni %>%
    group_by(vek, Sex, !! group_var) %>%
    summarise(pocet = length(Person_PseudoID), .groups = "drop") %>%
    ungroup()
}

plot_pocty <- function(odchazeni, by = "none") {
  if(by == "year") {
    facet <- facet_wrap( ~ Year)
  } else if(by == "none") {
    facet <- NULL
  } else {
    stop("Neplatné by.")
  }

  sumar_pocty(odchazeni, by) %>%
    ggplot(aes(x = vek, y = pocet)) +
    geom_vline(xintercept = 15, size = 1, linetype = "dashed", color = "gray") +
    geom_line(aes(color = Sex)) +
    expand_limits(y=0) +
    vodorovne_popisky_x +
    facet

}

skautis_to_tidy <- function(skautis_raw) {
  skautis_raw %>%
    filter(vek_kategorie_interval %in% c("[<6;11)]", "[<11;15)]", "[<15;18)]")) %>%
    select(-vek_kategorie_popis) %>%
    gather("rok_pohlavi","pocet", -vek_kategorie_interval) %>%
    mutate(rok_pohlavi = gsub("rok_", "", rok_pohlavi)) %>%
    filter(grepl("_", rok_pohlavi)) %>%
    separate(rok_pohlavi, into = c("rok","pohlavi"), sep = "_", convert = TRUE) %>%
    mutate(Sex = fct_recode(pohlavi, "male" = "muzi", "female" = "zeny"))

}

registrace_pro_porovnani_se_skautisem <- function(registrace) {
  registrace %>%
    filter(vek >= 6, vek < 18) %>%
      mutate(vek_kategorie_interval = case_when(
        vek >= 6 & vek < 11 ~ "[<6;11)]",
        vek >= 11 & vek < 15 ~ "[<11;15)]",
        vek >= 15 & vek < 18 ~ "[<15;18)]",
        TRUE ~ NA_character_
      )) %>%
      group_by(vek_kategorie_interval, Sex, Year)
}

porovnej_skautis_my <- function(skautis, my) {
  skautis %>%
    inner_join(my, by = c("vek_kategorie_interval" = "vek_kategorie_interval", "Sex" = "Sex", "rok" = "Year"),
               suffix = c(".opendata",".nase")
               ) %>%
    rename(opendata = pocet.opendata, nase = pocet.nase) %>%
    gather("zdroj","pocet", opendata, nase) %>%
    ggplot(aes(x = as.factor(rok), y = pocet, color = zdroj)) +
      geom_point(size = 2,position = position_dodge(width = 0.2)) +
      vodorovne_popisky_x +
      scale_color_revize() +
      facet_grid(vek_kategorie_interval ~ Sex, scales = "free")

}
