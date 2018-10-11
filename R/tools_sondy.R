cisti_data <- function(df) {
  df %>% mutate(celkova_spokojenost_s_programem_rs_kmenu = recode(celkova_spokojenost_s_programem_rs_kmenu, `Velmi nespokojen/a`= 1, `Spíše nespokojen/a` = 2, `Spíše spokojen/a` = 3, `Spíše spokojen/a` = 4, `Velmi spokojen/a` = 5, .default = NA_real_),
                      spokojenost_s_frekvenci_akci2 = recode(spokojenost_s_frekvenci_akci, `Jsem spokojen/a`= 1, `Nejsem spokojen/a - přál/a bych si akce častěji` = 0, `Nejsem spokojen/a - přál/a bych si akce méně často` = 0, .default = NA_real_),
                      frekvence_rs_akci_celodenni_akce = recode(frekvence_rs_akci_celodenni_akce, `Nikdy` = 0, `Méně často než 1 za rok` = 1, `Asi jednou za půl roku` = 2, `Asi jednou za čtvrt roku` = 3, `Asi jednou měsíčně` = 4, `Asi jednou za 14 dní` = 5, `Každý týden nebo častěji` = 6),
                      frekvence_rs_akci_schuzky = recode(frekvence_rs_akci_schuzky, `Nikdy` = 0, `Méně často než 1 za rok` = 1, `Asi jednou za půl roku` = 2, `Asi jednou za čtvrt roku` = 3, `Asi jednou měsíčně` = 4, `Asi jednou za 14 dní` = 5, `Každý týden nebo častěji` = 6),
                      frekvence_rs_akci_vicedenni_akce = recode(frekvence_rs_akci_vicedenni_akce, `Nikdy` = 0, `Méně často než 1 za rok` = 1, `Asi jednou za půl roku` = 2, `Asi jednou za čtvrt roku` = 3, `Asi jednou měsíčně` = 4, `Asi jednou za 14 dní` = 5),
                      frekvence_rs_akci_tabory_expedice = recode(frekvence_rs_akci_tabory_expedice, `Nikdy` = 0, `Méně často než 1 za rok` = 1, `Asi jednou za půl roku` = 2, `Asi jednou za čtvrt roku` = 3),
                      vychovne_nastroje_vyzvy = recode(vychovne_nastroje_vyzvy, `Ano` = 1, `Ne` = 0, .default = NA_real_),
                      vychovne_nastroje_projekty = recode(vychovne_nastroje_projekty, `Ano` = 1, `Ne` = 0, .default = NA_real_),
                      vychovne_nastroje_rovernet = recode(vychovne_nastroje_rovernet, `Ano` = 1, `Ne` = 0, .default = NA_real_),
                      vychovne_nastroje_kmen = recode(vychovne_nastroje_kmen, `Ano` = 1, `Ne` = 0, .default = NA_real_),
                      vychovne_nastroje_roversky_zacatek = recode(vychovne_nastroje_roversky_zacatek, `Ano` = 1, `Ne` = 0, .default = NA_real_),
                      vychovne_nastroje_diar = recode(vychovne_nastroje_diar, `Ano` = 1, `Ne` = 0, .default = NA_real_),
                      vychovne_nastroje_vlastni_stezka = recode(vychovne_nastroje_vlastni_stezka, `Ano` = 1, `Ne` = 0, .default = NA_real_),
                      ma_kmen_vychovny_plan = recode(ma_kmen_vychovny_plan, `Ano` = 1, `Ne` = 0, .default = NA_real_),
                      vlastni_zvyklosti_nic = recode(vlastni_zvyklosti_nic, `Ano` = 1, `Ne` = 0, .default = NA_real_),
                      vlastni_zvyklosti_ritual_vstupu = recode(vlastni_zvyklosti_ritual_vstupu, `Ano` = 1, `Ne` = 0, .default = NA_real_),
                      vlastni_zvyklosti_obnova_slibu = recode(vlastni_zvyklosti_obnova_slibu, `Ano` = 1, `Ne` = 0, .default = NA_real_),
                      vlastni_zvyklosti_ritual_odchodu = recode(vlastni_zvyklosti_ritual_odchodu, `Ano` = 1, `Ne` = 0, .default = NA_real_),
                      vlastni_zvyklosti_ritual_predavani_vudcovstvi = recode(vlastni_zvyklosti_ritual_predavani_vudcovstvi, `Ano` = 1, `Ne` = 0, .default = NA_real_),
                      rs_kmen_prvky_vudce = recode(rs_kmen_prvky_vudce, `Ano` = 1, `Ne` = 0, .default = NA_real_),
                      rs_kmen_prvky_rada = recode(rs_kmen_prvky_rada, `Ano` = 1, `Ne` = 0, .default = NA_real_),
                      rs_kmen_prvky_ani_jedno = recode(rs_kmen_prvky_ani_jedno, `Ano` = 1, `Ne` = 0, .default = NA_real_)
  )
  
}

std <- function(x) {
  sd(x)/sqrt(length(x))
}

lower_lim <- function(x) {
  mean(x) + qt(0.05/2,(length(x)-1))*std(x)
}

upper_lim <- function(x) {
  mean(x) - qt(0.05/2,(length(x)-1))*std(x)
}

summarize_group <- function(df, grp_var = NULL) {
  if (is.null(grp_var)) {
    sondy_mean <- df %>%
      summarize_at(.vars = vars(ucast_rs_kmen_procenta_lide:ucast_rs_kmen_procenta_neco_jineho), .funs = mean) %>%
      gather(key = "duvod", value = "prumer", ucast_rs_kmen_procenta_lide:ucast_rs_kmen_procenta_neco_jineho)
    
    sondy_lower <- df %>% 
      summarize_at(.vars = vars(ucast_rs_kmen_procenta_lide:ucast_rs_kmen_procenta_neco_jineho), .funs = lower_lim) %>%
      gather(key = "duvod", value = "lower", ucast_rs_kmen_procenta_lide:ucast_rs_kmen_procenta_neco_jineho)  
    
    sondy_upper <- df %>% 
      summarize_at(.vars = vars(ucast_rs_kmen_procenta_lide:ucast_rs_kmen_procenta_neco_jineho), .funs = upper_lim) %>%
      gather(key = "duvod", value = "upper", ucast_rs_kmen_procenta_lide:ucast_rs_kmen_procenta_neco_jineho)  
    sondy_n <- df %>% 
      summarize(n = n()) 
    sondy_ucast2 <- sondy_mean %>% left_join(sondy_lower) %>% left_join(sondy_upper)
    return(sondy_ucast2)
  } else {
    df$grp <- as.factor(df[[grp_var]])
    sondy_mean <- df %>%
      group_by(grp) %>% 
      summarize_at(.vars = vars(ucast_rs_kmen_procenta_lide:ucast_rs_kmen_procenta_neco_jineho), .funs = mean) %>%
      gather(key = "duvod", value = "prumer", ucast_rs_kmen_procenta_lide:ucast_rs_kmen_procenta_neco_jineho)
    
    sondy_lower <- df %>% 
      group_by(grp) %>% 
      summarize_at(.vars = vars(ucast_rs_kmen_procenta_lide:ucast_rs_kmen_procenta_neco_jineho), .funs = lower_lim) %>%
      gather(key = "duvod", value = "lower", ucast_rs_kmen_procenta_lide:ucast_rs_kmen_procenta_neco_jineho)  
    
    sondy_upper <- df %>% 
      group_by(grp) %>% 
      summarize_at(.vars = vars(ucast_rs_kmen_procenta_lide:ucast_rs_kmen_procenta_neco_jineho), .funs = upper_lim) %>%
      gather(key = "duvod", value = "upper", ucast_rs_kmen_procenta_lide:ucast_rs_kmen_procenta_neco_jineho)  
    sondy_n <- df %>% 
      group_by(grp) %>% 
      summarize(n = n()) 
    sondy_ucast2 <- sondy_mean %>% left_join(sondy_lower, by = c("grp", "duvod")) %>% left_join(sondy_upper, by = c("grp", "duvod")) %>% left_join(sondy_n, by = "grp")
    return(sondy_ucast2)
  }
}

summarize_group_neucast <- function(df, grp_var = NULL) {
  if (is.null(grp_var)) {
    sondy_mean <- df %>%
      summarize_at(.vars = vars(neucast_rs_kmen_procenta_prace_na_stredisku:neucast_rs_kmen_procenta_jine), .funs = mean) %>%
      gather(key = "duvod", value = "prumer", neucast_rs_kmen_procenta_prace_na_stredisku:neucast_rs_kmen_procenta_jine)
    
    sondy_lower <- df %>% 
      summarize_at(.vars = vars(neucast_rs_kmen_procenta_prace_na_stredisku:neucast_rs_kmen_procenta_jine), .funs = lower_lim) %>%
      gather(key = "duvod", value = "lower", neucast_rs_kmen_procenta_prace_na_stredisku:neucast_rs_kmen_procenta_jine)  
    
    sondy_upper <- df %>% 
      summarize_at(.vars = vars(neucast_rs_kmen_procenta_prace_na_stredisku:neucast_rs_kmen_procenta_jine), .funs = upper_lim) %>%
      gather(key = "duvod", value = "upper", neucast_rs_kmen_procenta_prace_na_stredisku:neucast_rs_kmen_procenta_jine)  
    sondy_n <- df %>% 
      summarize(n = n()) 
    sondy_ucast2 <- sondy_mean %>% left_join(sondy_lower) %>% left_join(sondy_upper)
    return(sondy_ucast2)
  } else {
    df$grp <- as.factor(df[[grp_var]])
    sondy_mean <- df %>%
      group_by(grp) %>% 
      summarize_at(.vars = vars(neucast_rs_kmen_procenta_prace_na_stredisku:neucast_rs_kmen_procenta_jine), .funs = mean) %>%
      gather(key = "duvod", value = "prumer",  neucast_rs_kmen_procenta_prace_na_stredisku:neucast_rs_kmen_procenta_jine)
    
    sondy_lower <- df %>% 
      group_by(grp) %>% 
      summarize_at(.vars = vars(neucast_rs_kmen_procenta_prace_na_stredisku:neucast_rs_kmen_procenta_jine), .funs = lower_lim) %>%
      gather(key = "duvod", value = "lower", neucast_rs_kmen_procenta_prace_na_stredisku:neucast_rs_kmen_procenta_jine)  
    
    sondy_upper <- df %>% 
      group_by(grp) %>% 
      summarize_at(.vars = vars(neucast_rs_kmen_procenta_prace_na_stredisku:neucast_rs_kmen_procenta_jine), .funs = upper_lim) %>%
      gather(key = "duvod", value = "upper", neucast_rs_kmen_procenta_prace_na_stredisku:neucast_rs_kmen_procenta_jine)  
    sondy_n <- df %>% 
      group_by(grp) %>% 
      summarize(n = n()) 
    sondy_ucast2 <- sondy_mean %>% left_join(sondy_lower, by = c("grp", "duvod")) %>% left_join(sondy_upper, by = c("grp", "duvod")) %>% left_join(sondy_n, by = "grp")
    return(sondy_ucast2)
  }
}

plot_sondy_ucast <- function(df) {
  df %>%  
    ggplot(aes(x = duvod, y = prumer, col = grp, group = grp)) +
    geom_point() +
    geom_linerange(aes(ymin = lower, ymax = upper)) +
    geom_line() +
    #geom_text(x = 1, y = 60, aes(label = n)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    scale_x_discrete(labels = c("Lide", "Moznost realizace", "Neco jineho", "Odpocinek od oddilu", "Setrvacnost", "Jine", "Registrace cinnosti nepomuze")) +
    ylim(0,100)
  
}

plot_sondy_neucast <- function(df) {
  df %>%  
    ggplot(aes(x = duvod, y = prumer, col = grp, group = grp)) +
    geom_point() +
    geom_linerange(aes(ymin = lower, ymax = upper)) +
    geom_line() +
    #geom_text(x = 1, y = 60, aes(label = n)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    scale_x_discrete(labels = c("Cinovnicka funkce", "Jina parta", "Jine", "Nemam cas", "Nezajimavy program", "Odchod na SS/VS", "Prace na stredisku", "Rodinny zivot", "Skolni povinnosti", "Vztahy ve spolecenstvi")) +
    ylim(0,100)
  
}