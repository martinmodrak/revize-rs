cisti_data <- function(df) {
  df %>% mutate(celkova_spokojenost_s_programem_rs_kmenu = recode(celkova_spokojenost_s_programem_rs_kmenu, `Velmi nespokojen/a`= 1, `Spíše nespokojen/a` = 2, `Spíše spokojen/a` = 3, `Spíše spokojen/a` = 4, `Velmi spokojen/a` = 5, .default = NA_real_),
                      spokojenost_s_frekvenci_akci2 = recode(spokojenost_s_frekvenci_akci, `Jsem spokojen/a`= 1, `Nejsem spokojen/a - přál/a bych si akce častěji` = 0, `Nejsem spokojen/a - přál/a bych si akce méně často` = 0, .default = NA_real_),
                      frekvence_rs_akci_celodenni_akce = recode(frekvence_rs_akci_celodenni_akce, `Nikdy` = 0, `Méně často než 1 za rok` = 1, `Asi jednou za půl roku` = 2, `Asi jednou za čtvrt roku` = 3, `Asi jednou měsíčně` = 4, `Asi jednou za 14 dní` = 5, `Každý týden nebo častěji` = 6, .default = NA_real_),
                      frekvence_rs_akci_schuzky = recode(frekvence_rs_akci_schuzky, `Nikdy` = 0, `Méně často než 1 za rok` = 1, `Asi jednou za půl roku` = 2, `Asi jednou za čtvrt roku` = 3, `Asi jednou měsíčně` = 4, `Asi jednou za 14 dní` = 5, `Každý týden nebo častěji` = 6, .default = NA_real_),
                      frekvence_rs_akci_vicedenni_akce = recode(frekvence_rs_akci_vicedenni_akce, `Nikdy` = 0, `Méně často než 1 za rok` = 1, `Asi jednou za půl roku` = 2, `Asi jednou za čtvrt roku` = 3, `Asi jednou měsíčně` = 4, `Asi jednou za 14 dní` = 5, .default = NA_real_),
                      frekvence_rs_akci_tabory_expedice = recode(frekvence_rs_akci_tabory_expedice, `Nikdy` = 0, `Méně často než 1 za rok` = 1, `Asi jednou za půl roku` = 2, `Asi jednou za čtvrt roku` = 3, .default = NA_real_),
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

format_corrtable <- function(c) {
  m <- matrix("", nrow = nrow(c$r), ncol = ncol(c$r))
  for (i in 1:nrow(m)) {
    for (j in 1:ncol(m)) {
      m[i,j] <- sprintf("%.2f (%d)", c$r[i,j], c$n[i,j])
    }
  }
  colnames(m) <- colnames(c$r)
  rownames(m) <- rownames(c$r)
  m
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
    sondy_ucast2 <- sondy_mean %>% left_join(sondy_lower, by = c("duvod")) %>% left_join(sondy_upper, by = c("duvod")) %>%  mutate(grp = "1", n = sondy_n$n)
    
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
    sondy_ucast2 <- sondy_mean %>% left_join(sondy_lower,by = "duvod") %>% left_join(sondy_upper,by = "duvod")  %>%  mutate(grp = "1", n = sondy_n$n)
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

summarize_group_podil_prace <- function(df, grp_var = NULL) {
  v <- vars(podil_casu_ve_skautu_u_detskych_oddilu:podil_casu_ve_skautu_jine)
  v2 <- quo(podil_casu_ve_skautu_u_detskych_oddilu:podil_casu_ve_skautu_jine) 
  
  if (is.null(grp_var)) {
    sondy_mean <- df %>%
      summarize_at(.vars = v, .funs = mean) %>%
      gather(key = "duvod", value = "prumer", !! v2)
    
    sondy_lower <- df %>% 
      summarize_at(.vars = v, .funs = lower_lim) %>%
      gather(key = "duvod", value = "lower", !! v2)  
    
    sondy_upper <- df %>% 
      summarize_at(.vars = v, .funs = upper_lim) %>%
      gather(key = "duvod", value = "upper", !! v2)  
    sondy_n <- df %>% 
      summarize(n = n()) 
    sondy_ucast2 <- sondy_mean %>% left_join(sondy_lower, by = c("duvod")) %>% left_join(sondy_upper, by = c("duvod"))  %>% mutate(grp = "1", n = sondy_n$n)
    return(sondy_ucast2)
  } else {
    df$grp <- as.factor(df[[grp_var]])
    sondy_mean <- df %>%
      group_by(grp) %>% 
      summarize_at(.vars = v, .funs = mean) %>%
      gather(key = "duvod", value = "prumer", !! v2)
    
    sondy_lower <- df %>% 
      group_by(grp) %>% 
      summarize_at(.vars = v, .funs = lower_lim) %>%
      gather(key = "duvod", value = "lower", !! v2)  
    
    sondy_upper <- df %>% 
      group_by(grp) %>% 
      summarize_at(.vars = v, .funs = upper_lim) %>%
      gather(key = "duvod", value = "upper", !! v2)  
    sondy_n <- df %>% 
      group_by(grp) %>% 
      summarize(n = n()) 
    sondy_ucast2 <- sondy_mean %>% left_join(sondy_lower, by = c("grp", "duvod")) %>% left_join(sondy_upper, by = c("grp", "duvod")) %>% left_join(sondy_n, by = "grp")
    return(sondy_ucast2)
  }
}

summarize_group_typ_programu <- function(df, grp_var = NULL) {
  v <- vars(roverske_akce_probehle_2roky_zavody:roverske_akce_kurzy_vzdelavaci_akce)
  v2 <- quo(roverske_akce_probehle_2roky_zavody:roverske_akce_kurzy_vzdelavaci_akce) 
  
  if (is.null(grp_var)) {
    sondy_mean <- df %>%
      summarize_at(.vars = v, .funs = mean) %>%
      gather(key = "duvod", value = "prumer", !! v2)
    
    sondy_lower <- df %>% 
      summarize_at(.vars = v, .funs = lower_lim) %>%
      gather(key = "duvod", value = "lower", !! v2)  
    
    sondy_upper <- df %>% 
      summarize_at(.vars = v, .funs = upper_lim) %>%
      gather(key = "duvod", value = "upper", !! v2)  
    sondy_n <- df %>% 
      summarize(n = n()) 
    sondy_ucast2 <- sondy_mean %>% left_join(sondy_lower, by = c("duvod")) %>% left_join(sondy_upper, by = c("duvod")) %>% mutate(grp = "1", n = sondy_n$n)
    return(sondy_ucast2)
  } else {
    df$grp <- as.factor(df[[grp_var]])
    sondy_mean <- df %>%
      group_by(grp) %>% 
      summarize_at(.vars = v, .funs = mean) %>%
      gather(key = "duvod", value = "prumer", !! v2)
    
    sondy_lower <- df %>% 
      group_by(grp) %>% 
      summarize_at(.vars = v, .funs = lower_lim) %>%
      gather(key = "duvod", value = "lower", !! v2)  
    
    sondy_upper <- df %>% 
      group_by(grp) %>% 
      summarize_at(.vars = v, .funs = upper_lim) %>%
      gather(key = "duvod", value = "upper", !! v2)  
    sondy_n <- df %>% 
      group_by(grp) %>% 
      summarize(n = n()) 
    sondy_ucast2 <- sondy_mean %>% left_join(sondy_lower, by = c("grp", "duvod")) %>% left_join(sondy_upper, by = c("grp", "duvod")) %>% left_join(sondy_n, by = "grp")
    return(sondy_ucast2)
  }
}

plot_sondy_podil_casu <- function(df) {
  df %>%  
    ggplot(aes(x = duvod, y = prumer, col = grp, group = grp)) +
    geom_point() +
    geom_linerange(aes(ymin = lower, ymax = upper)) +
    geom_line() +
    #geom_text(x = 1, y = 60, aes(label = n)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    scale_x_discrete(labels = c("Jine", "Roverska cinnost", "U detskych oddilu", "Ucast/pomoc celostatni a vzdelavaci")) +
    ylim(0,100)
  
}

plot_sondy_ucast <- function(df) {
  df %>%  
    ggplot(aes(x = duvod, y = prumer, col = grp, group = grp)) +
    geom_point() +
    geom_linerange(aes(ymin = lower, ymax = upper)) +
    geom_line() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    scale_x_discrete(labels = ) +
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

plot_sondy_clusters <- function(df, lab = c("zavody", "expedice", "tabor", "programy", "pobyt v prirode", "zahranicni akce", "kulturni akce", "diskuze", "tradicni akce", "setkani bez programu/hospoda", "sluzba", "velke rs akce", "vzdelavaci akce"),ylim_min = 0, ylim_max = 1) {
  df %>%  
    ggplot(aes(x = duvod, y = prumer, col = grp, group = grp)) +
    geom_point() +
    geom_linerange(aes(ymin = lower, ymax = upper)) +
    geom_line() +
    #geom_text(x = 1, y = 60, aes(label = n)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    scale_x_discrete(labels = lab) +
    ylim(ylim_min, ylim_max) +
    scale_color_discrete(labels = )
  
}

subgroup_corr <- function(df, v, grp_var, grp_id){
  
  df %>% 
    filter_at(grp_var, all_vars(. == grp_id)) %>%
    select_at(v) %>% 
    as.matrix() %>% Hmisc::rcorr(type = "spearman") %>% 
    format_corrtable() 
}

compare_corr_table <- function(df,v,grp_var1,grp_id1, grp_var2, grp_id2) {
  c1 <- df %>% 
    filter_at(grp_var1, all_vars(. == grp_id1)) %>%
    select_at(v) %>% 
    as.matrix() %>% Hmisc::rcorr(type = "spearman")
  c2 <- df %>% 
    filter_at(grp_var2, all_vars(. == grp_id2)) %>%
    select_at(v) %>% 
    as.matrix() %>% Hmisc::rcorr(type = "spearman")
  cr <- c1$r-c2$r
  ggcorrplot(c1$r-c2$r, type = "lower")
}