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
