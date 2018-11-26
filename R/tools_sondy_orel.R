nacti_sondy <- function() {
  if(!file.exists("private_data/sondy_181101.rds")) {
    stop("Nenalezena predzpracovana data ze sond. Spust sondy_prepare_data.R")
  }
  sondy_raw <- readRDS("private_data/sondy_181101.rds")

  sondy <- sondy_raw %>%
    filter(q0_clen_junaka == "Ano") %>%
    mutate(
      max_funkce = if_else(
        q0_vedouci_strediska == "Ano", "vudce_strediska", if_else(
          q0_vudce_oddilu == "Ano", "vudce_oddilu",if_else(
            q0_vudce_rs_kmene == "Ano", "vudce_kmene", if_else(
              funkce_v_oddile_zastupce_VO == "Ano", "zastupce_v_oddilu", if_else(
                funkce_v_oddile_oddilovy_radce == "Ano", "oddilovy_radce", if_else(
                  funkce_v_oddile_druzinovy_radce == "Ano", "druzinovy_radce", if_else(

                    q0_ve_vedeni_oddilu == "Ano" | funkce_v_oddile_clen_vedeni_oddilu == "Ano", "sirsi_vedeni_oddilu", "nic"
                  ))))))),
      max_funkce = fct_relevel(max_funkce, "vudce_strediska", "vudce_oddilu", "vudce_kmene", "zastupce_v_oddilu", "oddilovy_radce","druzinovy_radce","sirsi_vedeni_oddilu"),
      max_funkce_v_kmeni = case_when(
        rs_kmen_moje_funkce_vudce == "Ano" ~ "vudce",
        rs_kmen_moje_funkce_rada == "Ano" ~ "rada",
        !is.na(rs_kmen_moje_funkce_jine) ~ "jine",
        rs_kmen_moje_funkce_clen == "Ano" ~ "clen",
        TRUE ~ "zadna"
      ),
      max_funkce_v_kmeni = fct_relevel(max_funkce_v_kmeni, "vudce","rada","jine","clen", "zadna"),
      celkova_spokojenost_s_programem_rs_kmenu = fct_relevel(celkova_spokojenost_s_programem_rs_kmenu, "Velmi nespokojen/a", "Spíše nespokojen/a","Spíše spokojen/a","Velmi spokojen/a"),
      kdo_pripravuje_program = fct_recode(vyrok_o_spolecenstvi, uzka_skupina = "Program připravuje úzká skupina lidí", vsichni =  "Na přípravě programu se podílí různí členové podle toho, o jakou činnost se jedná.", vudce =  "Program připravuje vůdce/vůdkyně kmene nebo roverského společenství"   ),
      studium_ve_meste_strediska = fct_recode(studium_ve_meste_strediska, Ne = "Ne, většinu roku trávím mimo místo, kde mám svoje středisko")
    )

  sondy
}
