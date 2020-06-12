kompetence <- c("samostatny", "fyzicky_zdatny", "pripraven_krize", "pripraven_bezny_zivot","resit_problemy","taboreni_tym","tvorivost_zrucnost","sebepoznani","duchovni_zivot","slib_zakon","svedomi","rozvoj_osobnosti","vztahy","komunikace","pomaham","rodina","skautsky_zit","clen_tymu","aktivni_obcan","propojenost_sveta","tolerantni","pobyt_v_prirode","vztah_k_prirode_krajine","setrnost")
kategorie_kompetence <- c("zvladam","dulezite","rozvijim", "skauting")

kompetence_nazvy_sloupcu <- data.frame(kompetence) %>%
  crossing(data.frame(kategorie_kompetence)) %>%
  mutate(nazev_raw = paste(kompetence, kategorie_kompetence, sep = "_"),
         nazev = paste(kompetence, kategorie_kompetence, sep = "."))


kompetence_otazky <- readxl::read_excel(here::here("public_data/kompetence_otazky.xlsx")) %>%
  mutate(ciselne_id = paste0(cislo_oblasti,".",cislo_podoblasti),
        popis_pro_grafy = paste0(ciselne_id, " ", otazka_strucne))

manual_codings <- list(
  kolik_casu = c("kratsi", "delsi"),
  sex = c("muz","zena","jinak_neuvedeno"),
  kategorie_respondenta = c("nyni_spolecenstvi", "drive_spolecenstvi", "nikdy_spolecenstvi"),
  bez_zkusenosti_mladsi = c("ano","ne"),
  jeste_pokracovat = c("ano","ne"),
  pocet_clenu_spolecenstvi = c("5_a_mene","6_10","11_20","21_30","31_a_vice"),
  frekvence_kratkych_akci = c("nikdy","rocne","nekolik_rocne","mesicne","nekolik_mesicne","tydne","nekolik_tydne"),
  frekvence_vicedennich_akci= c("nikdy","mene_nez_rocne","rocne","nekolik_rocne","mesicne","nekolik_mesicne"),
  frekvence_velkych_akci= c("nikdy","mene_nez_rocne","rocne","nekolik_rocne"),

  bez_zkusenosti_setkavam_se_s_vrstevniky = c("vubec","rocne","nekolik_rocne","casteji"),
  bez_zkusenosti_seberozvojovy_program = c("vubec","rocne","nekolik_rocne","casteji"),
  bez_zkusenosti_velke_akce = c("obrok","mixem","roverska_porada", "kurz_tabor", "regionalni_setkani",
                                "institut", "nic"),

  co_zazil = c("radce_podradce","radcovsky_kurz","cekatelky","vudcovky","roversky_kurz","jiny_kurz"),
  fungovani_skautskeho_oddilu = c("druzinovy_system", "radci_program_schuzky","radci_vedli_schuzky",
                                  "clenove_tvorili_program", "samostatne_schuzky", "samostatne_vypravy",
                                  "minimalni_dozor", "nebyl_clenem_druziny", "radcove_16let"),
  spolecenstvi_registrace = c("ruzna_strediska","kmen","klub_dospelych","clenove_kmen","clenove_u_oddilu",
                              "clenove_nereg", "nevim", "jine"),
  s_cim_spokojen = c("vztahy","program","cetnost_akci","kontakty","postoj_strediska"),
  s_cim_nespokojen = c("vztahy","program","cetnost_akci","kontakty","postoj_strediska"),
  organizace_spolecenstvi = c("formalni_vudce_zhury", "formalni_vudce_demokraticky", "formalni_rada_zhury",
                              "formalni_rada_demokraticky", "neformalni_tahoun", "neformalni_rada", "vsichni",
                              "nikdo", "neaktivni"),
  vyroky_o_roveringu_zazil = c("pro_jednu_gen", "podpora_strediska", "rovering_na_SR", "vedeni_je_rovering",
                               "koedukovany", "spoluprace_mimo_stredisko", "kurzy_akce"),
  vyroky_o_roveringu_zazil_2 = c("rover_automaticky", "vstupni_ritual", "snadny_prechod", "pri_prechodu_schopny",
                                 "mladsi_starsi", "prechodovy_ritual", "roversky_slib", "jasne_ukonceni",
                                 "jak_dlouho_chci"),
  problemy_roveringu = c("vytizeni_oddily", "odstehovani", "vytizeni_mimo", "nevidi_smysl", "bez_podpory_strediska",
                         "bez_vedeni", "spatne_vztahy", "bez_kvalitniho_programu", "pracovni_ceta",
                         "soupereni_mezi_skupinami", "nevime_jak"),
  sluzba = c("stredisku", "skauting_mimo", "rodina","lidem", "prirode", "dobrovolnictvi", "aktivni_obcan",
             "sobe", "nic"),
  proc_nebyl_rover = c("nelakalo", "bez_spolecenstvi", "vedeni", "program", "vztahy", "smysl", "jine"),

  vyroky_o_roveringu_stredisko = c("je_spolecenstvi", "pravidelne", "vlastni_akce", "akce_pro_druhe",
                                   "rozviji_se","roverske_heslo", "vedou", "pomocna_sila","bez_ulohy",
                                   "nejsou", "nevim"),

  komunikacni_kanaly_existujici = c("email","facebook","instagram","krizovatka","SI", "vedouci", "diar",
                                    "knihy", "casopis_kmen", "rovernet"),
  zivotni_faze = c("studuji","pracuji","studiji_pracuji","nestuduji_nepracuji"),
  typ_id_strediska = c("reg_cislo","obecne_informace","nevim", "bez_strediska"),
  pocet_clenu_strediska = c("70_a_mene","70_100","100_130","130_200","200_a_vice")
)

manual_codings$problemy_roveringu_stredisko = manual_codings$problemy_roveringu

ordered_sloupce <- c("frekvence_kratkych_akci","frekvence_vicedennich_akci", "frekvence_velkych_akci",
                     "bez_zkusenosti_setkavam_se_s_vrstevniky", "bez_zkusenosti_seberozvojovy_program")

# Metadata k multiple choice sloupcum (kde toho slo vybrat vic).
# Pokud je explicitní možnost "Nic" a pokud respondent nic nezaškrtl, tak můžeme odpověď brát jako NA, protože
# nedával pozor. To je podchyceno v "moznost_pro_kazdeho"
mc_sloupce <- list(
  role_skauting = list(),
  co_zazil = list(),
  fungovani_skautskeho_oddilu = list(),
  spolecenstvi_registrace = list(moznost_pro_kazdeho = TRUE),
  s_cim_spokojen = list(),
  s_cim_nespokojen = list(),
  organizace_spolecenstvi = list(moznost_pro_kazdeho = TRUE),
  vyroky_o_roveringu_zazil = list(),
  vyroky_o_roveringu_zazil_2 = list(),
  vychovne_nastroje = list(moznost_pro_kazdeho = TRUE),
  problemy_roveringu = list(),
  co_pomaha_roveringu = list(moznost_pro_kazdeho = TRUE),
  komunikacni_kanaly_existujici = list(),
  komunikacni_kanaly_hypoteticke = list(),
  proc_neni_rover = list(moznost_pro_kazdeho = TRUE),
  bez_zkusenosti_velke_akce = list(moznost_pro_kazdeho = TRUE),
  sluzba = list(moznost_pro_kazdeho = TRUE),
  proc_nebyl_rover = list(),
  vyroky_o_roveringu_stredisko = list(),
  problemy_roveringu_stredisko = list())


factor_sloupce  <- c("sex", "kolik_casu","kategorie_respondenta","kategorie_respondenta_full",
                     "bez_zkusenosti_mladsi", "pocet_clenu_spolecenstvi", "frekvence_kratkych_akci",
                     "frekvence_vicedennich_akci", "frekvence_velkych_akci",
                     "bez_zkusenosti_setkavam_se_s_vrstevniky", "bez_zkusenosti_seberozvojovy_program",
                     "jeste_pokracovat", "zivotni_faze", "typ_id_strediska", "pocet_clenu_strediska")

# Timto se nahradi factory, kde je NA
explicit_na_level <- "nevyplneno"

roverske_role <- c("vedouci_roveru", "clen_rady_roveru", "clen_roveru", "rover_sam", "tahoun_roveru")

