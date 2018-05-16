jednotky_oddily_kmeny_predvyzkum <- function() {

  #warning("Očekávám, že v souboru '2017-02-17 Přehled jednotek s počty členů 2004 - 2016-3_F_170225.xlsx' byl upraven sloupec ")
  jednotky  <- readxl::read_excel(here("private_data","skautis","2017-02-17 Přehled jednotek s počty členů 2004 - 2016-3_F_170225.xlsx"), sheet = "List1",col_types = c("numeric","text","text","text","text","text","text")) %>% rename(ev_cislo = RegistrationNumber, nazev_jednotky = DisplayName, mesto = City, ulice = Street, psc = Postcode )

  warning("Používám rucne upravený soubor - rok_registrace je v originalnim 'data_pracovni_dodatek_170624.csv.xlsx'  castecne formatovan jako datum, nutno vyřešit")
  #registrace  <- readxl::read_excel(here("private_data","skautis","data_pracovni_dodatek_170624.csv.xlsx"), sheet = "registrace", col_types = c("text","text", "text", "numeric", "date","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"), na = "NULL")
  registrace  <- readxl::read_excel(here("private_data","skautis","data_pracovni_dodatek_170624_upraveno.xls"), sheet = "registrace", col_types = c("text","text", "text", "numeric", "date","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"), na = "NULL")

  pocty_oddilu  <- readxl::read_excel(here("private_data","skautis","data_pracovni_dodatek_170624.csv.xlsx"), sheet = "pocty_oddilu", col_types = c("numeric","text","text","numeric"), na = "NULL") %>% rename(typ_oddilu = DisplayName, pocet_oddilu = Count)

  web  <- readxl::read_excel(here("private_data","skautis","data_pracovni_dodatek_170624.csv.xlsx"), sheet = "webove_stranky", col_types = c("numeric","text","numeric","text","text","text"), na = "NULL")

  ma_kmen <- pocty_oddilu %>% filter(rok == 2017 & typ_oddilu == "Kmen roverů/rangers") %>% mutate(ma_kmen = pocet_oddilu > 0) %>% select(ev_cislo, ma_kmen)

  web_condensed <- web %>% select(ev_cislo,web, typ_jednotky) %>% group_by(ev_cislo, typ_jednotky) %>% summarise(web = paste(web, collapse = ", "))

  je_stredisko <- function(nazev_jednotky, ev_cislo) {
    grepl("středisko|přístav",nazev_jednotky,ignore.case = TRUE) &
      !grepl("^[90]00", ev_cislo) #Středisko správy majetku, Gemini, Parvula, ústředí mají ev. číslo 000.x nebo 900.x
  }

  data  <- jednotky %>%
    left_join(registrace %>% filter(rok_registrace == 2017), by = c("ev_cislo" = "ev_cislo","IC" = "IC"))  %>%
    left_join(ma_kmen, by = c("ev_cislo" = "ev_cislo")) %>%
    mutate(ma_kmen = if_else(is.na(ma_kmen), FALSE, ma_kmen)) %>%
    left_join(web_condensed, by = c("ev_cislo" = "ev_cislo"))  %>%
    mutate(typ_jednotky = as.factor(if_else(is.na(typ_jednotky),
                                            if_else(je_stredisko(nazev_jednotky, ev_cislo), 'stredisko',
                                                    if_else(grepl("okres",nazev_jednotky), 'okres',
                                                            if_else(grepl("kraj",nazev_jednotky),'kraj','zvlastniJednotka'))
                                            )
                                            , typ_jednotky))
    )

  if(any(is.na(data$typ_jednotky))) {
    stop("Některým jednotkám se nepodařilo přiřadit typ")
  }
  data <- data %>%
    mutate(
      kategorie_do6let = if_else(is.na(kategorie_do6let), 0, kategorie_do6let),
      kategorie_7az15let = if_else(is.na(kategorie_7az15let), 0, kategorie_7az15let),
      kategorie_16az18let = if_else(is.na(kategorie_16az18let), 0, kategorie_16az18let),
      kategorie_19az26let = if_else(is.na(kategorie_19az26let), 0, kategorie_19az26let),
      kategorie_nad26let = if_else(is.na(kategorie_nad26let), 0, kategorie_nad26let),
      clenove_celkem = if_else(is.na(celkem), 0, celkem)
    )
  data  <- data %>% mutate(pocet_roveru = kategorie_16az18let + kategorie_19az26let)

  if(length(unique(data$ev_cislo)) != length(unique(jednotky$ev_cislo))) {
    stop("Ztráta jednotek")
  }

  data
}

normalizovat_mesto_psc <- function(data) {
  #Vyresit nestandartní adresy
  non_existent_psc_map = c(
    "28000" = "28002",
    "25002" = "25001",
    "18102" = "18100",
    "77200" = "77900",
    "40501" = "40502",
    "64402" = "66402"
  )

  mesto_map = c(
    "Rájec Jestřebí" = "Rájec-Jestřebí",
    "Rožmitál p. Tř." = "Rožmitál pod Třemšínem",
    "Libčice n.Vlt." = "Libčice nad Vltavou",
    "Lipník n.B." = "Lipník nad Bečvou",
    "České Budejovice" = "České Budějovice",

    #Zahazuji doplnky nazvu, protoze ty jsou reseny PSC
    "Hlinsko v Čechách" = "Hlinsko",
    "Čechovice, Velký Týnec" = "Čechovice",
    "Šlapanice u Brna" = "Šlapanice",
    "Staré Město u Uherského Hradiště" = "Staré Město",
    "Chrast u Chrudimě" = "Chrast"

  )


  data %>%
    mutate(mesto_orig = mesto, psc_orig = psc,
           mesto = gsub("^Praha[ \\-].*$","Praha",mesto),
           mesto = gsub("^Brno[ \\-].*$", "Brno", mesto),
           mesto = gsub("^Ostrava[ \\-].*$", "Ostrava", mesto),
           mesto = gsub("^Havířov[ \\-].*$", "Havířov", mesto),
           mesto = gsub("^[0-9 ]*","",mesto), #leading numbers (e.g. psc duplicate)
           mesto = gsub("[0-9 ]*$","",mesto), #trailing numbers (e.g. cislo popisne)
           mesto = gsub(" ?- ?","-",mesto), #spaces around "-"
           mesto = gsub(",* *pošta.*$","",mesto)
    ) %>%
    rowwise() %>%
    mutate(mesto = if_else(mesto %in% names(mesto_map), mesto_map[mesto], mesto),
           psc = if_else(psc %in% names(non_existent_psc_map), non_existent_psc_map[psc], psc),
           psc = if_else(mesto == "Česká Ves" && psc == "79001","79081", psc)) %>% #Specialni pripad - je vice ceskych vsi a ceska ves ma vice PSC
    ungroup()
}
