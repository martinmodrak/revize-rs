pocty_clenu_skautis_cols <- cols(
  Year = col_integer(),
  ID_Unit = col_integer(),
  ID_UnitType = col_character(),
  RegistrationNumber = col_character(),
  UnitName = col_character(),
  Location = col_character(),
  RegularMembersTo6 = col_integer(),
  RegularMembersTo15 = col_integer(),
  RegularMembersTo18 = col_integer(),
  RegularMembersTo26 = col_integer(),
  RegularMembersFrom26 = col_integer(),
  RegularMembers = col_integer(),
  MembersTo6 = col_integer(),
  MembersTo15 = col_integer(),
  MembersTo18 = col_integer(),
  MembersTo26 = col_integer(),
  MembersFrom26 = col_integer(),
  Members = col_integer()
)


nacti_skautis_pocty_clenu <- function(file) {
  read_delim(file, delim = ";", na = c("NULL"),
          col_types = pocty_clenu_skautis_cols)
}

nacti_strediska_kraje <- function() {
  strediska_skautis <- nacti_skautis_pocty_clenu(here::here("public_data/pocet-clenu-strediska-2019.csv")) %>%
    filter(Year == 2019) %>%
    select(RegistrationNumber,UnitName, Location)
  kraje_skautis <- nacti_skautis_pocty_clenu(here::here("public_data/pocet-clenu-VOJ.csv")) %>%
    filter(Year == 2019) %>%
    select(ID_UnitType, RegistrationNumber, UnitName) %>% 
    filter(ID_UnitType == "kraj") %>% 
    select(-ID_UnitType)
  
  stopifnot(all(nchar(strediska_skautis$RegistrationNumber)==6))
  
  strediska_skautis %>% 
    separate(RegistrationNumber, sep = "[^[:alnum:]]", into =c("reg1","reg2"),remove=F) %>% # rozsekni reg.cislo do dvou sloupcu
    mutate(RegistrationNumber_kraj = (((str_split_fixed(reg1,pattern="",n=6)[1:2]) %>% paste0(collapse = "") %>% as.numeric())*10) %>% as.character()) %>% # trochu trik, jak vyseknout prvni dve cisla, pridame nulu a zpatky na string
    rename(UnitName_stredisko = UnitName) %>% 
    left_join(kraje_skautis, by = c("RegistrationNumber_kraj"="RegistrationNumber")) %>% # joineme s krajem
    select(RegistrationNumber_kraj,RegistrationNumber,UnitName_kraj = UnitName,UnitName_stredisko) %>% ungroup() # a vratime dobre sloupce
  
  # Reg. číslo střediska, Reg. číslo kraje, Název kraje
  
}
