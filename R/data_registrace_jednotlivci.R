library(lubridate)
library(readxl)
library(tidyverse)

registrace_jednotlivci <- function() {

  registrace <- read_excel(here("private_data","Registrace (2003 až 2018) - psedonymizovaný seznam členů.xlsx"),col_types = c("numeric","numeric","numeric","date","text","text","text"))
  registrace <- registrace %>%
    mutate(Year = as.integer(Year), Unit_PseudoID = as.integer(Unit_PseudoID),
           Person_PseudoID = as.integer(Person_PseudoID), ID_Sex = factor(ID_Sex),
           ID_MembershipType = factor(ID_MembershipType),
           ID_MembershipCategory = factor(ID_MembershipCategory),
           rok_narozeni = year(Birthday),
           vek = Year - rok_narozeni)

  cat("Data obsahují", sum(registrace$ID_Sex == "NULL"), "řádků s neznámým pohlavím. Ignoruji je.\n")
  registrace <- registrace %>% filter(ID_Sex != "NULL") %>% droplevels()

  registrovani_pred_narozenim <- registrace %>% filter(rok_narozeni >= Year) %>% get("Person_PseudoID",.) %>% unique()

  cat("Data obsahují", length(registrovani_pred_narozenim), "osob, které byly registrovány dříve, než se narodily. Ignoruji je.\n")
  registrace <- registrace %>% filter(!(Person_PseudoID %in% registrovani_pred_narozenim))

  registrace
}
