# if(!grepl("cz|utf|Slovak", Sys.getlocale(), ignore.case = TRUE)) {
#   Sys.setlocale(locale= 'English_United States.1250')
# }

devtools::load_all()
library(here)
library(tidyverse)
library(patchwork)
library(formr)

options(mc.cores = parallel::detectCores())
rstan::rstan_options(auto_write = TRUE)
options(dplyr.summarise.inform = FALSE)



source(here("setup_knitr.R"))


set_theme_revizers()

