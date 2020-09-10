# if(!grepl("cz|utf|Slovak", Sys.getlocale(), ignore.case = TRUE)) {
#   Sys.setlocale(locale= 'English_United States.1250')
# }

devtools::load_all()
library(here)
library(tidyverse)
library(patchwork)
library(formr)

# library(lubridate)
# library(rlang)
# library(formr)
# library(RCzechia)
# library(sf)
# library(polycor)
# library(vegan)
# library(cowplot)
# library(brms)


options(mc.cores = parallel::detectCores())
rstan::rstan_options(auto_write = TRUE)
options(dplyr.summarise.inform = FALSE)


knitr::opts_chunk$set(fig.width = default_plot_width, fig.height = default_plot_height)


set_theme_revizers()
