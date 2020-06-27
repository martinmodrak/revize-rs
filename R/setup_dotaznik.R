# if(!grepl("cz|utf|Slovak", Sys.getlocale(), ignore.case = TRUE)) {
#   Sys.setlocale(locale= 'English_United States.1250')
# }
library(lubridate)
library(rlang)
library(formr)
library(here)
library(RCzechia)
library(sf)
library(polycor)
library(vegan)
library(tidyverse)
library(cowplot)
library(patchwork)
library(brms)


options(mc.cores = parallel::detectCores())
options(dplyr.summarise.inform = FALSE)


source(here::here("R","tools_kompetence.R"), encoding = "UTF-8")
source(here::here("R","metadata_dotaznik.R"), encoding = "UTF-8")
source(here::here("R","preprocess_dotaznik.R"), encoding = "UTF-8")
source(here::here("R","tools_knitr.R"), encoding = "UTF-8")
source(here::here("R","tools_dotaznik.R"), encoding = "UTF-8")
source(here::here("R","tools_plots.R"), encoding = "UTF-8")
source(here::here("R","tools_skautis_opendata.R"), encoding = "UTF-8")
source(here::here("R","tools_brms.R"), encoding = "UTF-8")
source(here::here("R","tools_INLA.R"), encoding = "UTF-8")
source(here::here("R","tools_fa.R"), encoding = "UTF-8")

knitr::opts_chunk$set(fig.width = default_plot_width, fig.height = default_plot_height)


set_theme_revizers()
