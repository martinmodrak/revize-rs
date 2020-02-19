if(!grepl("cz|utf", Sys.getlocale(), ignore.case = TRUE)) {
  Sys.setlocale(locale= 'English_United States.1250')
}
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


options(mc.cores = parallel::detectCores())

knitr::opts_chunk$set(fig.width = 8, fig.height = 4.5)

source(here::here("R","tools_kompetence.R"), encoding = "UTF-8")
source(here::here("R","metadata_dotaznik.R"), encoding = "UTF-8")
source(here::here("R","preprocess_dotaznik.R"), encoding = "UTF-8")
source(here::here("R","tools_plots.R"), encoding = "UTF-8")
source(here::here("R","tools_INLA.R"), encoding = "UTF-8")
source(here::here("R","tools_fa.R"), encoding = "UTF-8")

set_theme_revizers()
