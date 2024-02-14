# NYMTC OD Flow Dashboard 
# Developed by Reid Haefer, Nicholas Fisher, Kyeongsu Kim at RSG
# Latest Update on 11/22/2023 

### preppppppppppppp ###

# TO do list 
# update scripts that replaces legacy packages maptools, rgdal, and rgeos, underpinning the sp package


library(shiny)
library(shinydashboard)
library(shinyjs)
library(arrow)
library(data.table)
library(tidyverse)
library(sf)
library(leaflet)
library(plotly)
library(DT)
library(shinycssloaders)
library(shinyWidgets)
library(tigris)
library(scales)
library(RColorBrewer) 
library(formattable)
library(reactablefmtr)
library(htmltools)
library(ggalluvial)
library(classInt)
library(lwgeom)
library(nasapower)

energy_consumed_locally_by_source_ba<-fread("data/energy_consumed_locally_by_source_ba.csv")

monthly_energy_balance_tidy<-fread("data/monthly_energy_balance_tidy.csv")

usage_by_ba_and_generation_type<-fread("data/usage_by_ba_and_generation_type.csv")

local_generation_grid_mix<-fread("data/local_generation_grid_mix.csv")

baby_energy_model<-fread("data/baby_energy_model.csv")

meteo<-fread("data/meteo.csv")


meteo_df <- as.data.frame(meteo)

daily_single <- get_power(
  community = "ag",
  lonlat = c(-119.981612, 38.918158),
  pars = c("RH2M", "T2M", "PRECTOTCORR"),
  dates = c("2020","2019","2018","2017","2016","2015","2014"),
  temporal_api = "monthly"
)


iot <- fread("data/iot.csv") %>%
  mutate(`Temperature (F) - median` = ((`Temperature (C) - median` * (9/5))) + 32)
iot_long <- iot %>% pivot_longer(cols=3:13)


local_generation_grid_mix <- local_generation_grid_mix %>% mutate(`Generation (MWh)`=as.numeric(`Generation (MWh)`))
energy_consumed_locally_by_source_ba <- energy_consumed_locally_by_source_ba %>% mutate(`Power consumed locally (MWh)`=as.numeric(`Power consumed locally (MWh)`))
usage_by_ba_and_generation_type <- usage_by_ba_and_generation_type%>% filter(!if_any(everything(), ~is.na(.) | is.infinite(.)))  %>% mutate(`Usage (MWh)`=as.numeric(`Usage (MWh)`))

home_latitude = 38.918158
home_longitude = -119.981612