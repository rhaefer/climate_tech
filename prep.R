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
library(openmeteo)
library(lubridate)
library(geojsonio)
library(lehdr)
library(tigris)
library(mapview)


# lodes<-grab_lodes(state = c("ca"), 
#            year = c(2021), 
#            lodes_type = "od", 
#            job_type = "JT01", 
#            segment = "S000", 
#            state_part = "main", 
#            agg_geo = "tract")

#write_parquet(lodes %>% slice(1:1000), 'data/lodes.parquet')

lodes<-read_parquet('data/lodes.parquet')

tracts<-tracts(state="CA")

aadt<-geojson_read('data/Traffic_Volumes_AADT.geojson', what='sp') %>% st_as_sf(crs=4326)

ba_geo<-geojson_read('data/California_Electric_Balancing_Authority.geojson', what='sp') %>% st_as_sf(crs=4326)

# DC_fast_charging_stations_that_do_not_meet_NEVI_requirements_but_within_1-mile_of_a_corridor_(updated_October_2023)
charger_no_nevi<-geojson_read('data/DC_fast_charging_stations_that_do_not_meet_NEVI_requirements_but_within_1-mile_of_a_corridor_(updated_October_2023).geojson', what='sp') %>% st_as_sf(crs=4326)

charger_nevi<-geojson_read('data/Stations_that_meet_NEVI_requirements_(October_2023).geojson', what='sp') %>% st_as_sf(crs=4326)

e_fuel_cor<-geojson_read('data/Electric_Fuel_Corridor_Groups_(Updated_December_2023).geojson', what='sp') %>% st_as_sf(crs=4326)

heavy_infra<-geojson_read('data/Medium_and_Heavy_Duty_Infrastructure.geojson', what='sp') %>% st_as_sf(crs=4326)

### open meteo api call
weather<-weather_history(location="South Lake Tahoe", start="2014-02-15", end=Sys.Date(),
                      hourly = c("temperature_2m","precipitation","windspeed_10m","cloudcover","pressure_msl")) %>%
  mutate(temp_f=((hourly_temperature_2m * (9/5))) + 32) %>% pivot_longer(cols=2:7) %>%
  mutate(date_time_tidy=ymd_hms(datetime),
         date=ymd(as.Date(date_time_tidy)))

###

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
