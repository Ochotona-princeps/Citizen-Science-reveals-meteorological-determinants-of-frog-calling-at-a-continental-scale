## script to add time zone then suncalc and mooncalc data


library(lubridate)
library(tidyverse)
library(suncalc)
library(parallel)

mc.cores= 4

dat <- read.csv("Data/clean_frogid_data_with_grids/in_range_observations.csv", header=T)


# now we will calculate the sun and moon stuff for each day
temp_dat <- dat %>%
  mutate(tz_name=case_when(
    states_name == "New South Wales" ~ "Australia/Sydney",
    states_name == "Western Australia" ~ "Australia/Perth",
    states_name == "Victoria" ~ "Australia/Victoria",
    states_name == "Tasmania" ~ "Australia/Tasmania",
    states_name == "Queensland" ~ "Australia/Brisbane", 
    states_name == "Australian Capital Territory" ~ "Australia/ACT",
    states_name == "Northern Territory" ~ "Australia/Darwin",
    states_name == "Jervis Bay Territory" ~ "Australia/Sydney"
  )) %>%
  dplyr::select(id, date, lat, lng, grid_lat, grid_lng, tz_name) %>%
  distinct()

suntime_function <- function(x){
  
  dat.temp <- temp_dat %>%
    slice(x)
  
  times_records <- getSunlightTimes(date=as.Date(dat.temp$date), lat=dat.temp$lat, lon=dat.temp$lng, tz=dat.temp$tz_name) %>%
    mutate(id=dat.temp$id) %>%
    dplyr::select(id, date, sunrise, sunset) %>%
    mutate(day_length=as.numeric(sunset-sunrise)) %>%
    rename(record_sunrise=sunrise) %>%
    rename(record_sunset=sunset) %>%
    rename(record_day_length=day_length)
  
  times_grids <- getSunlightTimes(date=as.Date(dat.temp$date), lat=dat.temp$grid_lat, lon=dat.temp$grid_lng, tz=dat.temp$tz_name) %>%
    mutate(id=dat.temp$id) %>%
    dplyr::select(id, date, sunrise, sunset) %>%
    mutate(day_length=as.numeric(sunset-sunrise)) %>%
    rename(grid_sunrise=sunrise) %>%
    rename(grid_sunset=sunset) %>%
    rename(grid_day_length=day_length)
  
  moons <- getMoonIllumination(date=as.Date(dat.temp$date), keep = c("fraction", "phase"))
  
  summary <- times_records %>%
    left_join(times_grids) %>%
    left_join(., moons)
  
  return(summary)
}

suntime_dat_results_list <- lapply(1:nrow(temp_dat), function(x){suntime_function(x)})

suntime_dat <- do.call(rbind.data.frame, suntime_dat_results_list)

suntime_dat2 <- suntime_dat %>%
  dplyr::select(-date) %>%
  distinct() 


# combine the suntime dat with the original dat and then export
# select the files necessary
dat2 <- dat %>%
  left_join(suntime_dat2) %>%
  dplyr::select(id, lat, lng, capture_time_in_zone,
                location_accuracy, geoprivacy, user_id, species, date, 
                number_of_sp_calling, water_body, habitat,
                lga_name, state, grid_id, grid_lat, grid_lng,
                record_sunrise, record_sunset, record_day_length,
                grid_sunrise, grid_sunset, grid_day_length, 
                fraction, phase)



# export file
# as RDS because csv was switching date/time columns
saveRDS(dat2, "Data/clean_frogid_data_with_grids/in_range_observations_with_moon_sun.RDS")
