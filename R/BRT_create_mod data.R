## script to run BRT
### in modeling script 

library(dismo)
library(dplyr)
library (parallel)
library(tidyverse)
library(lubridate)
library(scam)
library(readr)


# source global functions
source("R/global_functions.R")

mc.cores= 4

#still have to read in all the data at first 

# read in min_temp data
min_temp <- readRDS("Data/RDS outs/min_temp_data_summarized.RDS") %>%
  unite(year_day, year, day, sep="_", remove=FALSE)

min_temp_10 <- readRDS("Data/RDS outs/min_temp_with_mean10.rds") %>%
  unite(year_day, year, day, sep="_", remove=FALSE)

# replacing column name
colnames(min_temp_10)[which(names(min_temp_10) == "temp_10_days")] <- "min_temp_10_days"

# read in max_temp data
max_temp <- readRDS("Data/RDS outs/max_temp_data_summarized.RDS") %>%
  unite(year_day, year, day, sep="_", remove=FALSE)

max_temp_10 <- readRDS("Data/RDS outs/max_temp_with_mean10.rds") %>%
  unite(year_day, year, day, sep="_", remove=FALSE)

# replacing column name
colnames(max_temp_10)[which(names(max_temp_10) == "temp_10_days")] <- "max_temp_10_days"

# read in rainfall data
rainfall <- readRDS("Data/RDS outs/rainfall_data_summarized.RDS") %>%
  unite(year_day, year, day, sep="_", remove=FALSE)

rainthree <- readRDS("Data/RDS outs/rainfall_cum_3_days.RDS") %>%
  unite(year_day, year, day, sep="_", remove=FALSE)

rain10 <- readRDS("Data/RDS outs/rainfall_cum_10_days.RDS") %>%
  unite(year_day, year, day, sep="_", remove=FALSE)

vapour <- readRDS("Data/RDS outs/vapour_data_summarized.RDS") %>%
  mutate(previous_day=Date-1) %>%
  mutate(year=year(previous_day)) %>%
  mutate(day=yday(previous_day)) %>%
  mutate(month=month(previous_day)) %>%
  unite(year_day, year, day, sep="_", remove=FALSE) %>%
  dplyr::select(-Date, -previous_day)

# pick a species
species_name <- "Crinia_signifera" 




species_list <- readRDS("raw_frogid_data/FrogID_clean_data.RDS") %>%
  group_by(species)%>%
  summarize(count=n())%>%
  filter(count >100)%>%
  distinct(species)%>%
  select(species)

 
#skip to here - load summary species stats 
summary_data <- read_rds("Data/summary_species_stats.RDS")


##start this but stop and save mod_data
# make a function to run a model for each species
run_mod_data_function <- function(species_name){
  
  message(paste0("Analyzing ", species_name))
  
  # first read in the data
  # and mutate some time variables
  dat <- readRDS(paste0("Data/species_range_model_data/", gsub(" ", "_", species_name), ".RDS")) %>%
    mutate(day=yday(date)) %>% # add the julian day
    mutate(year=year(date)) %>% # add the year
    unite(year_day, year, day, sep="_", remove=FALSE) 
  # unite the two together for a key for later
  
  # get data for analysis
  analysis_dat <- dat %>%
    left_join(., min_temp) %>%
    left_join(., max_temp) %>%
    left_join(., min_temp_10)%>%
    left_join(., max_temp_10)%>%
    left_join(., vapour) %>%
    left_join(., rainfall) %>%
    left_join(., rainthree)%>%
    left_join(., rain10) 
  
  
  ## Need to split to 'grid analysis' too
  grid_info_only <- analysis_dat %>%
    dplyr::select(grid_id, grid_lat, grid_lng,
                  grid_sunrise, grid_sunset, grid_day_length,
                  fraction, phase, year_day:rain_10_days) %>%
    distinct()
  
  ## Splitting by grid analysis here
  samples_per_grid_and_day <- analysis_dat %>% 
    group_by(grid_id, year_day, present) %>% 
    summarize(N=n()) %>% 
    ungroup() %>% 
    group_by(grid_id, year_day) %>% 
    mutate(number_of_samples=sum(N)) %>% 
    dplyr::select(-N) %>%
    group_by(grid_id, year_day) %>%
    arrange(desc(present)) %>%
    slice(1) %>%
    ungroup() %>%
    right_join(grid_info_only) %>%
    mutate(present_char=case_when(
      present==0 ~ "Absent",
      present==1 ~ "Present"
    ))
  
  # omit data
  mod_data <- samples_per_grid_and_day %>%
    dplyr::filter(complete.cases(.)) %>%
    dplyr::select(present, number_of_samples, day,
                  grid_lat, grid_lng, mean_max_temp, max_temp_10_days,
                  mean_min_temp, min_temp_10_days, phase, rain_10_days, 
                  rain_10_days, rain_3_days, mean_rainfall, mean_vapour) # choose the variables to model
  mod_data <- mod_data[, c(1,2,4,5,3,6,7,8,9,10,11,12,13,14)]
  
  write_rds(mod_data,sprintf("Data/mod_data/%s.rds", species_name))
  
}

# test the function on an example species
run_mod_data_function("Crinia_signifera")

lapply_with_error(species_list$species, run_mod_data_function)


