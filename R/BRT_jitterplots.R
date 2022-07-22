### This is an R script to
### read in a species' data
### and make raw data jitter plots 

##should be with mod-data but takes forever now 

# packages
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(hrbrthemes)
library(mgcv)
library(broom)
library(patchwork)
library(MuMIn)
library(ggcorrplot)
library(GGally)
library(ranger)
library(scam)
library(PresenceAbsence)
devtools::install_github("zmjones/edarf", subdir = "pkg")
library(gridExtra)
library(forestmangr)
library(parallel)
# source global functions
source("R/global_functions.R")

mc.cores = 4

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



  species_list <- readRDS("raw_frogid_data/FrogID_clean_data.RDS") %>%
  group_by(species)%>%
  summarize(count=n())%>%
  filter(count >100)%>%
  distinct(species)%>%
  select(species)

# pick a species
species_name <- "Adelotus brevis" 


########### Function to read in and prepare data
########### and then make a plot of the species raw data and export plot


raw_data_plot_function <- function(species_name){
  
  # first read in the data
  # and mutate some time variables
  dat <- readRDS(paste0("Data/species_range_model_data/", gsub(" ", "_", species_name), ".RDS")) %>%
    mutate(day=yday(date)) %>% # add the julian day
    mutate(year=year(date)) %>% # add the year
    unite(year_day, year, day, sep="_", remove=FALSE) 

  
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
  
  ## make plots of the variables we are interested in
  summary <- samples_per_grid_and_day %>%
    dplyr::select(present_char, day, phase, mean_rainfall,
                  rain_3_days, rain_10_days, mean_max_temp, max_temp_10_days,
                  mean_min_temp, min_temp_10_days, mean_vapour) %>%
    rename(`DOY` = day,
           `Moon phase` = phase,
           `Mean rainfall` = mean_rainfall,
           `3-day rain total` = rain_3_days,
           `10-day rain total` = rain_10_days,
           `Mean max temperature` = mean_max_temp,
           `Mean of past 10 days max temp` = max_temp_10_days,
           `Mean min temperature` = mean_min_temp,
           `Mean of past 10 days min temp` = min_temp_10_days,
           `Mean vapour pressure` = mean_vapour) %>%
    pivot_longer(-present_char, names_to="Variable", values_to="Value") %>%
    mutate(Variable=factor(Variable, levels=c("Mean rainfall", "3-day rain total",
                                              "10-day rain total", "DOY",
                                              "Moon phase", "Mean vapour pressure",
                                              "Mean min temperature", "Mean of past 10 days min temp", "Mean max temperature", "Mean of past 10 days max temp")))
  ggplot(summary, aes(y=present_char, x=Value, color=Variable))+
    geom_jitter(alpha=0.5)+
    theme_bw()+
    theme(axis.text=element_text(color="black"))+
    facet_wrap(~Variable, scales="free")+
    scale_color_brewer(palette="Paired")+
    theme(legend.position="none")+
    xlab("Corresponding variable values")+
    ylab("")+
    ggtitle(paste0(species_name, "; Total sample size = ", 
                   nrow(samples_per_grid_and_day), 
                   "; Occurrences = ", samples_per_grid_and_day %>% 
                     dplyr::filter(present_char=="Present") %>% 
                     nrow(.)))
  
  
  ggsave(paste0("figures/BRT_raw_data_plots/", gsub(" ", "_", species_name), ".png"), width=9, height=7, units="in")
  
}  

# test the function on an example species
raw_data_plot_function("Adelotus brevis")

lapply_with_error(species_list$species, raw_data_plot_function)

