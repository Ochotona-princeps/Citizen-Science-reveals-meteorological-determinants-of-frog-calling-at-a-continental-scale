

#run for species that might have been misIDed
#esp those with small data sets to make sure 
#the models aren't skewed

packages

library(readr)
library(dplyr)
library(ggplot2)
library(gganimate)
library(lubridate)
library(bdvis)

#read data in
crosscheck <- read_csv("Data/raw_frogid_data/clean_data.csv")
#old data 
data<- read_rds("Data/raw_frogid_data/frogid_dat.RDS")

#filter for a given species
species_name <- "Crinia glauerti"
df <- data %>%
  filter(species == species_name)
#select the four necessary data types
df <- df %>%
  dplyr::select(lat, lng, date, species) %>%
  rename(Latitude = lat) %>%
  rename(Longitude = lng) %>%
  rename(Date_collected = date) %>%
  rename(Scientific_name = species)
#title of plot
title <- gsub("_", " ", species_name)
#add number of observations
N <- paste0("(N = ", as.character(as.numeric(nrow(df))), " obs)")
#ad a cellid
df <- getcellid(df)
#make a calendar heat map
bdcalendarheat(df)%>%
  #as.Date(week)


------
  #after viewing heat maps maps
  #target outliers 
  #EX
  
  cg<- data %>%
  filter(species == "Crinia deserticola")%>%
  filter (date == "2018-11-29")

