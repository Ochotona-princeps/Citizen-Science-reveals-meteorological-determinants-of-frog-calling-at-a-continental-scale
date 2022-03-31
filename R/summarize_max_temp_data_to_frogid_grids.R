## A script to read in environmental data from the sf format
## and then aggregate to the 'grids' used in the analysis for FrogID

# packages
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)

library(parallel) 

mc.cores = 4

# read in aus grids
# and set crs

aus_grids<- readRDS("Data/spatial_data/grids_10km.RDS")


#load("Data/spatial_data/aus_grids.RData")
st_crs(aus_grids) <- 4326

# write a function that takes every RDS
# in a folder, which incorporates the rain data
# and summarizes it into the FrogID grids


summarize_max_temp_data <- function(x){
  
  message(paste0("Analyzing ", x))
  
  max_temp_dat <- readRDS(paste0("Data/max_temp_data_sf_RDS/", x))
  
  st_crs(max_temp_dat) <- 4326
  
  max_temp_dat <- max_temp_dat %>%
    st_transform(crs=4326) %>%
    mutate(row.id=1:nrow(.))
  
  joined_grids <- max_temp_dat %>%
    st_intersects(aus_grids) %>%
    as.data.frame() %>%
    group_by(row.id) %>%
    slice(1) %>%
    right_join(., max_temp_dat %>%
                 st_set_geometry(NULL), by="row.id") %>%
    rename(max_temp_grid_id=row.id) %>%
    rename(aus_grids_grid_id=col.id)
  
  grids_summarized <- joined_grids %>%
    group_by(aus_grids_grid_id) %>%
    summarize(Number_of_max_temp_grids=n(),
              mean_max_temp=mean(max_temp, na.rm=TRUE),
              median_max_temp=median(max_temp, na.rm=TRUE),
              sd_max_temp=sd(max_temp, na.rm=TRUE),
              min_max_temp=min(max_temp, na.rm=TRUE),
              max_max_temp=max(max_temp, na.rm=TRUE)) %>%
    mutate(Date=unique(joined_grids$Date)) %>%
    mutate(year=year(Date)) %>%
    mutate(month=month(Date)) %>%
    mutate(day=yday(Date)) %>%
    rename(grid_id=aus_grids_grid_id)
  
  return(grids_summarized)
  
}

# list of files
file_list <- list.files("Data/max_temp_data_sf_RDS/")

# apply the function to every RDS file in the RDS file
results_list <- lapply(file_list, function(x){summarize_max_temp_data(x)})
#running 2019 only RN, ran 2017 and 2018 in one batch, still need 2020
df_of_results <- bind_rows(results_list)


df_of_results2<- df_of_results %>% 
  select(-sd_max_temp, -median_max_temp, -Number_of_max_temp_grids, -max_max_temp, -min_max_temp)

saveRDS(df_of_results2, "Data/RDS outs/max_temp_data_summarized.RDS")

check<- readRDS("Data/RDS outs/max_temp_data_summarized.RDS")
