## A script to read in environmental data from the sf format
## and then aggregate to the 'grids' used in the analysis for FrogID

# packages
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)

# read in aus grids
# and set crs
aus_grids<- readRDS("Data/spatial_data/grids_10km.RDs")

st_crs(aus_grids) <- 4326

###Maureen's hashtag what does this CRs do? 

# write a function that takes every RDS
# in a folder, which incorporates the rain data
# and summarizes it into the FrogID grids

summarize_rain_data <- function(x){
  
  message(paste0("Analyzing ", x))
  
  rain_dat <- readRDS(paste0("Data/rainfall_data_sf_RDS/", x))
  
  st_crs(rain_dat) <- 4326
  
  rain_dat <- rain_dat %>%
    st_transform(crs=4326) %>%
    mutate(row.id=1:nrow(.))
  
  joined_grids <- rain_dat %>%
    st_intersects(aus_grids) %>%
    as.data.frame() %>%
    group_by(row.id) %>%
    slice(1) %>%
    right_join(., rain_dat %>%
                 st_set_geometry(NULL), by="row.id") %>%
    rename(rain_grid_id=row.id) %>%
    rename(aus_grids_grid_id=col.id)
  
  grids_summarized <- joined_grids %>%
    group_by(aus_grids_grid_id) %>%
    summarize(Number_of_rainfall_grids=n(),
              mean_rainfall=mean(rain_value, na.rm=TRUE),
              median_rainfall=median(rain_value, na.rm=TRUE),
              sd_rainfall=sd(rain_value, na.rm=TRUE),
              min_rainfall=min(rain_value, na.rm=TRUE),
              max_rainfall=max(rain_value, na.rm=TRUE)) %>%
    mutate(Date=unique(joined_grids$Date)) %>%
    mutate(year=year(Date)) %>%
    mutate(month=month(Date)) %>%
    mutate(day=yday(Date)) %>%
    rename(grid_id=aus_grids_grid_id)
  
  return(grids_summarized)
  
}

# list of files
file_list <- list.files("Data/rainfall_data_sf_RDS/")

# apply the function to every RDS file in the RDS file
results_list <- lapply(file_list, function(x){summarize_rain_data(x)})

df_of_results <- bind_rows(results_list)

df_of_results2<- df_of_results %>% 
  select(-sd_rainfall, -median_rainfall, -Number_of_rainfall_grids, -max_rainfall, -min_rainfall)

saveRDS(df_of_results2, "Data/RDs outs/rainfall_data_summarized.RDS")
