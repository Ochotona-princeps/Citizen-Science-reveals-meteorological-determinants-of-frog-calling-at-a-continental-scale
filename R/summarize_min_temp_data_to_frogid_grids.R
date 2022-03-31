## A script to read in environmental data from the sf format
## and then aggregate to the 'grids' used in the analysis for FrogID

# packages
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)

# read in aus grids
# and set crs
aus_grids<- readRDS("Data/spatial_data/grids_10km.RDS")
st_crs(aus_grids) <- 4326

# write a function that takes every RDS
# in a folder, which incorporates the rain data
# and summarizes it into the FrogID grids


summarize_min_temp_data <- function(x){
  
  message(paste0("Analyzing ", x))
  
  min_temp_dat <- readRDS(paste0("Data/min_temp_data_sf_RDS/", x))
  
  st_crs(min_temp_dat) <- 4326
  
  min_temp_dat <- min_temp_dat %>%
    st_transform(crs=4326) %>%
    mutate(row.id=1:nrow(.))
  
  joined_grids <- min_temp_dat %>%
    st_intersects(aus_grids) %>%
    as.data.frame() %>%
    group_by(row.id) %>%
    slice(1) %>%
    right_join(., min_temp_dat %>%
                 st_set_geometry(NULL), by="row.id") %>%
    rename(min_temp_grid_id=row.id) %>%
    rename(aus_grids_grid_id=col.id)
  
  grids_summarized <- joined_grids %>%
    group_by(aus_grids_grid_id) %>%
    summarize(Number_of_min_temp_grids=n(),
              mean_min_temp=mean(min_temp, na.rm=TRUE),
              median_min_temp=median(min_temp, na.rm=TRUE),
              sd_min_temp=sd(min_temp, na.rm=TRUE),
              min_min_temp=min(min_temp, na.rm=TRUE),
              max_min_temp=max(min_temp, na.rm=TRUE)) %>%
    mutate(Date=unique(joined_grids$Date)) %>%
    mutate(year=year(Date)) %>%
    mutate(month=month(Date)) %>%
    mutate(day=yday(Date)) %>%
    rename(grid_id=aus_grids_grid_id)
  
  return(grids_summarized)
  
}

# list of files
file_list <- list.files("Data/min_temp_data_sf_RDS/")

# apply the function to every RDS file in the RDS file
results_list <- lapply(file_list, function(x){summarize_min_temp_data(x)})

df_of_results <- bind_rows(results_list)


df_of_results2<- df_of_results %>% 
  select(-sd_min_temp, -median_min_temp, -Number_of_min_temp_grids, -max_min_temp, -min_min_temp)


saveRDS(df_of_results2, "Data/RDS outs/min_temp_data_summarized.RDS")
