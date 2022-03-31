## A script to read in environmental data from the sf format
## and then aggregate to the 'grids' used in the analysis for FrogID

# packages
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)

# read in aus grids
# and set crs
load("Data/spatial_data/aus_grids.RData")
st_crs(aus_grids) <- 4326

###Maureen's hashtag what does this CRs do? 

# write a function that takes every RDS
# in a folder, which incorporates the rain data
# and summarizes it into the FrogID grids


summarize_vapour_data <- function(x){
  
  message(paste0("Analyzing ", x))
  
  vapour_dat <- readRDS(paste0("Data/9am_vapour_pressure_sf_RDS/", x))
  
  st_crs(vapour_dat) <- 4326
  
  vapour_dat <- vapour_dat %>%
    st_transform(crs=4326) %>%
    mutate(row.id=1:nrow(.))
  
  joined_grids <- vapour_dat %>%
    st_intersects(aus_grids) %>%
    as.data.frame() %>%
    group_by(row.id) %>%
    slice(1) %>%
    right_join(., vapour_dat %>%
                 st_set_geometry(NULL), by="row.id") %>%
    rename(vapour_grid_id=row.id) %>%
    rename(aus_grids_grid_id=col.id)
  
  grids_summarized <- joined_grids %>%
    group_by(aus_grids_grid_id) %>%
    summarize(Number_of_vapour_grids=n(),
              mean_vapour=mean(vapour, na.rm=TRUE),
              median_vapour=median(vapour, na.rm=TRUE),
              sd_vapour=sd(vapour, na.rm=TRUE),
              min_vapour=min(vapour, na.rm=TRUE),
              max_vapour=max(vapour, na.rm=TRUE)) %>%
    mutate(Date=unique(joined_grids$Date)) %>%
    mutate(year=year(Date)) %>%
    mutate(month=month(Date)) %>%
    mutate(day=yday(Date)) %>%
    rename(grid_id=aus_grids_grid_id)
  
  return(grids_summarized)
  
}

# list of files
file_list <- list.files("Data/9am_vapour_pressure_sf_RDS/")

# apply the function to every RDS file in the RDS file
results_list <- lapply(file_list, function(x){summarize_vapour_data(x)})

df_of_results <- bind_rows(results_list)

df_of_results2<- df_of_results %>% 
  select(-sd_vapour, -median_vapour, -Number_of_vapour_grids, -max_vapour, -min_vapour)

saveRDS(df_of_results2, "Data/RDS outs/vapour_data_summarized.RDS")

