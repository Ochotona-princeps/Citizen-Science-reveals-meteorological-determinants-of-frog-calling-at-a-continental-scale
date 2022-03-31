##### 3. A script to get presence/absence of species x within species x's range #####

# packages
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(sp)
library(parallel)

mc.cores=4

# Read in the file (that you made in section 1) containing all recordings 
# that were made within each species' known distribution
data <- readRDS("Data/clean_frogid_data_with_grids/in_range_observations_with_moon_sun.RDS")

unique_submissions <- data %>%
  dplyr::select(lng, lat, id) %>%
  distinct()


## Function to make a dataframe for a species containing
## presence/absence for each recording - need 1 row for each call id
## within that species' range
get_dat_for_models <- function(species_name) {
  
  presence <- data %>%
    filter(species == gsub("_", " ", species_name)) %>%
    mutate(present=1)
  
  spatial_points <- unique_submissions %>%
    dplyr::filter(!id %in% presence$id)
  
  spatial_df <- SpatialPointsDataFrame(spatial_points[,c("lng", "lat")], spatial_points[,]) 

  
  load(paste0("Data/frog_distribution_maps/RData/", species_name, ".RData")) #load in the range maps
  
  polygons <- SpatialPolygons(range_map@polygons)
  
  proj4string(polygons) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  
  proj4string(spatial_df) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  
  outliers <- data.frame(join=over(spatial_df, polygons))
  
  spatial_points$join <- outliers$join
  
  in_range_id <- spatial_points %>%
    replace_na(list(join="out")) %>% #NA values are identified as outliers
    filter(join != "out")
  
  in_range_absences <- data %>%
    dplyr::filter(id %in% in_range_id$id) %>%
    distinct() %>%
    mutate(present=0)
  
  final_dat <- bind_rows(presence,
                         in_range_absences) %>%
    mutate(species=gsub("_", " ", species_name))
  
  saveRDS(final_dat, paste0("Data/species_range_model_data/", species_name, ".RDS"))
  
  
}

# get list of species
## Run the function for each species in the data
species_list <- unique(data$species)
species_list <- gsub(" ", "_", species_list)

lapply(species_list, function(x){get_dat_for_models(x)})






