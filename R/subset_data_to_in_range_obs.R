##### 2. A script to identify observations that are in the species' range #####

# Load packages
library(readr)
library(sp)
library(rgdal)
library(tidyr)
library(dplyr)
library(parallel)

mc.cores = 4

# Read in the cleaned frog data 
data <- read_csv("Data/clean_frogid_data_with_grids/FrogID_griddata.csv")
data<- data [-c(6,13:21)]
# Add an index for each row in the database
data$row_id <- 1:nrow(data)

# Create a function to return all observations within a species' range 
in_range_species <- function(species_name) {
  
  obs <- data %>%
    mutate(species = gsub(" ", "_", .$species)) %>% 
    filter(species == species_name)
  #replace spaces in species' names with underscores
  
  obs_2 <- SpatialPointsDataFrame(obs[,c("lng", "lat")], obs[,]) 
  #changed obs[,] from Corey's original code
  
  load(paste0("Data/frog_distribution_maps/RData/", species_name, ".RData")) #load in the range maps
  
  polygons <- SpatialPolygons(range_map@polygons)
  
  proj4string(polygons) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  
  proj4string(obs_2) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  
  outliers <- data.frame(join=over(obs_2, polygons))
  
  obs$join <- outliers$join
  
  in_range_id <- obs %>%
    replace_na(list(join="out")) %>% #NA values are identified as outliers
    filter(join != "out") %>% #filter out the outliers
    select(row_id) %>%
    .$row_id
  
  in_range_data <- data %>%
    filter(row_id %in% in_range_id)
  
  return(in_range_data)
  
}


## Test the function (Try with any species)
test <- in_range_species("Crinia_glauerti")

## Run the function for each species in the data
species_list <- unique(data$species)
species_list <- gsub(" ", "_", species_list)

### create empty list vector
inliers <- vector("list", length(species_list))

for (i in 1:length(species_list)) {
  
  inliers[[i]] <- in_range_species(species_list[i])
  
}

in.df <- do.call(rbind.data.frame, inliers) %>%
  dplyr::select(-row_id)

# Save the output as a csv (Rename as anything you want)
write_csv(in.df, "Data/clean_frogid_data_with_grids/in_range_observations.csv")


