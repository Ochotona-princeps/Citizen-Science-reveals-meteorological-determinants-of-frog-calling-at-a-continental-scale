# assign frogID records to grids
##### 1. A script to read in FrogID data, clean it and then assign each observation to a grid_id #####

## Load packages
library(dplyr) 
library(readr)
library(ggplot2)
library(sf)
library(tidyr)

sf::sf_use_s2(FALSE)


## Read in FrogID data
## (Change the file name and destination to match your file)
FrogID <- readRDS("raw_frogid_data/FrogID_clean_data.RDS") %>%
  dplyr::filter(location_accuracy <= 3000) %>%
  dplyr::filter(location_accuracy >= 0) %>%
  mutate(date=as.Date(date)) %>%
  dplyr::filter(date <= "2020-11-30") %>%
  dplyr::filter(date >= "2017-11-10")

## spatial dataset
## (Change the file destination to match your file)
#load("Data/spatial_data/aus_grids.RData")
aus_grids<- readRDS("Data/spatial_data/grids_10km.RDS")

### Assign FrogID data to grids
## create metadata df
grids_metadata <- data.frame(grid_id=aus_grids[[1]]) %>%
  mutate(col.id=1:nrow(.))

## create points which are in sf format again
## use a different crs this time though
crs <- "+proj=longlat +datum=WGS84 +no_defs"

points_sf <- st_as_sf(FrogID, coords = c("lng", "lat"), 
                      crs = crs, agr = "constant")

## set crs for the grids
aus_grids <- aus_grids %>%
  st_set_crs(crs)

## As above, use intersect to assign each point to a polygon in the shapefile
assigned_points <- as.data.frame(st_intersects(points_sf, aus_grids))

# join the original dataframe with the intersected df
# then remove the row and col ids
FrogID_clean <- FrogID %>%
  mutate(row.id = 1:nrow(.)) %>%
  left_join(assigned_points, by="row.id") %>%
  right_join(., grids_metadata, by="col.id") %>%
  dplyr::select(-row.id, -col.id)

# remove any frog records which have location accuracy > than
# 3000
# this removes records that the phone could not properly assign
# filter out records with >3km accuracy
FrogID_clean_acc <- FrogID_clean %>%
  dplyr::filter(location_accuracy <=3000) %>%
  dplyr::filter(location_accuracy >= 0)


# read in the shapefile
# for the australian coast
# (Change the file destination to match yours)
aus_coastline <- st_read("Data/spatial_data/aust_cd66states.shp")

# create metadata df
# used to link the dfs back together later
coast_metadata <- data.frame(aus_coastline=aus_coastline[[1]],
                             col.id=1:8)

# convert points from FrogID into sf points
# so that they can be used with the polygons
crs <- "+proj=longlat +ellps=GRS80 +no_defs"

##alternate from Jodi to compare 
crs <- "+proj=longlat +ellps=WGS84 +no_defs"

grid_points_sf <- st_as_sf(aus_grids, coords = c("lon", "lat"), 
                           crs = crs, agr = "constant") %>%
  st_transform(crs)

st_crs(aus_coastline) <- crs

# use intersect to return a list of points and the associated
# polygon they belong to - uses integer ids for rows and columns
assigned_points <- as.data.frame(st_intersects(grid_points_sf, aus_coastline))

# join the original dataframe with the intersected df
# then remove the row and col ids
grids_overlaid <- aus_grids %>%
  mutate(row.id = 1:nrow(.)) %>%
  left_join(assigned_points, by="row.id") %>%
  left_join(., coast_metadata, by="col.id") %>%
  dplyr::select(-row.id, -col.id) %>%
  replace_na(list(aus_coastline=0))

# now we can get the grids that intersect with Australia
st_geometry(grids_overlaid) <- NULL


grids_in_aus <- grids_overlaid %>%
  dplyr::select(grid_id, aus_coastline) %>%
  dplyr::filter(aus_coastline != 0) %>%
  left_join(., FrogID_clean_acc, by="grid_id") %>%
  left_join(., aus_grids %>%
              rename(grid_lat=lat) %>%
              rename(grid_lng=lon), by="grid_id")


# add grid info to the clean dataset
grids_in_aus_distinct <- grids_in_aus %>%
  dplyr::filter(complete.cases(id)) %>%
  dplyr::select(id, grid_id, grid_lat, grid_lng, aus_coastline) %>%
  distinct() %>% 
  group_by(id) %>%
  top_n(., 1, grid_lat) %>% 
  top_n(., 1, grid_lng)  %>%   
  top_n(., 1, aus_coastline) 
# NOTE - some ids (e.g. 2894) have 2 grids assiged to it 
# b/c their location is on the border of 2 grids (??) - 
# so use the first grid that it is assigned to for now 
# (unless you can find a solution)

FrogID_with_grids <- left_join(FrogID_clean_acc, grids_in_aus_distinct) %>%
  dplyr::filter(complete.cases(aus_coastline))

# Save the output as a csv (Rename as anything you want)
write_csv(FrogID_with_grids, "Data/clean_frogid_data_with_grids/FrogID_griddata.csv")


# example plots
submissions_per_grid <- grids_in_aus %>%
  dplyr::group_by(grid_id) %>%
  summarise(N=n()) %>%
  left_join(., aus_grids, by="grid_id")

plasma_pal <- c(viridis::plasma(n = 10))

# an example plot
S<- ggplot(submissions_per_grid, aes(x=lon, y=lat))+
  geom_point(aes(color=log(N)), size=0.25)+
  theme_classic()+
  xlab("")+
  ylab("")+
  theme(axis.text=element_blank())+
  theme(axis.ticks=element_blank())+
  theme(axis.line=element_blank())+
  #scale_fill_gradient(low = "yellow", high = "red", na.value = NA)+
  scale_color_gradientn(colours = plasma_pal)+
  #scale_fill_viridis_c(option="plasma")+
  labs(color="Number of submissions (log)")

S + scale_color_viridis_c(option = "cividis")
S + scale_color_viridis_c()
S + scale_color_viridis_c(option = "inferno") 
#S + scale_color_viridis_c(high = "#132B43", low = "#56B1F7")
#S+ scale_fill_distiller(palette ="RdBu", direction = 1)
#S + scale_color_continuous(values = rev(RColorBrewer::brewer.pal(N,'Blues')))

S + scale_color_brewer(palette = "Greens")

# an example of a plot
# of the number of submissions per grid in Australia

ggplot()+
  geom_sf(data=aus_coastline)+
  geom_sf(data=submissions_per_grid$geometry, mapping=aes(fill=log(submissions_per_grid$N)))+
  scale_fill_gradientn(colours=plasma_pal)+
  theme_classic()+
  theme(axis.text=element_text(color="black"))+
  theme(panel.grid.major = element_line(colour = 'transparent'))+
  labs(fill="Number of submissions (log)")
