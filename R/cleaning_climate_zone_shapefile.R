# this script is used to clean up the climate zone map (http://www.bom.gov.au/iwk/climate_zones/map_1.shtml)
# which was downloaded from here: http://www.bom.gov.au/jsp/ncc/climate_averages/climate-classifications/index.jsp?maptype=kpngrp#maps
# more information on it is here: http://www.bom.gov.au/climate/averages/climatology/gridded-data-info/metadata/md_koppen_classification.shtml
# I first used ArcMap to read in the downloaded ASCII file and convert to a shapefile
# and also trimmed to only mainland Australia
# here I clean up the fields and also add a key which wasn't present before
# because we are mostly interested in the upper-level climate zone classification
# I then write the file out as climate_zones_clean.shp and climate_zones_clean.geojson

# packages
library(sf)
library(dplyr)
library(parallel)

mc.cores= 4

climate_zones <- st_read("Data/spatial_data/climate_zones/climate_zones.shp")

# metadata table
metadata <- data.frame(gridcode=c(1:9, 11:15, 21:24, 31:37, 41:42), 
                       subdivision_name=c("no dry season (cool summer)",
                                          "distinctly dry (and mild) summer",
                                          "no dry season (mild summer)",
                                          "distinctly dry (and warm) summer",
                                          "moderately dry winter (warm summer)",
                                          "no dry season (warm summer)",
                                          "distinctly dry (and hot) summer",
                                          "moderately dry winter (hot summer)",
                                          "no dry season (hot summer)",
                                          "warm (summer drought)",
                                          "warm (persistently dry)",
                                          "hot (winter drought)",
                                          "hot (summer drought)",
                                          "hot (persistently dry)",
                                          "warm (persistently dry)",
                                          "hot (winter drought)",
                                          "hot (summer drought)",
                                          "hot (persistently dry)",
                                          "moderately dry winter",
                                          "distinctly dry winter", 
                                          "distinctly dry summer",
                                          "no dry season",
                                          "savanna", 
                                          "rainforest (monsoonal)",
                                          "rainforest (persistently wet)",
                                          "savanna", 
                                          "rainforest (monsoonal)"),
                       climate_zone=c("Temperate", "Temperate", "Temperate", "Temperate", "Temperate",
                                      "Temperate", "Temperate", "Temperate", "Temperate", "Grassland",
                                      "Grassland", "Grassland", "Grassland", "Grassland", "Desert",
                                      "Desert", "Desert", "Desert", "Subtropical", "Subtropical",
                                      "Subtropical", "Subtropical", "Tropical", "Tropical", "Tropical",
                                      "Equatorial", "Equatorial"))

climate_zones.2 <- climate_zones %>%
  dplyr::select(gridcode) %>%
  left_join(., metadata, by="gridcode")

st_write(climate_zones.2, "Data/spatial_data/climate_zones/climate_zones_clean.shp")
st_write(climate_zones.2, "Data/spatial_data/climate_zones/climate_zones_clean.geojson")

