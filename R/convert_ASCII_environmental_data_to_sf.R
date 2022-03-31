## This is an R script to
## convert the environmental data that we get from BOM
## which comes in ASCII format to individual RDSs containing
## sf objects, which will be easier to work with
## and aggregate up for analyses with the FrogID data
## multiple different types of environmental data will be eventually analyzed
## so we'll write this script as a function to pass whatever the environmental data
## is by name (e.g., rainfall_data)

# packages
library(dplyr)
library(rgdal)
library(raster)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)

## create a folder for the files to end up in before you run code (ex: Data/max_temp_data_sf_RDS)

# function to convert all daily environmental data
# into RDSs in sf format
# where you pass the function the folder name (e.g., "rainfall_data")
convert_ASCII_to_sf_RDS_files <- function(folder_name){
  
  # get the file location
  file.locs <- paste0("Data/", folder_name, "/")
  
  # get the list of files
  files <- list.files(file.locs)
  
  # get start date and end date
  # this works because the files are ordered by date
  # at least for now, but just ensure this stays true
  # but because there are multiple types of environmental data
  # the prefixes are different
  # so will code this in to switch the 'r' to 'mi' for min_temp and 'ma' for max_temp
  # for example
  date_start <- if (folder_name=="rainfall_data"){
    gsub("r", "", gsub(".txt", "", files[1]))
  } else {
    if (folder_name=="min_temp_data"){
      gsub("mi", "", gsub(".txt", "", files[1]))
    } else {
      if (folder_name=="9am_vapour_pressure"){
        gsub("va", "", gsub(".txt", "", files[1]))
      } else{
        gsub("mx", "", gsub(".txt", "", files[1]))
      }
    }
  }
  
  date_end <- if (folder_name=="rainfall_data"){
    gsub("r", "", gsub(".txt", "", files[length(files)]))
  } else {
    if (folder_name=="min_temp_data"){
      gsub("mi", "", gsub(".txt", "", files[length(files)]))
    } else {
      if (folder_name=="9am_vapour_pressure"){
        gsub("va", "", gsub(".txt", "", files[length(files)]))
      } else{
        gsub("mx", "", gsub(".txt", "", files[length(files)]))
      }
    }
  }
  
  # create a list of dates
  dates <- seq.Date(as.Date(as.character(date_start), format="%Y%m%d"), 
                    as.Date(as.character(date_end), format="%Y%m%d"), by=1)
  
  # create a 'prefix'
  # for each of the potential folder names
  prefix <- if (folder_name=="rainfall_data"){
    "r"
  } else {
    if (folder_name=="min_temp_data"){
      "mi"
    } else {
      if (folder_name=="9am_vapour_pressure"){
        "va"
      } else{
        "mx"
      }
    }
  }
  # now get a list of files to read in
  # because we only want to read in the .txt files
  files_to_read <- paste0(file.locs, prefix, as.character(format(dates,"%Y%m%d")),".txt")
  
  # create a raster stack
  stack <- raster:::.quickStack(files_to_read)
  
  # now use a forloop
  # to loop through every layer in the raster stack
  # and convert to an sf object
  # but will do this in a nested if_else statement
  # to accomodate for the different folder names potentially used
  if (folder_name=="rainfall_data"){
    for (i in 1:nlayers(stack)) {
      
      date <- as.Date(as.character(gsub("r", "", stack[[i]]@data@names)), format="%Y%m%d")
      
      message(paste0("Analyzing ", date))
      
      sf_dat <- as(stack[[i]],'SpatialPolygonsDataFrame') %>%
        st_as_sf() %>%
        rename(rain_value=paste0(stack[[i]]@data@names)) %>%
        mutate(Date=date)
      
      saveRDS(sf_dat, file=paste0("Data/", folder_name, "_sf_RDS/", as.character(date), ".RDS"))
    }
  } else {
    if (folder_name=="min_temp_data"){
      for (i in 1:nlayers(stack)) {
        
        date <- as.Date(as.character(gsub("mi", "", stack[[i]]@data@names)), format="%Y%m%d")
        
        message(paste0("Analyzing ", date))
        
        sf_dat <- as(stack[[i]],'SpatialPolygonsDataFrame') %>%
          st_as_sf() %>%
          rename(min_temp=paste0(stack[[i]]@data@names)) %>%
          mutate(Date=date)
        
        saveRDS(sf_dat, file=paste0("Data/", folder_name, "_sf_RDS/", as.character(date), ".RDS"))
      }
    } else {
      if (folder_name=="9am_vapour_pressure"){
        for (i in 1:nlayers(stack)) {
          
          date <- as.Date(as.character(gsub("va", "", stack[[i]]@data@names)), format="%Y%m%d")
          
          message(paste0("Analyzing ", date))
          
          sf_dat <- as(stack[[i]],'SpatialPolygonsDataFrame') %>%
            st_as_sf() %>%
            rename(vapour=paste0(stack[[i]]@data@names)) %>%
            mutate(Date=date)
          
          saveRDS(sf_dat, file=paste0("Data/", folder_name, "_sf_RDS/", as.character(date), ".RDS"))
        }
      } else{
        for (i in 1:nlayers(stack)) {
          
          date <- as.Date(as.character(gsub("mx", "", stack[[i]]@data@names)), format="%Y%m%d")
          
          message(paste0("Analyzing ", date))
          
          sf_dat <- as(stack[[i]],'SpatialPolygonsDataFrame') %>%
            st_as_sf() %>%
            rename(max_temp=paste0(stack[[i]]@data@names)) %>%
            mutate(Date=date)
          
          saveRDS(sf_dat, file=paste0("Data/", folder_name, "_sf_RDS/", as.character(date), ".RDS"))
        }
      }
    }
  } 
}
  



#convert_ASCII_to_sf_RDS_files("rainfall_data")
#FYI each one takes a full 26 hurs to run

#convert_ASCII_to_sf_RDS_files("min_temp_data")

#convert_ASCII_to_sf_RDS_files("max_temp_data")

convert_ASCII_to_sf_RDS_files("9am_vapour_pressure")

### check to see if it makes sense by opening some files 
temptest <- readRDS("Data/9am_vapour_pressure_sf_RDS/2017-11-01.RDS")

