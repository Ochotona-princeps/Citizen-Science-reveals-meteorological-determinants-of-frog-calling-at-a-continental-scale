#Functionl code to create a column displaying the average
#temp over the past 10 days 
## *do this before modeling for a species when you have all the data 

library(dplyr)
library(tidyr)
library(parallel)
library(zoo)
mc.cores = 4

max_temp <- readRDS("Data/RDS outs/max_temp_data_summarized.RDS") %>%
  unite(year_day, year, day, sep="_", remove=FALSE)

test<-head(max_temp,5000)



###
mean_10_day_function <- function(temp_dat, chunk_number){
  
  grid_list <- temp_dat %>%
    dplyr::filter(complete.cases(grid_id)) %>%
    dplyr::select(grid_id) %>%
    distinct() %>%
    .$grid_id
  
  apply_to_grid <- function(id_of_grid){
    
    message(paste0("Analyzing grid # ", id_of_grid))
    
    dat <- temp_dat %>%
      dplyr::filter(grid_id==id_of_grid)
    
    n <- 10
    
    #cs <- ave(dat$mean_max_temp)
        #dat$temp_10_days <- c(rep_len(NA, n - 1), tail(cs, -(n - 1)) - c(0, head(cs, -n)))
    #dat$temp_10_days <- c(rep(NA, n - 1), tail(cs, -(n - 1)) - c(0, head(cs, -n)))
    
    # cs <- ave(dat$mean_max_temp)
    # dat$temp_10_days <- c(rep_len(NA, n - 1), tail(cs, -(n - 1)) - c(0, head(cs, -n)))
    dat$temp_10_days = zoo::rollmean(dat$mean_max_temp, n, fill=NA, align="right")
    
    return(dat)
    
  }
  
  results <- lapply(grid_list, apply_to_grid)
  
  results <- bind_rows(results)
  
  saveRDS(results, paste0("Data/chunked_results/", chunk_number, ".RDS"))
  
}

# manually create chunks of the data
# based on grid_id
grid_list <- max_temp %>%
  dplyr::filter(complete.cases(grid_id)) %>%
  dplyr::select(grid_id) %>%
  distinct() %>%
  .$grid_id


# will do chunks of about 10,000 grids at a time
# and save these out
# so we don't lose the progress

chunk_1 <- max_temp %>%
  dplyr::filter(grid_id %in% grid_list[1:10000])
chunk_2 <- max_temp %>%
  dplyr::filter(grid_id %in% grid_list[10001:20000])
chunk_3 <- max_temp %>%
  dplyr::filter(grid_id %in% grid_list[20001:30000])
chunk_4 <- max_temp %>%
  dplyr::filter(grid_id %in% grid_list[30001:40000])
chunk_5 <- max_temp %>%
  dplyr::filter(grid_id %in% grid_list[40001:50000])
chunk_6 <- max_temp %>%
  dplyr::filter(grid_id %in% grid_list[50001:60000])
chunk_7 <- max_temp %>%
  dplyr::filter(grid_id %in% grid_list[60001:70000])
chunk_8 <- max_temp %>%
  dplyr::filter(grid_id %in% grid_list[70001:80911])

# now manually do this
# for each one
# which should help us achieve our goal
# and maybe avoid getting to much memory overload in R
mean_10_day_function(chunk_1, "chunk_1")
mean_10_day_function(chunk_2, "chunk_2")
mean_10_day_function(chunk_3, "chunk_3")
mean_10_day_function(chunk_4, "chunk_4")
mean_10_day_function(chunk_5, "chunk_5")
mean_10_day_function(chunk_6, "chunk_6")
mean_10_day_function(chunk_7, "chunk_7")
mean_10_day_function(chunk_8, "chunk_8")








#Repeat for min temp

library(dplyr)
library(tidyr)
library(parallel)

min_temp <- readRDS("Data/RDS outs/min_temp_data_summarized.RDS") %>%
  unite(year_day, year, day, sep="_", remove=FALSE)


mean_10_day_function <- function(temp_dat, chunk_number){
  
  grid_list <- temp_dat %>%
    dplyr::filter(complete.cases(grid_id)) %>%
    dplyr::select(grid_id) %>%
    distinct() %>%
    .$grid_id
  
  apply_to_grid <- function(id_of_grid){
    
    message(paste0("Analyzing grid # ", id_of_grid))
    
    dat <- temp_dat %>%
      dplyr::filter(grid_id==id_of_grid)
    
    n <- 10
    
    #cs <- ave(dat$mean_max_temp)
    #dat$temp_10_days <- c(rep_len(NA, n - 1), tail(cs, -(n - 1)) - c(0, head(cs, -n)))
    #dat$temp_10_days <- c(rep(NA, n - 1), tail(cs, -(n - 1)) - c(0, head(cs, -n)))
    
    # cs <- ave(dat$mean_max_temp)
    # dat$temp_10_days <- c(rep_len(NA, n - 1), tail(cs, -(n - 1)) - c(0, head(cs, -n)))
    dat$temp_10_days = zoo::rollmean(dat$mean_min_temp, n, fill=NA, align="right")
    
    return(dat)
    
  }
  
  results <- lapply(grid_list, apply_to_grid)
  
  results <- bind_rows(results)
  
  saveRDS(results, paste0("Data/chunked_results/", chunk_number, ".RDS"))
  
}
# manually create chunks of the data
# based on grid_id
grid_list <- min_temp %>%
  dplyr::filter(complete.cases(grid_id)) %>%
  dplyr::select(grid_id) %>%
  distinct() %>%
  .$grid_id


# will do chunks of about 10,000 grids at a time
# and save these out
# so we don't lose the progress
chunk_1 <- min_temp %>%
  dplyr::filter(grid_id %in% grid_list[1:10000])
chunk_2 <- min_temp %>%
  dplyr::filter(grid_id %in% grid_list[10001:20000])
chunk_3 <- min_temp %>%
  dplyr::filter(grid_id %in% grid_list[20001:30000])
chunk_4 <- min_temp %>%
  dplyr::filter(grid_id %in% grid_list[30001:40000])
chunk_5 <- min_temp %>%
  dplyr::filter(grid_id %in% grid_list[40001:50000])
chunk_6 <- min_temp %>%
  dplyr::filter(grid_id %in% grid_list[50001:60000])
chunk_7 <- min_temp %>%
  dplyr::filter(grid_id %in% grid_list[60001:70000])
chunk_8 <- min_temp %>%
  dplyr::filter(grid_id %in% grid_list[70001:80911])

# now manually do this
# for each one
# which should help us achieve our goal
# and maybe avoid getting to much memory overload in R
mean_10_day_function(chunk_1, "chunk_1")
mean_10_day_function(chunk_2, "chunk_2")
mean_10_day_function(chunk_3, "chunk_3")
mean_10_day_function(chunk_4, "chunk_4")
mean_10_day_function(chunk_5, "chunk_5")
mean_10_day_function(chunk_6, "chunk_6")
mean_10_day_function(chunk_7, "chunk_7")
mean_10_day_function(chunk_8, "chunk_8")







##Below is to do it all in one go
##but it's too big for Katana

library(dplyr)
library(tidyr)
library(parallel)

max_temp <- readRDS("Data/RDS outs/max_temp_data_summarized.RDS") %>%
  unite(year_day, year, day, sep="_", remove=FALSE)


ave_10_day_function <- function(id_of_grid){
  
  dat <- max_temp %>%
    dplyr::filter(grid_id==id_of_grid)
  
  n <- 10
  
  cs <- ave(dat$mean_max_temp)
  
  dat$temp_10_days <- c(rep_len(NA, n - 1), tail(cs, -(n - 1)) - c(0, head(cs, -n)))
  
  return(dat)
}


grid_list <- max_temp %>%
  dplyr::filter(complete.cases(grid_id)) %>%
  dplyr::select(grid_id) %>%
  distinct() %>%
  .$grid_id

temp.3 <- bind_rows(lapply(grid_list, ave_10_day_function))

saveRDS(temp.3,"Data/RDS outs/max_temp_with_10.RDS")



####Do it again for min temp JIC

## *do this before modeling for a species when you have all the data 


library(dplyr)
library(tidyr)
library(parallel)

mc.cores =4

min_temp <- readRDS("Data/RDS outs/min_temp_data_summarized.RDS") %>%
  unite(year_day, year, day, sep="_", remove=FALSE)

min_temp<- head(min_temp, 5000)


ave_10_day_function <- function(id_of_grid){
  
  dat <- min_temp %>%
    dplyr::filter(grid_id==id_of_grid)
  
  n <- 10
  
  cs <- ave(dat$mean_min_temp)
  
  dat$temp_10_days <- c(rep_len(NA, n - 1), tail(cs, -(n - 1)) - c(0, head(cs, -n)))
  
  return(dat)
}


grid_list <- min_temp %>%
  dplyr::filter(complete.cases(grid_id)) %>%
  dplyr::select(grid_id) %>%
  distinct() %>%
  .$grid_id

temp.4 <- bind_rows(lapply(grid_list,(as.list(ave_10_day_function)))

LsaveRDS(temp.4,"Data/RDS outs/min_temp_with_10.RDS")




