##Functionl code to add cumulative rainfall columns
## *do this before modeling for a species when you have all the data 

library(dplyr)
library(tidyr)
library(parallel)

rainfall <- readRDS("Data/RDS outs/rainfall_data_summarized.RDS") %>%
  unite(year_day, year, day, sep="_", remove=FALSE)
  
 
cumsum_3_day_function <- function(id_of_grid){
  
  dat <- rainfall %>%
    dplyr::filter(grid_id==id_of_grid)
  
  n <- 3
  
  cs <- cumsum(dat$mean_rainfall)
  
  dat$rain_3_days <- c(rep_len(NA, n - 1), tail(cs, -(n - 1)) - c(0, head(cs, -n)))
  
  return(dat)
}

grid_list <- rainfall %>%
  dplyr::filter(complete.cases(grid_id)) %>%
  dplyr::select(grid_id) %>%
  distinct() %>%
  .$grid_id

mccores=16

t1<-Sys.time()
rainfall.2 <- bind_rows(mclapply(grid_list[1:100], cumsum_3_day_function))
t2<-Sys.time()
t2-t1

#so 2 minutes per 100 without mccores
# nd 4 minutes with 1 mccore
#and 2 minutes with 4 mccores
# and 2 minutes with 16 cores
#so 1k in 20 minutes
#10 k in 200 minutes
#100k in 2,000
cumsum_10_day_function <- function(id_of_grid){
  
  dat <- rainfall.2 %>%
    dplyr::filter(grid_id==id_of_grid)
  
  n <- 10
  
  cs <- cumsum(dat$mean_rainfall)
  
  dat$rain_10_days <- c(rep_len(NA, n - 1), tail(cs, -(n - 1)) - c(0, head(cs, -n)))
  
  return(dat)
}

rainfall.3 <- bind_rows(lapply(grid_list, cumsum_10_day_function))


saveRDS(rainfall.3,"Data/RDS outs/rainfall_with_cumulative.RDS")
