## recombine combine the weather data that wont run in one go 

# packages
library(sf)
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)

first <- readRDS("Data/chunked_results/max_temp/chunk_1.RDS")

second <- readRDS("Data/chunked_results/max_temp/chunk_2.RDS")

third <-readRDS("Data/chunked_results/max_temp/chunk_3.RDS")
fourth <-readRDS("Data/chunked_results/max_temp/chunk_4.RDS")
fifth <-readRDS("Data/chunked_results/max_temp/chunk_5.RDS")
sixth <-readRDS("Data/chunked_results/max_temp/chunk_6.RDS")
seventh <-readRDS("Data/chunked_results/max_temp/chunk_7.RDS")
eigth <-readRDS("Data/chunked_results/max_temp/chunk_8.RDS")


start <- rbind(first, second)

nest<- rbind(start,third)

word<- rbind(nest, fourth)

again<- rbind(word, fifth)

over<- rbind(again, sixth)

work<- rbind(over, seventh)

final<- rbind(work, eigth)


write_rds(final, "Data/RDS outs/max_temp_with_mean10.rds")








##min temp now 

first <- readRDS("Data/chunked_results/chunk_1.RDS")

second <- readRDS("Data/chunked_results/chunk_2.RDS")

third <-readRDS("Data/chunked_results/chunk_3.RDS")
fourth <-readRDS("Data/chunked_results/chunk_4.RDS")
fifth <-readRDS("Data/chunked_results/chunk_5.RDS")
sixth <-readRDS("Data/chunked_results/chunk_6.RDS")
seventh <-readRDS("Data/chunked_results/chunk_7.RDS")
eigth <-readRDS("Data/chunked_results/chunk_8.RDS")


start <- rbind(first, second)

nest<- rbind(start,third)

word<- rbind(nest, fourth)

again<- rbind(word, fifth)

over<- rbind(again, sixth)

work<- rbind(over, seventh)

final<- rbind(work, eigth)


write_rds(final, "Data/RDS outs/min_temp_with_mean10.rds")

