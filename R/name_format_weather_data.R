files <- list.files("Data/max_temp_data/")




for (i in files) {
  
  file.rename(i, paste0("mx", substr(i, start = 1, stop = 8), ".txt"))
  
  
}
files

