#make heatmap of all speices interaction plots

library(dplyr)
library(tidyverse)
library(corrplot)
library(colorRamps)
library(tidyr)

#create a summary table by 
#removing species
# summarise(across(everything(), mean))
#for each variable 
#like this 
sum_table< - readRDS("sum_table.RDS")

#based on data here 
#setwd("Data/BRT_results/interactions_tables")

# read in data
#variable_interactions <- list.files(pattern = ".RDS") %>%
  #map(readRDS) %>% 
  #bind_rows()


#then pivot long to rescale by value 0-1 
sum_table<- sum_table[, c(11,1,2,3,4,5,6,7,8,9,10)]
rownames(sum_table) <- sum_table[,1]
sum_table <- sum_table[,-1]


long_int <- sum_table%>% 
  pivot_longer(
    cols = c('day', 'max_temp_10_days', 'mean_max_temp', 'min_temp_10_days', 
             'mean_min_temp', 'phase', 'rain_3_days','rain_10_days', 'mean_rainfall', 'mean_vapour'),
    names_to = "variables", 
    values_to = "value"
  )


##maybe scale this too? 
long_int_scaled <- long_int %>% 
  mutate(rel.int_scaled=scales::rescale(value))

long_int_scaled<- long_int_scaled[-c(2)]


plotdata<- pivot_wider(long_int_scaled, 
                       names_from = c(variables), # category column(s) to pivot from long to wide
                       values_from =c(rel.int_scaled), # value columns(s) that hold data for each category column
                       #names_sep ="_" # optional string separator for category-value columns
)



##TRY removing zeros 
#to see easier 
#long_int_scaled[long_int_scaled==0] <- NA

##


#or scaled version 
scaled_wide<- readRDS("summary_table_wide_form_scaled.RDS")


#still needs to be a data matrix?
plotdat<- data.matrix(scaled_wide)

corrplot(plotdat, method = 'color', order = 'alphabet', type = 'upper', diag = FALSE)







