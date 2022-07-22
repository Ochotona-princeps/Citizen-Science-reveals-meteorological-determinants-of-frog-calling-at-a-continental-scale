#Violin plots
#to visualize variable significance across all species


### Just the basics or 
## all data to do spread + mean + SD 

library(dplyr)
library(purrr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(hrbrthemes)
library(viridis)
library(RColorBrewer)

setwd("Data/BRT_results")
#so this is unscaled summaries right now


# read in data
species_importance <- list.files(pattern = ".RDS") %>%
  map(readRDS) %>% 
  bind_rows()

### remove uninterested variables
### and scale the remaining from 0-1
data <- species_importance %>%
  dplyr::filter(!var %in% c("number_of_samples", "grid_lat", "grid_lng")) %>%
  group_by(Species) %>%
  mutate(rel.inf_scaled=scales::rescale(rel.inf))


####make day species list here 

day_species<- data[,-c(4:10)]
#
day_species <- data %>%
  dplyr::filter(var == "day")%>% 
  arrange(desc(rel.inf_scaled))
  
  print<- head(day_species, 67)

write_rds(print, "day_species_list.RDS")

p <- data %>%
  #arrange(predictor(desc)) %>%
  mutate(var = factor(var, levels=c("day", "max_temp_10_days", "min_temp_10_days",  "rain_10_days", "mean_vapour", "mean_max_temp", "mean_min_temp", "phase","rain_3_days", "mean_rainfall"))) %>%  # Reorder data
  ggplot( aes(x=var, y=rel.inf_scaled, fill=var, color=var)) +
  geom_violin(width=3, size=0.5) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(legend.position="none"
  )+
  #coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  xlab("") +
  ylab("Relative Influence")+
  stat_summary(
    geom = "point",
    fun = "mean",
    col = "red",
    size = 3,
    shape = 20,
    fill = "red"
  )

p

p+ theme(axis.text.x=element_text(angle=30, hjust=1))

##for details in paper


topdown<- data %>%
  group_by(var) %>%
  summarize(mean_PI = mean(rel.inf_scaled, na.rm = TRUE))





  
######
##now day only 



setwd("Data/BRT_day_results")

# read in data

species_importance <- list.files(pattern = ".RDS") %>%
  map(readRDS) %>% 
  bind_rows()



### remove uninterested variables
### and scale the remaining from 0-1
data <- species_importance %>%
  dplyr::filter(!var %in% c("number_of_samples", "grid_lat", "grid_lng")) %>%
  group_by(Species) %>%
  drop_na()%>%
  mutate(rel.inf_scaled=scales::rescale(rel.inf))



p <- data %>%
  #arrange(predictor(desc)) %>%
  mutate(var = factor(var, levels=c("max_temp_10_days", "min_temp_10_days",  "rain_10_days", "mean_vapour", "mean_max_temp", "mean_min_temp", "phase","rain_3_days", "mean_rainfall"))) %>%  # Reorder data
  ggplot( aes(x=var, y=rel.inf_scaled, fill=var, color=var)) +
  geom_violin(width=3, size=0.5) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(legend.position="none"
  )+
  #coord_flip() + # This switch X and Y axis and allows to get the horizontal version
  xlab("") +
  ylab("Relative Influence")+
  stat_summary(
    geom = "point",
    fun = "mean",
    col = "red",
    size = 3,
    shape = 20,
    fill = "red"
  )

p

p+ theme(axis.text.x=element_text(angle=30, hjust=1))






