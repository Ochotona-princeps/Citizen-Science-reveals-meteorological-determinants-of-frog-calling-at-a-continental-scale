# play K-means clustering

library(dplyr)
library(purrr)
library(ggplot2)
library(gridExtra)
library (parallel)
library(RColorBrewer)

mc.cores= 4

# set wd to folder where RDS results are stored
setwd("Data/day_species_model_results/pi_wo_day")

# read in data
species_importance <- list.files(pattern = ".RDS") %>%
  map(readRDS) %>% 
  bind_rows()

write.csv(species_importance, "day_spp_kmeans_inputs092321.csv")

species_importance<- species_importance %>%
  filter(Species != "Cophixalus hosmeri")

topdownday<- species_importance %>%
  group_by(predictor) %>%
  summarize(mean_PI = mean(importance, na.rm = TRUE))

###figure out color codes so both graphs match 
day ="#1B9E77" 
mean_max_temp ="#D95F02" 
mean_min_temp ="#7570B3" 
mean_vapour = #E7298A" 
rain_10 = "#66A61E" 
rain_3 = "#E6AB02" 
rainfall = "#A6761D" 
moonphase ="#666666"


# summarizing across all species
ggplot(species_importance, aes(x=predictor, y=importance, color=predictor))+
  geom_point(alpha=0.8)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  coord_flip()+
  scale_color_brewer(palette = "Dark2" )+ 
  guides(color=FALSE)+
  xlab("")+
  ylab("Scaled importance")

# one potential way to visualize species-specific results
length(unique(species_importance$Species))

#ggplot(species_importance, aes(x=Species, y=importance, color=predictor))+
  aes(x = reorder(Species, desc(Species)))+
  geom_point()+
  geom_point(size = 2.5)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  theme(axis.text.y = element_text(face="italic"))+
  #scale_fill_brewer("Dark2")+ 
  #scale_color_brewer(palette = "Dark2" )+ 
  coord_flip()+
  xlab("")+
  ylab("Scaled importance")+
  scale_color_manual (name = "predictor", 
                      values =c ("mean_min_temp" = "#D95F02",
                        "mean_max_temp" = "#7570B3", 
                      "rain_10_days" = #66A61E", 
                        "mean_vapour" = #E7298A", 
                        "rain_3_days" = #E6AB02",
                        "phase" = #666666",
                        "mean_rainfall" = #A6761D"
                        ))
  
species_importance$predictor <- factor(species_importance$predictor, levels = c("mean_min_temp", "mean_max_temp", "rain_10_days", "mean_vapour","rain_3_days","phase","mean_rainfall"))

ggplot(species_importance, aes(x=Species, y=importance, color=predictor))+
  aes(x = reorder(Species, desc(Species)))+
  geom_point()+
  geom_point(size = 2.5)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  theme(axis.text.y = element_text(face="italic"))+
  #scale_fill_brewer("Dark2")+ 
  scale_color_brewer(palette = "Dark2")+ 
  coord_flip()+
  xlab("")+
  ylab("Scaled importance")

#colour = "#7570B3", "#D95F02", "#66A61E", "#E7298A", "#E6AB02", "#666666", "#A6761D"  
#scale_color_manual (name = "predictor", 
                      values =c ("mean_min_temp" = "#7570B3",
                                   "mean_max_temp" = "#D95F02", 
                                 "rain_10_days" = "#66A61E", 
                                   "mean_vapour" = "#E7298A", 
                                   "rain_3_days" = "#E6AB02",
                                   "phase" = "#666666",
                                   "mean_rainfall" = "#A6761D"
                      ))
 

# this will get difficult though with too many species
library(tidyverse)
library(gridExtra)

#first regroup our data so all the variables associate with a species in one line
regroup <- species_importance %>% 
  filter(predictor == "rain_10_days")

regroup$Im_rain_10_days<-regroup$importance

#x<-regroup %>%
  #left_join(select(rain10days,Species, Im_rain_10_days), by = "Species")

meanrain <- species_importance %>% 
  filter(predictor == "mean_rainfall")

meanrain$Im_mean_rain<-meanrain$importance

rain3days <- species_importance %>% 
  filter(predictor == "rain_3_days")

rain3days$Im_rain_3_days<-rain3days$importance

phase <- species_importance %>% 
  filter(predictor == "phase")

phase$Im_phase<-phase$importance

mintemp <- species_importance %>% 
  filter(predictor == "mean_min_temp")

mintemp$Im_min_temp<-mintemp$importance

maxtemp <- species_importance %>% 
  filter(predictor == "mean_max_temp")

maxtemp$Im_max_temp<-maxtemp$importance


vapour <- species_importance %>% 
  filter(predictor == "mean_vapour")

vapour$Im_vapour<-vapour$importance


x<-meanrain %>%
  left_join(dplyr::select(regroup,Species, Im_rain_10_days), by = "Species")%>%
  left_join(dplyr::select(phase,Species, Im_phase), by = "Species")%>%
  left_join(dplyr::select(mintemp,Species, Im_min_temp), by = "Species")%>%
  left_join(dplyr::select(maxtemp,Species, Im_max_temp), by = "Species")%>%
  left_join(dplyr::select(rain3days,Species, Im_rain_3_days), by = "Species")%>%
  left_join(dplyr::select(vapour,Species, Im_vapour), by = "Species")

x$Im_rain_10_days<-x$importance

x<-x[-c(1,2)]



plot2 <- x%>% 
  ggplot(aes(x = "species", y = Im_phase)) + 
  geom_jitter(width = .025, size = 2, alpha = .3, color = "green") +
  labs(x = "", y="importance of moonphase")

plot3 <- x%>% 
  ggplot(aes(x = "species", y = Im_min_temp)) + 
  geom_jitter(width = .025, size = 2, alpha = .3, color = "pink") +
  labs(x = "", y="importance of minimum temperature")

plot4 <- x%>% 
  ggplot(aes(x = "species", y = Im_max_temp)) + 
  geom_jitter(width = .025, size = 2, alpha = .3, color = "purple") +
  labs(x = "", y="importance of maximum temperature")

plot5 <- x%>% 
  ggplot(aes(x = "species", y = Im_mean_rain)) + 
  geom_jitter(width = .025, size = 2, alpha = .3, color = "navy") +
  labs(x = "", y="importance of daily rain")

plot6 <- x%>% 
  ggplot(aes(x = "species", y = Im_rain_3_days)) + 
  geom_jitter(width = .025, size = 2, alpha = .3, color = "blue") +
  labs(x = "", y="importance of previous 3 days rain")

plot7 <- x%>% 
  ggplot(aes(x = "species", y = Im_rain_10_days)) + 
  geom_jitter(width = .025, size = 2, alpha = .3, color = "turquoise") +
  labs(x = "", y="importance of previous 10 days rain")

plot8 <- x%>% 
  ggplot(aes(x = "species", y = Im_vapour)) + 
  geom_jitter(width = .025, size = 2, alpha = .3, color = "red") +
  labs(x = "", y="importance of vapour pressure")

grid.arrange(plot2, plot3, plot4, plot5, plot6, plot7, plot8)


# As the initial centroids are defined randomly,
# we define a seed for purposes of reprodutability
set.seed(123)

# Let's remove the column with the species' names, so it won't be used in the clustering
input <- x[-c(1)]

# The nstart parameter indicates that we want the algorithm to be executed 20 times.
# This number is not the number of iterations, it is like calling the function 20 times and then
# the execution with lower variance within the groups will be selected as the final result.
kmeans(input, centers = 3, nstart = 20)

#' Plots a chart showing the sum of squares within a group for each execution of the kmeans algorithm. 
#' In each execution the number of the initial groups increases by one up to the maximum number of centers passed as argument.
#'
#' @param data The dataframe to perform the kmeans 
#' @param nc The maximum number of initial centers
#'
wssplot <- function(data, nc=15, seed=123){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of groups",
       ylab="Sum of squares within a group")}

wssplot(input, nc = 20)

#reran 

set.seed(123)
clustering <- kmeans(input, centers = 5, nstart = 20)
clustering

library(cluster)
library(factoextra)

sil <- silhouette(clustering$cluster, dist(input))
fviz_silhouette(sil)

#The silhouette plot below gives us evidence that our clustering using four groups is just okay because 
#there are negative silhouette widths and most of the values are between 0 and 0.50 
#better if above 0.5 

library(GGally)
library(plotly)

x$cluster <- as.factor(clustering$cluster)


p <- ggparcoord(data = x, columns = c(2,3,4,5,6,7,8), groupColumn = "cluster", scale = "globalminmax") + labs(x = "predictors of calling", y = "importance", title = "Clustering")

ggplotly(p)


## now to try identifying the group members

x$cluster

group1 <- x %>% 
  filter(cluster == "1")
write_csv(group1,"Data/K_means_outs/K_means_no_day/group1.csv")

group1$cluster <- as.factor(group1$cluster)

group1 <- ggparcoord(data = group1, columns = c(2:8),
                     groupColumn = "cluster", scale = "globalminmax") + 
  labs(x = "variables", y = "scaled importance", title = "Clustering")

ggplotly(group1)


group2 <- x %>% 
  filter(cluster == "2")
write_csv(group2,"Data/K_means_outs/K_means_no_day/group2.csv")

p2 <- ggparcoord(data = group2, columns = c(2:8),
                 groupColumn = "cluster", scale = "globalminmax") + 
  labs(x = "variables", y = "scaled importance", title = "Clustering")
ggplotly(p2)

group3 <- x %>% 
  filter(cluster == "3")
write_csv(group3,"Data/K_means_outs/K_means_no_day/group3.csv")

p3 <- ggparcoord(data = group3, columns = c(2:8),
                 groupColumn = "cluster", scale = "globalminmax") + 
  labs(x = "variables", y = "scaled importance", title = "Clustering")
ggplotly(p3)


group4 <- x %>% 
  filter(cluster == "4")
write_csv(group4,"Data/K_means_outs/K_means_no_day/group4.csv")

p4<- ggparcoord(data = group4, columns = c(2:8),
                groupColumn = "cluster", scale = "globalminmax") + 
  labs(x = "variables", y = "scaled importance", title = "Clustering")
ggplotly(p4)

group5 <- x %>% 
  filter(cluster == "5")
write_csv(group5,"Data/K_means_outs/K_means_no_day/group5.csv")


p5<- ggparcoord(data = group5, columns = c(2:8),
 groupColumn = "cluster", scale = "globalminmax") + 
  labs(x = "variables", y = "scaled importance", title = "Clustering")
ggplotly(p5)

group6 <- x %>% 
  filter(cluster == "6")
write_csv(group6,"Data/K_means_outs/K_means_no_day/group6.csv")


p6 <- ggparcoord(data = group6, columns = c(2:8),
                 groupColumn = "cluster", scale = "globalminmax") + 
  labs(x = "variables", y = "scaled importance", title = "Clustering")
ggplotly(p6)



# make an NMDS plot with the species colored by cluster
library(vegan)

mds_results <- x %>%
  dplyr::select(2:8) %>%
  metaMDS(., distance="euclidean") %>%
  scores() %>%
  as.data.frame() %>%
  bind_cols(x %>%
              dplyr::select(Species, cluster))

ggplot(mds_results, aes(x=NMDS1, y=NMDS2, color=cluster))+
  geom_point()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  scale_color_brewer(palette="Set1")

library(ggfortify)
library(ggrepel)

pca_dat <- x %>%
  dplyr::select(2:8) %>%
  prcomp(.) %>%
  .$x %>%
  as.data.frame() %>%
  bind_cols(x %>%
              dplyr::select(Species, cluster))

ggplot(pca_dat, aes(x=PC1, y=PC2, color=cluster, label=Species))+
  geom_point()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  scale_color_brewer(palette="Set1")

ggplot(pca_dat, aes(x=PC1, y=PC2, color=cluster, label=Species))+
  geom_point()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  scale_color_brewer(palette="Dark2")+
  geom_label_repel()



