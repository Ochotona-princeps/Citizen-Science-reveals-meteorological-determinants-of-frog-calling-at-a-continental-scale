# K-means clustering

library(dplyr)
library(purrr)
library(ggplot2)
library(gridExtra)
library(purrr)
library(RColorBrewer)
library(viridis)  
library(ggrepel)
library(stringr)

# set wd to folder where RDS results are stored


#set working directory to that folder 
setwd("Data/BRT_results")


# read in data
unscaled_imp <- list.files(pattern = ".RDS") %>%
  map(readRDS) %>% 
  bind_rows()
#now scale 0-1
#that's how it was in the original script 
data <- unscaled_imp %>%
  dplyr::filter(!var %in% c("number_of_samples", "grid_lat", "grid_lng")) %>%
  group_by(Species) %>%
  mutate(rel.inf_scaled=scales::rescale(rel.inf))

# reset working directory for rest of script up one level
setwd("../../..")


# Need to visualize these results somehow
# summarizing across all species
ggplot(data, aes(x=var, y=rel.inf_scaled, color=var))+
  geom_point(alpha=0.8)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  coord_flip()+
  scale_color_brewer(palette="Set1")+
  guides(color=FALSE)+
  xlab("")+
  ylab("Scaled importance")

# one potential way to visualize species-specific results
length(unique(data$Species))


data$var <- factor(data$var, levels = c("day","mean_vapour", "mean_max_temp","max_temp_10_days", "min_temp_10_days",  "mean_min_temp", "phase", "rain_10_days", "rain_3_days", "mean_rainfall"))

P<- ggplot(data, aes(x=Species, y=rel.inf_scaled,color= var))+
  geom_point()+
  geom_point(size = 2)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  theme(axis.text.y = element_text(face="italic"))+
  coord_flip()+
  scale_fill_brewer("Dark2")+ 
  xlab("")+
  scale_x_discrete(limits=rev)
#ylab("Scaled importance")

P
######
# this will get difficult though with too many species
library(tidyverse)
library(gridExtra)

#first regroup our data so all the variables associate with a species in one line
regroup <- data %>% 
  filter(var== "rain_10_days")

regroup$Im_rain_10_days<-regroup$rel.inf_scaled

x<-regroup %>%
  left_join(select(rain10days, Im_rain_10_days), by = "Species")

meanrain <- data %>% 
  filter(var == "mean_rainfall")

meanrain$Im_mean_rain<-meanrain$rel.inf_scaled

rain3days <- data %>% 
  filter(var == "rain_3_days")

rain3days$Im_rain_3_days<-rain3days$rel.inf_scaled

phase <- data %>% 
  filter(var == "phase")

phase$Im_phase<-phase$rel.inf_scaled

mintemp <- data %>% 
  filter(var == "mean_min_temp")

mintemp$Im_min_temp<-mintemp$rel.inf_scaled

maxtemp <- data%>% 
  filter(var == "mean_max_temp")

maxtemp$Im_max_temp<-maxtemp$rel.inf_scaled

days <- data %>% 
  filter(var == "day")

days$Im_day<-days$rel.inf_scaled

vapour <- data%>% 
  filter(var == "mean_vapour")

vapour$Im_vapour<-vapour$rel.inf_scaled

mintemp10 <- data%>% 
  filter(var == "min_temp_10_days")

mintemp10$Im_mintemp10<-mintemp10$rel.inf_scaled

maxtemp10 <- data%>% 
  filter(var == "max_temp_10_days")

maxtemp10$Im_maxtemp10<-maxtemp10$rel.inf_scaled


x<-meanrain %>%
  left_join(dplyr::select(regroup,Species, Im_rain_10_days), by = "Species")%>%
  left_join(dplyr::select(phase,Species, Im_phase), by = "Species")%>%
  left_join(dplyr::select(mintemp,Species, Im_min_temp), by = "Species")%>%
  left_join(dplyr::select(maxtemp,Species, Im_max_temp), by = "Species")%>%
  left_join(dplyr::select(days,Species, Im_day), by = "Species")%>%
  left_join(dplyr::select(rain3days,Species, Im_rain_3_days), by = "Species")%>%
  left_join(dplyr::select(vapour,Species, Im_vapour), by = "Species")%>%
  left_join(dplyr::select(mintemp10,Species, Im_mintemp10), by = "Species")%>%
  left_join(dplyr::select(maxtemp10,Species, Im_maxtemp10), by = "Species")



x<-x[-c(1,2,4:10)]



write_csv(x, "Data/K_means_outs/k_means_clusters_06302022.csv")


#Okay, now retrying the tutorial 


plot1 <- x%>% 
  ggplot(aes(x = "species", y = Im_day)) + 
  geom_jitter(width = .025, size = 2, alpha = .3, color = "violet") +
  labs(x = "", y="importance of day")

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


plot9 <- x%>% 
  ggplot(aes(x = "species", y = Im_mintemp10)) + 
  geom_jitter(width = .025, size = 2, alpha = .3, color = "navy") +
  labs(x = "", y="importance of 10 previous days minimum temperature")


plot10 <- x%>% 
  ggplot(aes(x = "species", y = Im_maxtemp10)) + 
  geom_jitter(width = .025, size = 2, alpha = .3, color = "purple") +
  labs(x = "", y="importance of 10 previous days maximum temperature")

grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9, plot10)


# As the initial centroids are defined randomly,
# we define a seed for purposes of reprodutability
set.seed(123)

# Let's remove the column with the species' names, so it won't be used in the clustering
input <- x[-c(1,2)]

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

#reran and new results - hinge at 5-8 

set.seed(123)
clustering <- kmeans(input, centers = 5, nstart = 20)
clustering

#Total _ss = 62%
set.seed(123)
clustering <- kmeans(input, centers = 6, nstart = 20)
clustering
#Total _ss = 66%

#but I also saw a big drop at 2
set.seed(123)
clustering <- kmeans(input, centers = 2, nstart = 20)
clustering
#Total _ss = 38%

library(cluster)
library(factoextra)

#so keep 7 here? 
set.seed(123)
clustering <- kmeans(input, centers = 7, nstart = 20)
clustering
#Total _ss = 69%

sil <- silhouette(clustering$cluster, dist(input))
fviz_silhouette(sil)

#The silhouette plot below gives us evidence that our clustering using seven groups 
#is subpar
#even though it's the best option 
#because #there's negative silhouette width 
#and most of the values are lower than 0.5.



library(GGally)
library(plotly)

x$cluster <- as.factor(clustering$cluster)
write_csv(x, "Data/K_means_outs/K_means_clusters_groups_January.csv")
#new name so you don't erase the other data format

p <- ggparcoord(data = x, columns = c(2,3,4,5,6,7,8,9,10,11,12), groupColumn = "cluster", scale = "globalminmax") + labs(x = "predictors of calling", y = "importance", title = "Clustering")

ggplotly(p)

## now to try identifying the group members

x$cluster

group1 <- x %>% 
  filter(cluster == "1")
group1$cluster <- as.factor(group1$cluster)

group1 <- ggparcoord(data = group1, columns = c(2:12),
   groupColumn = "cluster", scale = "globalminmax") + 
labs(x = "variables", y = "scaled importance", title = "Clustering")
ggplotly(group1)


group2 <- x %>% 
  filter(cluster == "2")
p2 <- ggparcoord(data = group2, columns = c(2:12),
                 groupColumn = "cluster", scale = "globalminmax") + 
  labs(x = "variables", y = "scaled importance", title = "Clustering")
ggplotly(p2)

group3 <- x %>% 
  filter(cluster == "3")
p3 <- ggparcoord(data = group3, columns = c(2:12),
                 groupColumn = "cluster", scale = "globalminmax") + 
  labs(x = "variables", y = "scaled importance", title = "Clustering")
ggplotly(p3)


group4 <- x %>% 
  filter(cluster == "4")
p4<- ggparcoord(data = group4, columns = c(2:12),
                groupColumn = "cluster", scale = "globalminmax") + 
  labs(x = "variables", y = "scaled importance", title = "Clustering")
ggplotly(p4)

group5 <- x %>% 
  filter(cluster == "5")
write_csv(group5,"Data/K_means_outs/group5.csv")
p5 <- ggparcoord(data = group5, columns = c(2:12),
                 groupColumn = "cluster", scale = "globalminmax") + 
  labs(x = "variables", y = "scaled importance", title = "Clustering")
ggplotly(p5)

group6 <- x %>% 
  filter(cluster == "6")
write_csv(group6,"Data/K_means_outs/group6.csv")
 p6 <- ggparcoord(data = group6, columns = c(2:12),
                 groupColumn = "cluster", scale = "globalminmax") + 
  labs(x = "variables", y = "scaled importance", title = "Clustering")
ggplotly(p6)

group7 <- x %>% 
  filter(cluster == "7")
write_csv(group7,"Data/K_means_outs/group7.csv")
p7 <- ggparcoord(data = group7, columns = c(2:12),
                 groupColumn = "cluster", scale = "globalminmax") + 
  labs(x = "variables", y = "scaled importance", title = "Clustering")
ggplotly(p7)


library(vegan)
library(tidyverse)
library(ggfortify)
library(ggrepel)


##problems? try no non numeric other columns 
pca_dat <- input %>%
  dplyr::select(1:10) %>%
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



