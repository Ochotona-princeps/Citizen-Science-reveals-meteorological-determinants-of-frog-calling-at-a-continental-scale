# play K-means clustering

library(dplyr)
library(purrr)
library(ggplot2)
library(gridExtra)
library(parallel)
library(RColorBrewer)
library(viridis)  
library(ggrepel)
library(stringr)

mc.cores = 4

# set wd to folder where RDS results are stored
setwd("Data/model_results/predictor_importance")

# read in data
species_importance <- list.files(pattern = ".RDS") %>%
  map(readRDS) %>% 
  bind_rows()

species_importance<- species_importance %>%
  filter(Species != "Cophixalus hosmeri")

test<- species_importance %>%
  filter(str_detect(Species, "Litoria"))

#filter(Species == "Litoria wilcoxii")

#levels(species_importance$predictor)<-c("Day of year", "Mean minimum temperature", "Mean maximum temperature", "Mean humidity", "Rainfall over the past 10 days", "Rainfall over the past 3 days", "Rainfall the day of calling", "Moonphase" )
 #messing it up now - everything turns to NA instead 
  
# reset working directory for rest of script up one level
setwd("../../..")


# Need to visualize these results somehow
# summarizing across all species
ggplot(species_importance, aes(x=predictor, y=importance, color=predictor))+
  geom_point(alpha=0.8)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  coord_flip()+
  scale_color_brewer(palette="Set1")+
  guides(color=FALSE)+
  xlab("")+
  ylab("Scaled importance")

###one species example/litoria 

ggplot(test, aes(x=predictor, y=importance, color=predictor))+
  geom_point(alpha= 0.8)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  coord_flip()+
  scale_color_brewer(palette="Set1")+
  guides(color=FALSE)+
  xlab("")+
  ylab("Scaled importance")
####


# one potential way to visualize species-specific results
length(unique(species_importance$Species))

#### added Dec 2020 to order predictors by importance 
#mean importance in "mean 2diff models script 

topdown<- species_importance %>%
  group_by(predictor) %>%
  summarize(mean_PI = mean(importance, na.rm = TRUE))

# to orgaize the legend 



#species_importance$predictor <- factor(species_importance$predictor, levels = c("Day of year", "Mean minimum temperature", "Mean maximum temperature", "Mean humidity", "Rainfall over the past 10 days", "Rainfall over the past 3 days", "Rainfall the day of calling", "Moonphase"))
#not working now 
#so legend not lined up right

P<- ggplot(species_importance, aes(x=Species, y=importance,color= predictor))+
  aes(x = reorder(Species, desc(Species)))+
  geom_point()+
  geom_point(size = 2)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  theme(axis.text.y = element_text(face="italic"))+
  coord_flip()+
  #scale_color_brewer(palette ="Set1")+
  scale_color_brewer(palette = "Dark2" )+ 
  xlab("")+
  ylab("Scaled importance")+
  facet_wrap(~predictor, ncol = 4)

P

#### litoria subset example

reg <- ggplot(test, aes(x=Species, y=importance,color= predictor))+
  aes(x = reorder(Species, desc(Species)))+
  geom_point()+
  geom_point(size = 2)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  theme(axis.text.y = element_text(face="italic"))+
  coord_flip()+
  scale_color_brewer(palette = "Dark2" )+ 
  xlab("")+
  ylab("Scaled importance")+
  facet_wrap(~predictor, ncol = 8)

reg



brewer.pal(n = 8, name = "Dark2")

######
# this will get difficult though with too many species
library(tidyverse)
library(gridExtra)

#first regroup our data so all the variables associate with a species in one line
regroup <- species_importance %>% 
  filter(predictor == "rain_10_days")

regroup$Im_rain_10_days<-regroup$importance

x<-regroup %>%
left_join(select(rain10days,Species, Im_rain_10_days), by = "Species")

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

days <- species_importance %>% 
  filter(predictor == "day")

days$Im_day<-days$importance

vapour <- species_importance %>% 
  filter(predictor == "mean_vapour")

vapour$Im_vapour<-vapour$importance


x<-meanrain %>%
  left_join(dplyr::select(regroup,Species, Im_rain_10_days), by = "Species")%>%
  left_join(dplyr::select(phase,Species, Im_phase), by = "Species")%>%
  left_join(dplyr::select(mintemp,Species, Im_min_temp), by = "Species")%>%
  left_join(dplyr::select(maxtemp,Species, Im_max_temp), by = "Species")%>%
  left_join(dplyr::select(days,Species, Im_day), by = "Species")%>%
  left_join(dplyr::select(rain3days,Species, Im_rain_3_days), by = "Species")%>%
  left_join(dplyr::select(vapour,Species, Im_vapour), by = "Species")

x<-x[-c(1,2)]




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

grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8)


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

#reran and new results - hinge at 7-8 

set.seed(123)
clustering <- kmeans(input, centers = 5, nstart = 20)
clustering

#Total _ss = 65.9%
# now 59 
library(cluster)
library(factoextra)

sil <- silhouette(clustering$cluster, dist(input))
fviz_silhouette(sil)

#The silhouette plot below gives us evidence that our clustering using four groups is good because 
#there's no negative silhouette width and most of the values are bigger than 0.5.


library(GGally)
library(plotly)
library(rlang)
library(ggplotlyExtra)

x$cluster <- as.factor(clustering$cluster)


p <- ggparcoord(data = x, columns = c(2,3,4,5,6,7,8,9), groupColumn = "cluster", scale = "globalminmax") + labs(x = "predictors of calling", y = "importance", title = "Clustering")

ggplotly(p)#now ggplotlyextra 


## now to try identifying the group members

x$cluster

group1 <- x %>% 
  filter(cluster == "1")
write_csv(group1,"Data/K_means_outs/group1.csv")

group1$cluster <- as.factor(group1$cluster)

group1 <- ggparcoord(data = group1, columns = c(2:9),
          groupColumn = "cluster", scale = "globalminmax") + 
  labs(x = "variables", y = "scaled importance", title = "Clustering")
  
ggplotly(group1)

group2 <- x %>% 
  filter(cluster == "2")
write_csv(group2,"Data/K_means_outs/group2.csv")

p2 <- ggparcoord(data = group2, columns = c(2:9),
              groupColumn = "cluster", scale = "globalminmax") + 
  labs(x = "variables", y = "scaled importance", title = "Clustering")
ggplotly(p2)

group3 <- x %>% 
  filter(cluster == "3")
write_csv(group3,"Data/K_means_outs/group3.csv")

p3 <- ggparcoord(data = group3, columns = c(2:9),
                 groupColumn = "cluster", scale = "globalminmax") + 
  labs(x = "variables", y = "scaled importance", title = "Clustering")
ggplotly(p3)


group4 <- x %>% 
  filter(cluster == "4")
write_csv(group4,"Data/K_means_outs/group4.csv")

p4<- ggparcoord(data = group4, columns = c(2:9),
                groupColumn = "cluster", scale = "globalminmax") + 
  labs(x = "variables", y = "scaled importance", title = "Clustering")
ggplotly(p4)

group5 <- x %>% 
  filter(cluster == "5")
write_csv(group5,"Data/K_means_outs/group5.csv")

p5 <- ggparcoord(data = group5, columns = c(2:9),
                 groupColumn = "cluster", scale = "globalminmax") + 
  labs(x = "variables", y = "scaled importance", title = "Clustering")
ggplotly(p5)

group6 <- x %>% 
  filter(cluster == "6")
write_csv(group6,"Data/K_means_outs/group6.csv")

p6 <- ggparcoord(data = group6, columns = c(2:9),
                 groupColumn = "cluster", scale = "globalminmax") + 
  labs(x = "variables", y = "scaled importance", title = "Clustering")
ggplotly(p6)

group7 <- x %>% 
  filter(cluster == "7")
write_csv(group7,"Data/K_means_outs/group7.csv")

p7 <- ggparcoord(data = group7, columns = c(2:9),
                 groupColumn = "cluster", scale = "globalminmax") + 
  labs(x = "variables", y = "scaled importance", title = "Clustering")
ggplotly(p7)


# make an NMDS plot with the species colored by cluster
library(vegan)

mds_results <- x %>%
  dplyr::select(2:9) %>%
  metaMDS(., distance="euclidean") %>%
  scores() %>%
  as.data.frame() %>%
  bind_cols(x %>%
              dplyr::select(Species, cluster))

ggplot(mds_results, aes(x=NMDS1, y=NMDS2, color=cluster))+
  geom_point(size=3)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  scale_color_brewer(palette="Set1")

library(ggfortify)
library(ggrepel)
library(RColorBrewer)

pca_dat <- x %>%
  dplyr::select(2:9) %>%
  prcomp(.) %>%
  .$x %>%
  as.data.frame() %>%
  bind_cols(x %>%
              dplyr::select(Species, cluster))

test<- ggplot(pca_dat, aes(x=PC1, y=PC2, color=cluster, label=Species))+
  geom_point(size =3)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  scale_color_brewer(palette="Set1")


##### trying interactive plotly widget

######https://www.htmlwidgets.org/showcase_plotly.html


library(plotly)

ggplotly(test)

##now to host it somewhere
library(htmlwidgets)

htmlwidgets::saveWidget(test, "figures/HTMLPCA_0517.html")


fig2 <- plotly_build(test)
fig2$sizingPolicy$browser$padding <- 0

saveWidget(
  fig2, 
  "HTMLPCA.html", 
  selfcontained = TRUE, 
  libdir = NULL,
  background = "white",
  title = "",
  knitrOptions = list()
)



#cbp2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



ggplot(pca_dat, aes(x=PC1, y=PC2, color=cluster, label=Species))+
  geom_point()+
  theme_classic()+
  theme(axis.text=element_text(color="grey"))+
  #scale_fill_manual(values = wes_palette("GrandBudapest1"))+
  #scale_fill_manual(values = cbp2)+
  scale_color_brewer(palette= "Dark2")+
  geom_label_repel()
    

library(ggrepel)
library (RColorBrewer)
set.seed(123)

p<- ggplot(pca_dat, aes(x=PC1, y=PC2, colour = cluster,label=Species))+
  geom_point()+
  theme_classic()+
  theme(axis.text=element_text(color="black"))+
  labs(title = "Annual species model k-means clusters")+
  scale_color_brewer(palette="Set1")+
  #theme(text = element_text())+
  geom_text_repel(box.padding = 0.1, max.overlaps = Inf) +
  ylim (-5, 5) +
  xlim(-5, 5)

p

p <- p + theme(text = element_text(size = 0.3)) 

p1<- p + geom_text_repel(min.segment.length = 1, max.segment.length = 100)

p1


