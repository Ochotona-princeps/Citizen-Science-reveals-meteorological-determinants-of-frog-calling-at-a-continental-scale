#BRT as a function

library(dplyr)
library (parallel)
library(dismo)
library(tidyverse)
library(readr)
library(ggplot2)
library(gridExtra)
source("R/global_functions.R")
library(parallel)
mc.cores= 4




#test
#dat<- readRDS("Data/mod_data/Adelotus brevis.rds") %>%
  #as.data.frame()

  species_list <- readRDS("raw_frogid_data/FrogID_clean_data.RDS") %>%
    group_by(species)%>%
    summarize(count=n())%>%
    filter(count >100)%>%
    distinct(species)%>%
    dplyr::select(species)

  

run_BRT_function <- function(species_name){  
  #apply_to_list <- function(species_name){
    
    message(paste0("Analyzing ", species_name))
  
  #first read in the species modeling data
  dat <- readRDS(paste0("Data/mod_data/", species_name, ".rds")) # gsub is not needed

  #These settings
  #number of trees determined in each model 
  
  dat.tc5.lr005 <- gbm.step(as.data.frame(dat), gbm.x = 2:14, gbm.y = 1, 
                            y.label="fitted function", x.label=NULL,
                            family = "bernoulli", tree.complexity = 5,
                            learning.rate = 0.005, bag.fraction = 0.5)
  #gbm.step does k-fold to optimal number
#bernoulli = binomial 

  
  summary<- summary(dat.tc5.lr005)%>%
    as.data.frame()%>%  
    dplyr::mutate(Species = species_name)




  saveRDS(summary, paste0("Data/BRT_results/", species_name, ".RDS"))

  ##
  #add interactions 
  find.int <- gbm.interactions(dat.tc5.lr005)
  
  int_out<- find.int$interactions %>%
    as.data.frame()%>%
    mutate(Species = species_name)

  #and save the output for ~a secondary heatmap
  saveRDS(int_out, paste0("Data/BRT_results/interactions_tables/", species_name, ".RDS"))

# this stack overflow helps explain some of this...
# https://stats.stackexchange.com/questions/122721/r-partial-dependency-plots-from-gbm-package-values-and-y-axis
# now the difference (y-axis) between these two figures
dev.off()
gbm::plot.gbm(dat.tc5.lr005, 5)
dismo::gbm.plot(dat.tc5.lr005, 5)

# write a quick function to get the data out for each individual predictor
# then we can combine them and make our own plot by ourselves...
get_partial_dependency <- function(variable_number){
  
  # get the values underlying the plot
  
  dat <- gbm::plot.gbm(dat.tc5.lr005, variable_number, return.grid=TRUE) %>%
    as.data.frame() 
  
  # pull out the predictors
  predictors <- dat[,1]
  
  # pull out the responses
  responses <- dat[,2] - mean(dat[,2])
  
  # center the responses around 0
  # to match how dismo::gbm.plot produces it and how Elith 2008 produces the plots
  responses <- scale(responses, scale = FALSE)
  
  # pull it together into a dataframe
  dat2 <- data.frame(x=predictors,
                     y=responses) %>%
    mutate(var=names(dat)[1])
  
  # return dataframe
  return(dat2)
  }
# now get a single dataframe that can be used to recreate the plot made
# using the plot.gbm function
partial_dependence_data <- bind_rows(lapply(c(1:13), get_partial_dependency))

####ADD another save out 100 files 
##save for posterity
saveRDS(partial_dependence_data, paste0("Data/BRT_results/pd/", species_name, ".RDS"))

# now make a plot too.
# first join in the relative influence
# as I like having the 'percentage' in there...
# remover the three 'covariables' that we aren't interested in technically
plot_dat <- partial_dependence_data %>%
  left_join(., summary, by="var") %>%
  mutate(label=paste0(var, " (", round(rel.inf, 2), "%)")) %>%
  dplyr::filter(! var %in% c("number_of_samples", "grid_lat", "grid_lng"))

bpd<- ggplot(plot_dat, aes(x=x, y=y,))+
  geom_line()+
  facet_wrap(~label, scales="free_x", ncol=5)+
  theme_bw()+
  theme(axis.text=element_text(color="black", size=5.5))+
  ylab("Fitted function")+
  xlab("Predictor variable value")+
  theme(strip.text.x = element_text(size = 4.3))+
  theme(axis.title=element_text(size=6))

ggsave(paste0("figures/BRT_plots/", species_name, ".png", sep=""), plot = bpd, device ="png")

dev.off()


##### Get some summary statistics from the model fit
model_sum_df<- data.frame(number_of_trees = dat.tc5.lr005$n.trees,
                          estimated_cv_deviance=dat.tc5.lr005$cv.statistics$deviance.mean,
                          estimated_cv_deviance_se=dat.tc5.lr005$cv.statistics$deviance.se,
                          cv_correlation=dat.tc5.lr005$cv.statistics$correlation.mean,
                          cv_correlation_se=dat.tc5.lr005$cv.statistics$correlation.se,
                          cv_AUC_score=dat.tc5.lr005$cv.statistics$discrimination.mean,
                          cv_AUC_score_se=dat.tc5.lr005$cv.statistics$discrimination.se) %>%
                          as.data.frame()%>%
                          mutate(Species = species_name)
  

#and save the output
saveRDS(model_sum_df, paste0("Data/BRT_results/model_summaries/", species_name, ".RDS"))
###write a script to bind
##and summarize
##add to appendix

  }
  
  for (s in species_list$species) { 
    run_BRT_function(s)
    
    
  }
  
  

  