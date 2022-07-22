#BRT as a function
#seasonal data
#day driven species only 

library(dplyr)
library (parallel)
library(dismo)
library(tidyverse)
library(readr)
library(ggplot2)
library(gridExtra)
source("R/global_functions.R")

mc.cores= 4

#test
#dat <- readRDS("Data/mod_data/Litoria peronii.rds") %>%
  #as.data.frame()

#species list
day_species <- readRDS("Data/BRT_results/day_species_list.RDS") %>%
  as.data.frame()
#reorder to exclude day

dat = dat[,c(1,5,2,3,4,6,7,8,9,10,11,12,13,14)] 

species_list <- day_species$Species

#test run
#species_list<-head(species_list, 1)

run_BRT_day_function <- function(species_name){  
  #apply_to_list <- function(species_name){
  
  message(paste0("Analyzing ", species_name))
  
  #first read in the species modeling data
  dat <- readRDS(paste0("Data/mod_data/", species_name, ".rds")) # gsub is not needed
  mod_data<-dat
  ###from RF day species script
  ##need to modify because there are no 100% here
  #so just select species where day was the most important variable? 
  
  # Now want to get the day-range where 90% of the data fall
  # for presences only!
  present <- mod_data %>% 
    dplyr::filter(present==1)
  
  ggplot(present, aes(x=day))+
    geom_histogram(color="black", fill="gray80", bins=50)
  
  days <- data.frame(day=c(1:365))
  
  top_90_percent_data <- present %>%
    group_by(day) %>%
    summarize(N=n()) %>% 
    right_join(days) %>%
    replace_na(list(N=0)) %>%
    mutate(total=nrow(present)) %>%
    mutate(proportion=N/total) %>%
    arrange(desc(proportion)) %>%
    mutate(cum_proportion=cumsum(proportion)) %>%
    dplyr::filter(cum_proportion<0.9)
  
  mod_data2 <- mod_data %>%
    dplyr::filter(day %in% top_90_percent_data$day)

  #These settings
  #number of trees determinded in each model 
  
  dat.tc5.lr005 <- gbm.step(as.data.frame(mod_data2), gbm.x = 3:14, gbm.y = 1, 
                            y.label="fitted function", x.label=NULL,
                            family = "bernoulli", tree.complexity = 5,
                            learning.rate = 0.005, bag.fraction = 0.5)
  #gbm.step does k-fold to optimal number
  #bernoulli = binomial 
  
  
  
  summary<- summary(dat.tc5.lr005)%>%
    as.data.frame()%>%  
    dplyr::mutate(Species = species_name)
  

  saveRDS(summary, paste0("Data/BRT_day_results/", species_name, ".RDS"))
  #saveRDS(summary, "Adelotus_brevis.RDS") 

  #need to add interactions 
  find.int <- gbm.interactions(dat.tc5.lr005)
  
  int_out<- find.int$interactions %>%
    as.data.frame()%>%
    mutate(Species = species_name)
  
  #and save the output for ~a secondary heatmap
  saveRDS(int_out, paste0("Data/BRT_day_results/interactions_tables/", species_name, ".RDS"))

  dev.off()
  gbm::plot.gbm(dat.tc5.lr005, 5)
  dismo::gbm.plot(dat.tc5.lr005, 5)
  
  # function to get the data out for each individual predictor
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
  partial_dependence_data <- bind_rows(lapply(c(1:12), get_partial_dependency))
  

 # remover the three 'covariables' that we aren't interested in technically
 
   plot_dat <- partial_dependence_data %>%
    left_join(., summary, by= "var") %>%
    mutate(label=paste0(var, " (", round(rel.inf, 2), "%)")) %>%
    dplyr::filter(! var %in% c("number_of_samples", "grid_lat", "grid_lng"))

   
  bpd<- ggplot(plot_dat, aes(x=x, y=y,))+
    geom_line()+
    facet_wrap(~label, scales="free_x", ncol=4)+
    theme_bw()+
    theme(axis.text=element_text(color="black", size=5.5))+
    ylab("Fitted function")+
    xlab("Predictor variable value")+
    theme(strip.text.x = element_text(size = 4.3))+
    theme(axis.title=element_text(size=6))


  ggsave(paste0("figures/BRT_day_plots/", species_name, ".png", sep=""), plot = bpd, device ="png")
  #ggsave(filename=paste(species_name, ".png",sep=""), plot=bpd, device = "png")
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
  saveRDS(model_sum_df, paste0("Data/BRT_day_results/model_summaries/", species_name, ".RDS"))
  
  
}

for (s in species_list$species) { 
  run_BRT_day_function(s)
  
  
}


# test the function on an example species
run_BRT_function("Litoria latopalmata")

