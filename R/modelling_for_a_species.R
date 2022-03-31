### This is an R script to
### read in a species' data
### and prepare the data for modelling
### and then do some modelling! Yay!

# packages
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(hrbrthemes)
library(mgcv)
library(broom)
library(patchwork)
library(MuMIn)
library(ggcorrplot)
library(GGally)
library(ranger)
library(scam)
library(PresenceAbsence)
#devtools::install_github("zmjones/edarf", subdir = "pkg")
library(edarf)
library(gridExtra)
library(forestmangr)
library (parallel)

# source global functions
source("R/global_functions.R")

mc.cores= 4

# read in min_temp data
min_temp <- readRDS("Data/RDS outs/min_temp_data_summarized.RDS") %>%
  unite(year_day, year, day, sep="_", remove=FALSE)

# read in max_temp data
max_temp <- readRDS("Data/RDS outs/max_temp_data_summarized.RDS") %>%
  unite(year_day, year, day, sep="_", remove=FALSE)

# read in rainfall data
rainfall <- readRDS("Data/RDS outs/rainfall_data_summarized.RDS") %>%
  unite(year_day, year, day, sep="_", remove=FALSE)

rainthree <- readRDS("Data/RDS outs/rainfall_cum_3_days.RDS") %>%
  unite(year_day, year, day, sep="_", remove=FALSE)

rain10 <- readRDS("Data/RDS outs/rainfall_cum_10_days.RDS") %>%
  unite(year_day, year, day, sep="_", remove=FALSE)

# read in vapour pressure data
# change vapour pressure to the day before
vapour <- readRDS("Data/RDS outs/vapour_data_summarized.RDS") %>%
  mutate(previous_day=Date-1) %>%
  mutate(year=year(previous_day)) %>%
  mutate(day=yday(previous_day)) %>%
  mutate(month=month(previous_day)) %>%
  unite(year_day, year, day, sep="_", remove=FALSE) %>%
  dplyr::select(-Date, -previous_day)


# pick a species
species_name <- "Litoria fallax" 


########### Function to read in and prepare data
########### and then make a plot of the species raw data and export plot
raw_data_plot_function <- function(species_name){
  
  # first read in the data
  # and mutate some time variables
  dat <- readRDS(paste0("Data/species_range_model_data/", gsub(" ", "_", species_name), ".RDS")) %>%
    mutate(day=yday(date)) %>% # add the julian day
    mutate(year=year(date)) %>% # add the year
    unite(year_day, year, day, sep="_", remove=FALSE) 
  # unite the two together for a key for later
  # RN may be problematic because vapour doesn't match 
  
  # get data for analysis
  analysis_dat <- dat %>%
    left_join(., min_temp) %>%
    left_join(., max_temp) %>%
    left_join(., vapour) %>%
    left_join(., rainfall) %>%
    left_join(., rainthree)%>%
    left_join(., rain10) 
  
  ## Need to split to 'grid analysis' too
  grid_info_only <- analysis_dat %>%
    dplyr::select(grid_id, grid_lat, grid_lng,
                  grid_sunrise, grid_sunset, grid_day_length,
                  fraction, phase, year_day:rain_10_days) %>%
    distinct()
  
  ## Splitting by grid analysis here
  samples_per_grid_and_day <- analysis_dat %>% 
    group_by(grid_id, year_day, present) %>% 
    summarize(N=n()) %>% 
    ungroup() %>% 
    group_by(grid_id, year_day) %>% 
    mutate(number_of_samples=sum(N)) %>% 
    dplyr::select(-N) %>%
    group_by(grid_id, year_day) %>%
    arrange(desc(present)) %>%
    slice(1) %>%
    ungroup() %>%
    right_join(grid_info_only) %>%
    mutate(present_char=case_when(
      present==0 ~ "Absent",
      present==1 ~ "Present"
    ))
  
  ## make plots of the variables we are interested in
  summary <- samples_per_grid_and_day %>%
    dplyr::select(present_char, day, phase, mean_rainfall,
                  rain_3_days, rain_10_days, mean_max_temp, 
                  mean_min_temp, mean_vapour) %>%
    rename(`Julian day` = day,
           `Moon phase` = phase,
           `Mean rainfall` = mean_rainfall,
           `3-day rain total` = rain_3_days,
           `10-day rain total` = rain_10_days,
           `Mean max temperature` = mean_max_temp,
           `Mean min temperature` = mean_min_temp,
           `Mean vapour pressure` = mean_vapour) %>%
    pivot_longer(-present_char, names_to="Variable", values_to="Value") %>%
    mutate(Variable=factor(Variable, levels=c("Mean rainfall", "3-day rain total",
                                              "10-day rain total", "Julian day",
                                              "Moon phase", "Mean vapour pressure",
                                              "Mean min temperature", "Mean max temperature")))
  
  ggplot(summary, aes(y=present_char, x=Value, color=Variable))+
    geom_jitter(alpha=0.5)+
    theme_bw()+
    theme(axis.text=element_text(color="black"))+
    facet_wrap(~Variable, scales="free")+
    scale_color_brewer(palette="Set1")+
    theme(legend.position="none")+
    xlab("Corresponding variable values")+
    ylab("")+
    ggtitle(paste0(species_name, "; Total sample size = ", 
                   nrow(samples_per_grid_and_day), 
                   "; Occurrences = ", samples_per_grid_and_day %>% 
                     dplyr::filter(present_char=="Present") %>% 
                     nrow(.)))
  
  
  ggsave(paste0("figures/raw_data_plots/", gsub(" ", "_", species_name), ".png"), width=9, height=7, units="in")
  
summary2 <- data.frame(species_name=species_name) %>%
  mutate(total_sample_size=nrow(samples_per_grid_and_day)) %>%
  mutate(number_presences=samples_per_grid_and_day %>% 
           dplyr::filter(present_char=="Present") %>% 
           nrow(.)) %>%
  mutate(number_grids_in_range=length(unique(samples_per_grid_and_day$grid_id))) %>%
  mutate(number_grids_with_presence=samples_per_grid_and_day %>%
           dplyr::filter(present_char=="Present") %>%
           dplyr::select(grid_id) %>%
           distinct() %>%
           nrow(.))

return(summary2)


}

# test the function on an example species
raw_data_plot_function("Litoria fallax")

# read in all the raw data
# make it into a list of detected species 
# and then apply the function to each of these species
species_list <- readRDS("raw_frogid_data/FrogID_clean_data.RDS") %>%
  group_by(species)%>%
  summarize(count=n())%>%
  filter(count >100)%>%
  distinct(species)%>%
  select(species)
  
summary_data <- bind_rows(lapply(species_list$species, raw_data_plot_function))

# make appendix B in one step 


saveRDS(summary_data, "Data/summary_species_stats.RDS")

write_csv(summary_data, "Data/summary_species_stats.csv")



# make a function to run a model for each species
run_random_forest_function <- function(species_name){
  
  message(paste0("Analyzing ", species_name))
  
  # first read in the data
  # and mutate some time variables
  dat <- readRDS(paste0("Data/species_range_model_data/", gsub(" ", "_", species_name), ".RDS")) %>%
    mutate(day=yday(date)) %>% # add the julian day
    mutate(year=year(date)) %>% # add the year
    unite(year_day, year, day, sep="_", remove=FALSE) 
  # unite the two together for a key for later
  
  # get data for analysis
  analysis_dat <- dat %>%
    left_join(., min_temp) %>%
    left_join(., max_temp) %>%
    left_join(., vapour) %>%
    left_join(., rainfall) %>%
    left_join(., rainthree)%>%
    left_join(., rain10) 
  
  ## Need to split to 'grid analysis' too
  grid_info_only <- analysis_dat %>%
    dplyr::select(grid_id, grid_lat, grid_lng,
                  grid_sunrise, grid_sunset, grid_day_length,
                  fraction, phase, year_day:rain_10_days) %>%
    distinct()
  
  ## Splitting by grid analysis here
  samples_per_grid_and_day <- analysis_dat %>% 
    group_by(grid_id, year_day, present) %>% 
    summarize(N=n()) %>% 
    ungroup() %>% 
    group_by(grid_id, year_day) %>% 
    mutate(number_of_samples=sum(N)) %>% 
    dplyr::select(-N) %>%
    group_by(grid_id, year_day) %>%
    arrange(desc(present)) %>%
    slice(1) %>%
    ungroup() %>%
    right_join(grid_info_only) %>%
    mutate(present_char=case_when(
      present==0 ~ "Absent",
      present==1 ~ "Present"
    ))
  
  # omit data
  mod_data <- samples_per_grid_and_day %>%
    dplyr::filter(complete.cases(.)) %>%
    dplyr::select(present, number_of_samples, day,
                  grid_lat, grid_lng, mean_max_temp,
                  mean_min_temp, phase, rain_10_days, 
                  rain_10_days, rain_3_days, mean_rainfall, mean_vapour) # choose the variables to model
  
  mod_data_split <- mod_data %>%
    split(if_else(runif(nrow(.)) <= 0.9, "train", "test"))
  
  detection_freq <- mean(mod_data_split$train$present)
  
  mod_data_split$train$present <- factor(mod_data_split$train$present)
  
  rf <- ranger(formula = present ~ .,
               data=mod_data_split$train,
               importance = "impurity",
               probability = TRUE,
               replace = TRUE,
               sample.fraction = c(detection_freq, detection_freq))
  
  # make predictions on training data
  occ_pred <- rf$predictions[, 2]
  
  # convert the observered response back to a numeric value from factor
  occ_obs <- as.numeric(as.character(mod_data_split$train$present))
  
  rf_pred_train <- tibble(obs = occ_obs, pred = occ_pred) %>% 
    drop_na()
  
  # fit gam calibration model
  # scam allows us to use constrained shapes for the smooths
  calibration_model <- scam(obs ~ s(pred, k = 6, bs = "mpi"), 
                            gamma = 2,
                            data = rf_pred_train)
  
  cal_pred <- tibble(pred = seq(0, 1, length.out = 100))
  cal_pred <- predict(calibration_model, cal_pred, type = "response") %>% 
    bind_cols(cal_pred, calibrated = .)
  
  model_calibration <- ggplot(cal_pred, aes(x = pred, y = calibrated))+
    geom_line()+
    labs(x = "RF prediction",
         y = "Calibrated prediction",
         title = "Calibration model")+ 
    xlim(0, 1)+ 
    ylim(0, 1)+
    theme_bw()+
    theme(axis.text=element_text(color="black"))
  
  
  # predict on test data using calibrated model
  p_fitted <- predict(rf, data = mod_data_split$test, type = "response")
  
  # extract probability of detection
  p_fitted <- p_fitted$predictions[, 2]
  
  p_calibrated <- predict(calibration_model, 
                          newdata = tibble(pred = p_fitted), 
                          type = "response")
  
  rf_pred_test <- data.frame(id = seq_along(p_calibrated),
                             # actual detection/non-detection
                             obs = mod_data_split$test$present,
                             # uncalibrated prediction
                             fit = p_fitted,
                             # calibrated prediction
                             cal = p_calibrated) %>%
    # constrain probabilities to 0-1
    mutate(cal = pmin(pmax(cal, 0), 1)) %>% 
    drop_na()
  
  # mean squared error (mse)
  mse_fit <- mean((rf_pred_test$obs - rf_pred_test$fit)^2, na.rm = TRUE)
  mse_cal <- mean((rf_pred_test$obs - rf_pred_test$cal)^2, na.rm = TRUE)
  
  # pick threshold to maximize kappa
  opt_thresh <- optimal.thresholds(rf_pred_test, opt.methods = "MaxKappa")
  
  # calculate accuracy metrics: auc, kappa, sensitivity, specificity, brier
  metrics_fit <- rf_pred_test %>% 
    select(id, obs, fit) %>% 
    presence.absence.accuracy(threshold = opt_thresh$fit, 
                              na.rm = TRUE, 
                              st.dev = FALSE)
  
  metrics_cal <- rf_pred_test %>% 
    select(id, obs, cal) %>% 
    presence.absence.accuracy(threshold = opt_thresh$cal, 
                              na.rm = TRUE, 
                              st.dev = FALSE)
  
  # combine various performance metrics together
  rf_assessment <- tibble(
    model = c("RF", "Calibrated RF"),
    mse = c(mse_fit, mse_cal),
    sensitivity = c(metrics_fit$sensitivity, metrics_cal$sensitivity),
    specificity = c(metrics_fit$specificity, metrics_cal$specificity),
    auc = c(metrics_fit$AUC, metrics_cal$AUC),
    kappa = c(metrics_fit$Kappa, metrics_cal$Kappa)
  ) %>%
  round_df(3)
  
  pi <- enframe(rf$variable.importance, "predictor", "importance")
  
  pi %>%
    dplyr::filter(! predictor %in% c("grid_lat", "grid_lng", "number_of_samples")) %>%
    mutate(importance=scales::rescale(importance)) %>%
    mutate(Species=species_name) %>%
    saveRDS(paste0("Data/model_results/predictor_importance/", gsub(" ", "_", species_name), ".RDS"))
  
  # plots
  variable_importance_plot <- ggplot(pi, aes(x = fct_reorder(predictor, importance), y = importance))+
    geom_col()+
    #geom_hline(yintercept = 0, size = 2, colour = "#555555")+
    scale_y_continuous(expand = c(0, 0))+
    coord_flip()+
    xlab("")+
    ylab("Predictor Importance (Gini Index)")+
    theme_bw()+
    theme(panel.grid = element_blank(),
          panel.grid.major.x = element_line(colour = "#cccccc", size = 0.5),
          axis.text=element_text(color="black"))+
    ggtitle("Predictor importance")
  
  # partial dependence
  # predictors of interest in our dataset
  top_pred <- pi %>% 
    filter(!predictor %in% c("number_of_samples", "grid_lng", "grid_lat")) %>% 
    arrange(desc(importance))
  
  # calculate partial dependence for each predictor
  pd <- top_pred %>% 
    mutate(pd = map(predictor, partial_dependence,
                    fit = rf, data = mod_data_split$train),
           pd = map(pd, ~ .[, c(1, 3)]),
           pd = map(pd, set_names, nm = c("value",  "occurrence_rate"))) %>% 
    unnest(cols = pd)
  
  # calibrate predictions
  pd$occurrence_rate <- predict(calibration_model, 
                               newdata = tibble(pred = pd$occurrence_rate), 
                               type = "response") %>% 
    as.numeric()
  
  # constrain probabilities to 0-1
  pd$occurrence_rate <- pmin(pmax(pd$occurrence_rate, 0), 1)
  
  # write out partial dependence data
  pd %>%
    mutate(Species=species_name) %>%
    saveRDS(paste0("Data/model_results/partial_dependence/", gsub(" ", "_", species_name), ".RDS"))
  
  # make plot of partial dependence for each variable
  partial_dependence_plots <- ggplot(pd, aes(x = value, y = occurrence_rate))+
    geom_line()+
    geom_point()+
    scale_y_continuous(labels = scales::percent)+
    facet_wrap(~ factor(predictor, levels=c("day", "mean_min_temp", "mean_max_temp",
                                            "mean_rainfall", "rain_3_days", "rain_10_days",
                                            "phase", "mean_vapour")), nrow = 3, scales = "free")+
    labs(x = NULL, y = "Probability of calling")+
    theme_bw()+
    theme(panel.grid = element_blank(),
          axis.line = element_line(color = "grey60"),
          axis.ticks  = element_line(color = "grey60"),
          axis.text=element_text(color="black"))
  
  # Set theme to allow for plotmath expressions
  tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
  tbl <- tableGrob(rf_assessment, rows=NULL, theme=tt)
  
  # Plot figures and table into one object
  # layout first
  lay <- rbind(c(1,1,1,2,2),
               c(1,1,1,3,3),
               c(4,4,4,3,3))
  
  pdf(paste0("figures/random_forest_summary/", gsub(" ", "_", species_name), ".pdf"), height=10, width=12)
  grid.arrange(partial_dependence_plots, tbl, 
               variable_importance_plot, model_calibration,
               top=paste0(species_name, " (N = ", nrow(samples_per_grid_and_day), " data points)"),
               layout_matrix=lay)
  dev.off()
  
}


# test the function on an example species
run_random_forest_function("Litoria latopalmata")

# read in all the raw data
# make it into a list of detected species 
# and then apply the function to each of these species
species_list <- readRDS("raw_frogid_data/FrogID_clean_data.RDS") %>%
  group_by(species)%>%
  summarize(count=n())%>%
  filter(count >100)%>%
  distinct(species)%>%
  select(species)

lapply_with_error(species_list$species, run_random_forest_function)










