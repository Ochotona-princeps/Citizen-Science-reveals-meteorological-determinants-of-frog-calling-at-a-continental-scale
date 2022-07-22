# Citizen-Science-reveals-meteorological-determinants-of-frog-calling-at-a-continental-scale
Recipe for Citizen Science reveals meteorological determinants of frog calling at a continental scale - 

#this recipe starts from scratch
#to start with the model inputs provided in this repository's "Data" folder
jump to line # 88

Bring together frog call data
•	Download FrogID data 
https://www.frogid.net.au/explore
•	Check for outliers in the frog ID script and have them reviewed by an expert
(check_frogID_clean_data_for_outliers.R)
 update the clean_data if temporally odd records turn out to be misidentified 
•	Assign frog ID data to grids 
(assign_frogid_reccords_to_grids.R)
Prereq: raw_frogid_data.csv
Output: FrogID_griddata.csv
•	Use the frogid_griddata output file as the input to find in range observations
(subset_data_to_in_range_obs.R)
Prereq: Data/clean_frogid_data_with_grids/FrogID_griddata.csv
Output: Data/clean_frogid_data_with_grids/in_range_observations.csv
•	Use the output from that as the input for suncalc to assign sun and moon cycle information to each grid (script_to_run_suncalc_and_mooncalc_data.R)
Prereq: Data/clean_frogid_data_with_grids/in_range_observations.csv
Output: Data/clean_frogid_data_with_grids/in_range_observations_with_moon_sun.csv

Download BOM data (Separate, can be before or after frog data)
Rainfall http://www.bom.gov.au/jsp/awap/rain/archive.jsp?colour=colour&map=totals&period=daily&area=nat
Mean Maximum and Minimum Temperature http://www.bom.gov.au/jsp/awap/temp/archive.jsp?colour=colour&map=maxave&period=daily&area=nat
9 am Vapour Pressure
http://www.bom.gov.au/jsp/awap/vprp/archive.jsp?colour=colour&map=vprph09&period=daily&area=nat
•	Clean Climate Zone shapefile
(cleaning_climate_zone_shapefile.R)
Prereq: Data/spatial_data/climate_zones/climate_zones.shp
Output: Data/spatial_data/climate_zones/climate_zones_clean.shp
•	Name Format weather data
Unzips, renames, and changes file format of BOM data downloads
Prereq: Data/new weather data* (temp*)
Output: Data/ min_temp_data/…, Data/max_temp data/…, Data/min_temp_data/…, Data/rainfall_data/…, Data/9am_vapour_pressure/…
•	Convert_ASCII_to_sf_RDS_files – converts data from BOM format to a useful one
(convert_ASCII_environmental_data_to_sf.R)
Prereq: Data/max temp data/… et al/
Output: Data/max temp data_sf_RDS/… et al 
Prereq: Data/min temp data/…
Output: Data/mintemp data_sf_RDS/…
Prereq: Data/rainfall data/…
Output: Data/rainfall data_sf_RDS/…
Prereq: Data/9am vapour pressure data/…
Output: Data/9am_vapour pressure data_sf_RDS/…
•	Add columns that create a mean of ten days worth of data to min_temp_data and max_temp_data files 
(10_day_max_and_min_temp_for_BRT.R)
Prereq: Data/RDS outs/max_temp_data_summarized.RDS
	Data/RDS outs/min_temp_data_summarized.RDS
Output: Data/chunked_results/max_temp/….RDS (chunks 1 through 8)
Data/chunked_results/min_temp/….RDS (chunks 1 through 8)
•	Add columns that tally previous days rainfall to rainfall summary file 
(get_cumulative_rainfall_values.R)
Prereq: Data/RDS outs/rainfall_data_summarized.RDS
Output: Data/RDS outs/rainfall_data_cumulative.RDS
•	Rejoin weather data that ran in batches, because it’s too big. 
(rejoin weather RDS out_BRT.R)
Prereq: Data/chunked_results/max_temp/….RDS (chunks 1 through 8)
	Data/chunked_results/min_temp/….RDS (chunks 1 through 8)
Output: Data/RDS outs/max_temp_with_10.RDS
Data/RDS outs/min_temp_with_10.RDS

•	Summarize environmental data to frog ID grids 
(BOM girds need to be aggregated to FrogID grid size)
(summarize_max_temp_data_to_frogid_grids.R, summarize_min_temp_data_to_frogid_grids.R, summarize_rain_data_to_frogid_grids.R, summarize_vapour_to_frogid_grids.R)
Prereq: Data/rainfall_data_sf_RDS/…
Output: Data/RDS outs/rainfall_data_summarized.RDS
Prereq: Data/max temp data_sf_RDS/…
Output: Data/RDS outs/max_temp_data_summarized.RDS
Prereq: Data/min temp data_sf_RDS/…
Output: Data/RDS outs/min_temp_data_summarized.RDS
Prereq: Data/9am_vapour_pressure_sf_RDS/…
Output: Data/RDS outs/vapour_data_summarized.RDS

-------Bring it all together-------
•	Get presence of all species with species X’s range, to get presence/absence recordings
(get_species_model_range_dat.R)
Prereq: Data/clean_frogid_data_with_grids/in_range_observations_with_moon_sun.csv
Output: Data/species_range_model_data/", species_name, ".RDS

•	Bring all the data together to create model inputs on a species level
(BRT_create_mod_data.R)
Prereqs: EVERYTHING listed above, plus 	("R/global_functions.R")

Outputs: Data/mod_data/species_name.RDS (100 files)
*provided in this repository, so you can start here

•	Create raw data jitter plots to visualize patterns/in appendix
(BRT_jitterplots.R)
Prereqs: Data/species_range_model_data/species_name.RDS

Output: figures/BRT_raw_data_plots/species_name.PNG (100 files)

•	Run boosted regression tree models for all 100 species 
(BRT_function_annual_all_species.R)
Prereqs: Data/mod_data/ species_name.RDS (100 files)

Outputs: Data/BRT_results/species_name.RDS
 Data/BRT_results/interactions_tables/species_name.RDS
Data/BRT_results/pd/species_name,.RDS
figures/BRT_plots/species_name.png
Data/BRT_results/model_summaries/species_name.RDS

•	Create figure 2 and day species list 
(BRT_create_violin_plots_and_day_species_list.R)
Prereqs: Data/BRT_results/species_name.RDS(100 files)

Output: Data/BRT_results/day_species_list.RDS
	Figure 2

•	Run boosted regression tree models for seasonal species 
(BRT_day_only_mod.R)
Prereqs: Data/BRT_results/day_species_list.RDS
	Data/mod_data/ species_name.RDS (100 files)

Outputs: Data/BRT_day_results/species_name.RDS
 Data/BRT_day_results/interactions_tables/species_name.RDS
Data/BRT_results/pd/species_name,.RDS
figures/BRT_day_plots/species_name.png
Data/BRT_day_results/model_summaries/species_name.RDS

•	Explore interaction effects among variables of annual all species model and create figure 3
(Interaction_summary_tables_heatmap.R)
Prereqs: Data/BRT_day_results/interactions_tables/species_name.RDS
Output: Figure 3

•	K means clustering of model results – to explore patterns among species and create Figure 4 
(BRT_ K_means_clustering.R)
Prereqs: "BRT_model_results/species_name.RDS",
Output: Figure 4
