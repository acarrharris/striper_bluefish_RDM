
options(scipen = 999)

packages <- c("tidyr",  "magrittr", "tidyverse", "reshape2", "splitstackshape","doBy","WriteXLS","Rcpp",
              "ggplot2","rlist","fitdistrplus","MASS","psych","rgl","copula","VineCopula","scales",
              "univariateML","logspline","readr","data.table","conflicted", "readxl", "writexl", "fs", "fst",
              "purrr", "readr", "here", "furrr", "profvis", "future", "magrittr", "feather", "RStata", "haven")

#Install only those not already installed
# installed <- packages %in% rownames(installed.packages())
# if (any(!installed)) {
#   install.packages(packages[!installed])
# }

lapply(packages, library, character.only = TRUE)

library(plyr)
library(dplyr)

conflicts_prefer(here::here)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::rename)
conflicts_prefer(dplyr::summarize)
conflicts_prefer(dplyr::summarise)
conflicts_prefer(dplyr::count)

#There are four folders needed::
#input data - contains all the MRIP, biological data, angler characteristics data, as well as some data generated in the simulation
#code - contains all the model code
#output_data - this folder is empty to begin with. It stores final simulation output
#iterative_data -this folder is empty to begin with. It compiles data generated in the simulation

#Need to ensure that the globals below are set up in both this file and the stata model_wrapper.do file.


#Set up R globals for input/output data and code scripts
code_cd=here("Code", "sim")
input_data_cd="E:/Lou_projects/groundfishRDM/input_data"
iterative_input_data_cd="E:/Lou_projects/groundfishRDM/process_data"

final_process_data_cd="E:/Lou_projects/groundfishRDM/final_process_data"
final_process_outcomes_cd="E:/Lou_projects/groundfishRDM/final_process_data/base_outcomes"
final_process_choice_occasions_cd="E:/Lou_projects/groundfishRDM/final_process_data/n_choice_occasions"
final_process_misc_cd="E:/Lou_projects/groundfishRDM/final_process_data/miscellaneous"

###################################################
###############Pre-sim Stata code##################
###################################################

#Stata code extracts and prepares the data needed for the simulation

#Connect Rstudio to Stata
#options("RStata.StataPath" = "\"C:\\Program Files\\Stata17\\StataMP-64\"")
#options("RStata.StataVersion" = 17)

#Set number of original draws. We create 125 (in case some don't converge in the calibration), but only use 100 for the final run. Choose a lot fewer for test runs
n_simulations<-201

n_draws<-50 #Number of simulated trips per day

#First, open "$code_cd\model wrapper.do" and set globals:
#a) data years for different datasets
#b) number of draws (ndraws), which should be the same as the object n_simulations above
#c) cd's

#Second, open "$code_cd\set regulations.do" and set regulations for the calibration and projection period.

#Third, run the model wrapper code below:
#stata('do "C:/Users/andrew.carr-harris/Desktop/Git/rdmtool/lou_files/cod_haddock/code/model wrapper.do"')

###################################################




###################################################
###############Simulation R code###################
###################################################

#Notes:

#Simulation stratum are the groups in which we allocate and simulate choice occasions.
#For the 2025 SFSBSB RDM, the stratum is the combination of mode (pr/fh/sh) and state

#Projection results are based on 100 iterations of the model. In each iteration we pull
#in new distributions of catch-per-trip, directed fishing effort, projected catch-at-length,
#and angler preferences. I calibrate the model with 125 iterations, some of which are
#excluded after Step 2. From the pool of remaining iterations, I use the first 100 in the projection.

#Prior to running the model, transfer the catch_draw files from .csv to .feather to reduce computing time
# statez <- c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")
#
# for(s in statez) {
#
#   dtrip0<-read.csv(file.path(iterative_input_data_cd, paste0("directed_trips_calibration_", s,".csv")))
#   write_feather(dtrip0, file.path(iterative_input_data_cd, paste0("directed_trips_calibration_", s,".feather")))
#
#   for(i in 1:n_simulations) {
#     catch<-read_dta(file.path(input_data_cd, paste0("calib_catch_draws_",s, "_", i,".dta")))
#     write_feather(catch, file.path(iterative_input_data_cd, paste0("calib_catch_draws_",s, "_", i,".feather")))
#
#     # make fake projection draws
#     # write_feather(catch, file.path(iterative_input_data_cd, paste0("projected_catch_draws_",s, "_", i,".feather")))
#
#   }
# }



##################### STEP 1 #####################
# Run the calibration algorithm to determine the difference between model-based harvest and MRIP-based harvest.
# I do this for each stratum and each stratum's 125 draws of MRIP trips/catch/harvest.
# This code retains for each stratum the percent/absolute difference between model-based harvest and MRIP-based harvest by species.

MRIP_comparison = read_dta(file.path(input_data_cd,"simulated_catch_totals.dta")) %>%
  dplyr::rename(estimated_trips=tot_dtrip_sim,
                cod_catch=tot_cod_cat_sim,
                hadd_catch=tot_hadd_cat_sim,
                cod_keep=tot_cod_keep_sim,
                hadd_keep=tot_hadd_keep_sim,
                cod_rel=tot_cod_rel_sim,
                hadd_rel=tot_hadd_rel_sim)

#Files needed:

#Scripts needed:


source(file.path(code_cd,"calibrate_rec_catch0.R"))

#Output files:
#calibration_comparison.feather




##################### STEP 2 #####################
#Now, run each stratum's calibration simulation again, but this time allocate discards to harvest,
#or harvest to discards, until the difference between model-based harvest and MRIP-based harvest
#is within abs(5%) or <500 fish.

#If a reallocation of discards to harvest is needed, I reallocate h* percent of all released
# fish that are between [(min. size - 3 inches), min.size] as harvest.

#If a reallocation of harvest to discards is needed, I reallocate h* percent of all harvested fish as discards

#Note that in some iterations, the difference in harvest between the model and MRIP from Step 1 is too large relative to the number of
#fish discarded in the model. So even if I allocate all discards as harvest, the percent difference in harvest
#between the model and MRIP will not be within abs(5%) or <500 fish. The code in Step 2 identifies and drops these iterations.

#This script saves calibration output, as well as the proportion of choice occasions in which we reallocate discards as harvest,
#or vice versa.

#Files needed:

#Scripts needed:
#calibration_catch_weights.R - can be commented out to save time if calibration catch weight are not needed.

source(file.path(code_cd,"calibration_routine.R"))

#Output files:
#calibration_comparison.rds
#calibration_catch_weights_cm.xlsx
#paste0("pds_new_", i,".rds")), where i is an indicator for a domain-draw combination
#paste0("costs_", i,".rds"))), where i is an indicator for a domain-draw combination


# Filter out model iterations that did not converge on harvest for both species
converged<-fst::read_fst(file.path(iterative_input_data_cd, "calibrated_model_stats_raw.fst")) %>%
  dplyr::mutate(abs_pct_diff_keep=abs(pct_diff_keep),
                abs_diff_keep=abs(diff_keep)) %>%
  dplyr::filter(abs_pct_diff_keep<5 | abs_diff_keep<500) %>%
  dplyr::group_by(draw) %>%
  dplyr::count() %>%
  dplyr::filter(n==8) %>%
  dplyr::select(draw)

converged<-converged %>%
  dplyr::ungroup() %>%
  dplyr::mutate(good_draw=1,
                draw2 = dplyr::row_number())

# Now select from the projection input data only the "good draws", i.e., model iterations that
# converged on harvest for both species
  # input data files for projections:
    # directed_trip_draws.csv
    # calibrated_model_stats_raw.rds
    # base_outcomes_"season"_"md"_"dr".fst
    # n_choice_occasions_"season"_"md"_"dr".fst
    # next year calendar adjustments.csv

# directed trips
directed_trips<-read_csv(file.path(iterative_input_data_cd,"directed_trip_draws.csv"), show_col_types = FALSE) %>%
  dplyr::left_join(converged, by="draw") %>%
  dplyr::filter(!is.na(good_draw)) %>%
  dplyr::select(-draw) %>%
  dplyr::rename(draw=draw2)
#write_csv(directed_trips, file.path(final_process_misc_cd, paste0("directed_trip_draws_final.csv")))
fst::write_fst(directed_trips, file.path(final_process_misc_cd, paste0("directed_trip_draws_final.fst")))

# calibration model stats
calib_stats<-read_fst(file.path(iterative_input_data_cd,"calibrated_model_stats_raw.fst")) %>%
  dplyr::left_join(converged, by="draw") %>%
  dplyr::filter(!is.na(good_draw)) %>%
  dplyr::select(-draw) %>%
  dplyr::rename(draw=draw2)
#write_csv(calib_stats, file.path(final_process_misc_cd, paste0("calibrated_model_stats_final.csv")))
fst::write_fst(calib_stats, file.path(final_process_misc_cd, paste0("calibrated_model_stats_final.fst")))

# calibration model stats
calendar_adj<-read_csv(file.path(input_data_cd,"next year calendar adjustments.csv"), show_col_types = FALSE) %>%
  dplyr::left_join(converged, by="draw") %>%
  dplyr::filter(!is.na(good_draw)) %>%
  dplyr::select(-draw) %>%
  dplyr::rename(draw=draw2)
#write_csv(calendar_adj, file.path(final_process_misc_cd, paste0("calendar_adj_final.csv")))
fst::write_fst(calendar_adj, file.path(final_process_misc_cd, paste0("calendar_adj_final.fst")))

# Baseline year outcomes and number of choice occasions
mode_draw <- c("pr", "fh")
season_draw <- c("summer", "winter")
for(dr in 1:n_simulations){
  for (md in mode_draw) {
    for(s in season_draw) {

      good_draws<-converged %>%
        dplyr::filter(draw2==dr)

      draw_orig<-mean(good_draws$draw)

      # pull trip outcomes from the calibration year
      base_outcomes_in<-fst::read_fst(file.path(iterative_input_data_cd, paste0("base_outcomes_", s, "_", md, "_", draw_orig, ".fst"))) %>%
        data.table::as.data.table()

      write_fst(base_outcomes_in, file.path(final_process_outcomes_cd, paste0("base_outcomes_final_", s, "_", md, "_", dr, ".fst")))

      # pull in data on the number of choice occasions per mode-day
      n_choice_occasions_in<-fst::read_fst(file.path(iterative_input_data_cd, paste0("n_choice_occasions_", s, "_", md, "_", draw_orig, ".fst"))) %>%
        data.table::as.data.table()

      write_fst(n_choice_occasions_in, file.path(final_process_choice_occasions_cd, paste0("n_choice_occasions_final_", s, "_", md, "_", dr, ".fst")))
    }
  }

}

# catch at length - save as .fst as well as .csv to pull back into stata for computing projected catch at length
catch_at_length<-read_csv(file.path(input_data_cd,"baseline_catch_at_length.csv"), show_col_types = FALSE) %>%
  dplyr::left_join(converged, by="draw") %>%
  dplyr::filter(!is.na(good_draw)) %>%
  dplyr::select(-draw) %>%
  dplyr::rename(draw=draw2)
write_csv(catch_at_length, file.path(final_process_misc_cd, paste0("baseline_catch_at_length.csv")))
fst::write_fst(catch_at_length, file.path(final_process_misc_cd, paste0("baseline_catch_at_length.fst")))


# projected catch at length - save as .fst as well as .csv to pull back into stata for computing projected catch at length
proj_catch_at_length<-read_csv(file.path(input_data_cd,"projected_catch_at_length.csv"), show_col_types = FALSE) %>%
  dplyr::left_join(converged, by="draw") %>%
  dplyr::filter(!is.na(good_draw)) %>%
  dplyr::select(-draw) %>%
  dplyr::rename(draw=draw2)
fst::write_fst(proj_catch_at_length, file.path(final_process_misc_cd, paste0("proj_catch_at_length.fst")))


#re-save other input files as .fst
# discard mortality
disc_mort<- readr::read_csv(file.path(input_data_cd, "Discard_Mortality.csv"), show_col_types = FALSE)
write_fst(disc_mort, file.path(final_process_misc_cd, paste0("Discard_Mortality.fst")))



##################### STEP 3 #####################
#Run the projection algorithm. This algorithm pulls in population-adjusted catch-at-length distributions and allocates
#fish discarded as harvest or vice versa in proportion to how they were allocated in the calibration.

source(file.path(code_cd, "predict_rec_catch_data_read.R"))
source(file.path(code_cd, "predict_rec_catch_data_functions.R"))
source(file.path(code_cd, "predict_rec_catch.R"))

#Output files:
# predictions.xlsx - output by mode, season, and draw









