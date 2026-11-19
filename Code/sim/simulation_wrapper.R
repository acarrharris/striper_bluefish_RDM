
options(scipen = 999)

packages <- c("tidyr",  "magrittr", "tidyverse", "reshape2", "splitstackshape","doBy","WriteXLS","Rcpp",
              "ggplot2","rlist","fitdistrplus","MASS","psych","rgl","copula","VineCopula","scales",
              "univariateML","logspline","readr","data.table","conflicted", "readxl", "writexl", "fs", "fst",
              "purrr", "readr", "here", "furrr", "profvis", "future", "magrittr", "feather", "RStata", "haven", "arrow")

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
conflicts_prefer(feather::read_feather)

# Data folders
  # input miscellaneous - contains:
      # raw and processed MRIP data
      # biological data (e.g., discard mortality rates, NEFSC trawl survey)
      # angler characteristics and preferences data,
      # scalars to adjust for differing # of weekend/weekday days per month btw calibration year and projection year
  # base_outcomes - stores simulated trip-level outcomes from the calibration
  # n_choice_occasions - stores # of choice occasions per day from calibration
  # calib_catch_draws - stores calibration-year catch-per-trip datasets
  # proj_catch_draws - stores projection-year catch-per-trip datasets

# Set directories consistent with stata model_wrapper.do file.
code_cd=here("Code", "sim")
input_data_cd="E:/Lou_projects/striper_bluefish_RDM/input_data"
iterative_input_data_cd="E:/Lou_projects/striper_bluefish_RDM/process_data"

final_process_data_cd="E:/Lou_projects/striper_bluefish_RDM/final_process_data"
final_process_outcomes_cd="E:/Lou_projects/striper_bluefish_RDM/final_process_data/base_outcomes"
final_process_choice_occasions_cd="E:/Lou_projects/striper_bluefish_RDM/final_process_data/n_choice_occasions"
final_process_misc_cd="E:/Lou_projects/striper_bluefish_RDM/final_process_data/miscellaneous"

# Helpers
cm_to_inch <- function(x) x / 2.54

parse_date_any <- function(x) {
  data.table::as.IDate(as.Date(
    x,
    tryFormats = c("%d%b%Y", "%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y")
  ))
}

n_simulations<-101 # Number of model iterations
n_draws<-50 # Number of simulated trips per day

### MODEL CALIBRATION ###
# Simulation stratum are the groups in which we allocate and simulate choice occasions.
# For the 202X striper/bluefish RDM, the stratum is the combination of mode (pr/fh/sh) and region

# Projection results are based on X iterations of the model. In each iteration we pull
# in new distributions of catch-per-trip, directed fishing effort, projected catch-at-length,
# and angler preferences.

# Transfer some files from .csv to .fst to reduce computing time
statez <- c("CTALL", "DEDERIV", "DENANT", "DEOCN", "MAALL", "MDCHES", "MDOCN", "MDPOT", "MEALL",
            "NCALB", "NCCNTRL", "NCOCN", "NHALL", "NJALL", "NYHUDN", "NYHUDS", "RIALL", "VACHES", "VAOCN", "VAPOTO")

for(s in statez) {

   dtrip0<-read.csv(file.path(iterative_input_data_cd, paste0("directed_trip_draws_", s,".csv"))) %>%
     dplyr::mutate(date_parsed = parse_date_any(date),
                   month=data.table::month(date_parsed)) %>%
     dplyr::select(-date)

   fst::write_fst(dtrip0, file.path(iterative_input_data_cd, paste0("directed_trip_draws_", s,".fst")))

   for(i in 1:n_simulations) {
      catch<-read_dta(file.path(iterative_input_data_cd, paste0("calib_catch_draws_processed_",s,"_", i,".dta"))) %>%
        dplyr::mutate(date_parsed = parse_date_any(date),
                      month=data.table::month(date_parsed)) %>%
        dplyr::select(-date)

      fst::write_fst(catch, file.path(iterative_input_data_cd, paste0("calib_catch_draws_processed_",s,"_", i,".fst")))

     dems<-read_dta(file.path(iterative_input_data_cd, paste0("pref_params_and_dems_",s,"_",i,".dta")))
     fst::write_fst(dems, file.path(iterative_input_data_cd, paste0("pref_params_and_dems_",s,"_", i,".fst")))

  }
}

# STEP 1
# Run the simulation to determine and retain percent/absolute differences between
# model-based harvest and MRIP-based harvest harvest numbers by species.

MRIP_comparison = read_dta(file.path(iterative_input_data_cd,"simulated_catch_totals_final.dta")) %>%
  dplyr::rename(estimated_trips=tot_dtrip_sim,
                striper_catch=tot_str_cat_sim,
                bluefish_catch=tot_blu_cat_sim,
                striper_keep=tot_str_keep_sim,
                bluefish_keep=tot_blu_keep_sim,
                striper_rel=tot_str_rel_sim,
                bluefish_rel=tot_blu_rel_sim)



source(file.path(code_cd,"calibrate_rec_catch0_striper_bluefish.R"))

# Output files:
# calibration_comparison.fst

# STEP 2
# Re-run the simulation, but this time reallocate trip-level discards to harvest or harvest to discards,
# until the difference between model-based harvest and MRIP-based harvest is within abs(5%) or <500 fish.

# If a transfer of discards to harvest is needed, reallocate r* percent of all released
# fish that are between [(min. size - 3 inches), min.size] as harvest. If a transfer of
# harvest to discards is needed, reallocate h* percent of all harvested fish as discards.

# Retain calibrated baseline trip outcomes, n_choice_occasions, calibration statistics (e.g. r*, h*)

source(file.path(code_cd,"calibration_routine_striper_bluefish.R"))

# Output files:
# calibrated_model_stats.fst
# file.path(final_process_choice_occasions_cd,paste0("n_choice_occasions_", s, "_", md, "_", i,".fst"))
# file.path(final_process_outcomes_cd, paste0("base_outcomes_", s, "_", md, "_", i, ".fst"))

### END MODEL CALIBRATION ###

##################### STEP 3 #####################
#Run the projection algorithm. This algorithm pulls in population-adjusted catch-at-length distributions and allocates
#fish discarded as harvest or vice versa in proportion to how they were allocated in the calibration.

source(file.path(code_cd, "project_rec_catch_striper_bluefish_policy_scenarios1.R"))











