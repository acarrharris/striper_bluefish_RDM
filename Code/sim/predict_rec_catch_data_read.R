
#Local loop for prediction

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

n_draws<-50

# Data read for non-shiny run of predict_rec_catch.R
## Run this script prior to predict rec catch

#Lou's repos
#test_code_cd="C:/Users/andrew.carr-harris/Desktop/Git/groundfishRDM/Code/test_code"
input_data_cd="E:/Lou_projects/groundfishRDM/input_data"
iterative_input_data_cd="E:/Lou_projects/groundfishRDM/process_data"

final_process_data_cd="E:/Lou_projects/groundfishRDM/final_process_data"
final_process_outcomes_cd="E:/Lou_projects/groundfishRDM/final_process_data/base_outcomes"
final_process_choice_occasions_cd="E:/Lou_projects/groundfishRDM/final_process_data/n_choice_occasions"
final_process_misc_cd="E:/Lou_projects/groundfishRDM/final_process_data/miscellaneous"

############# To Run Individual
# Variables to change
# s<-"summer"
# md<-"pr"
# dr<-1
#
# mode_draw <- c("pr", "fh")
# draws <- 1:15
# season_draw <- c("summer", "winter")


mode_draw   <- c("pr", "fh")
season_draw <- c("summer", "winter")

param_grid <- expand.grid(
  md = mode_draw,
  s  = season_draw,
  stringsAsFactors = FALSE
)

  ndraws=50 #number of choice occasions to simulate per strata

  #l_w_conversion parameters =
  cod_lw_a = 0.000005132
  cod_lw_b = 3.1625
  had_lw_a = 0.000009298
  had_lw_b = 3.0205

  disc_mort<- fst::read_fst(file.path(final_process_misc_cd, "Discard_Mortality.fst")) %>%
    dplyr::rename(month=Month)


  # Everything below happens within the draw

    directed_trips_draw<-read_fst(file.path(final_process_misc_cd, paste0("directed_trip_draws_final.fst"))) %>%
    tibble::tibble() %>%
    dplyr::select(mode, day,  dtrip, draw,
                  starts_with("cod_bag"), starts_with("cod_min"), starts_with("hadd_bag"),starts_with("hadd_min")) %>%
    dplyr::mutate(date=as.Date(day, format = "%d%b%Y"),
                  season = ifelse(lubridate::month(date) %in% c(9, 10, 11, 12, 1, 2, 3, 4), "winter", "summer")) %>%
    dplyr::filter(draw == dr) %>%
    as.data.table()

  get_lowest_min_size_draw<-read_fst(file.path(final_process_misc_cd, paste0("directed_trip_draws_final.fst"))) %>%
    tibble::tibble() %>%
    dplyr::select(mode, day,  dtrip, draw,
                  starts_with("cod_bag"), starts_with("cod_min"), starts_with("hadd_bag"),starts_with("hadd_min"))

  cod_min_size_FY_draw<-min(get_lowest_min_size_draw$cod_min_y2)
  hadd_min_size_FY_draw<-min(get_lowest_min_size_draw$hadd_min_y2)

  catch_data0 <- list()
  base_outcomes_angler_dems0 <- list()
  n_choice_occasions0 <- list()

  mode_draw <- c("pr", "fh")
  season_draw <- c("summer", "winter")

  k<-1

  for (md in mode_draw) {
    for (s in season_draw){

      catch_data0[[k]] <- fst::read_fst(file.path(final_process_outcomes_cd, paste0("base_outcomes_final_",s, "_", md, "_", dr,".fst"))) %>%
        dplyr::left_join(directed_trips_draw, by=c("mode", "date")) %>%
        dplyr::rename(tot_cod_catch_base = tot_cod_catch,
                      tot_hadd_catch_base = tot_hadd_catch) %>%
        dplyr::mutate(cod_cat=tot_cod_catch_base,
                      hadd_cat=tot_hadd_catch_base) %>%
        dplyr::select(date, mode, draw,  tripid, catch_draw, season,
                      cod_cat, hadd_cat, starts_with("cod_bag"), starts_with("cod_min"),
                      starts_with("hadd_bag"),starts_with("hadd_min")) %>%
        as.data.table()

      base_outcomes_angler_dems0[[k]] <- fst::read_fst(file.path(final_process_outcomes_cd, paste0("base_outcomes_final_",s, "_", md, "_", dr,".fst"))) %>%
        dplyr::select(date, mode,  tripid, catch_draw,
                      tot_keep_cod_base, tot_rel_cod_base,
                      tot_keep_hadd_base, tot_rel_hadd_base,
                      starts_with("beta"),
                      total_trips_12, fish_pref_more, educ1, educ2, educ3, own_boat, cost) %>%
        dplyr::rename(date_parsed=date) %>%
        as.data.table()

      n_choice_occasions0[[k]] <- fst::read_fst(file.path(final_process_choice_occasions_cd, paste0("n_choice_occasions_final_",s, "_", md, "_", dr,".fst"))) %>%
        dplyr::rename(date_parsed=date)  %>%
        as.data.table()

      k<-k+1

    }
  }

  catch_data_draw <- bind_rows(catch_data0)
  base_outcomes_angler_dems_draw <- bind_rows(base_outcomes_angler_dems0)
  n_choice_occasions_draw <- bind_rows(n_choice_occasions0)

  rm(base_outcomes_angler_dems0, n_choice_occasions0, catch_data0)

  # Size data used in projections is "baseline"proj_catch_at_length.fst"
  # For testing, change the size data file to "baseline_catch_at_length.fst"
  cod_size_data_draw <- read_fst(file.path(final_process_misc_cd, "baseline_catch_at_length.fst"))  %>%
    dplyr::filter(species=="cod", draw==dr) %>%
    dplyr::filter(!is.na(fitted_prob)) %>%
    dplyr::select(fitted_prob, length, season) %>%
    as.data.table()

  hadd_size_data_draw <- read_fst(file.path(final_process_misc_cd, "baseline_catch_at_length.fst"))  %>%
    dplyr::filter(species=="hadd", draw==dr) %>%
    dplyr::filter(!is.na(fitted_prob)) %>%
    dplyr::select(fitted_prob, length, season) %>%
    as.data.table()

  calendar_adjustments_draw <- read_fst(file.path(final_process_misc_cd, paste0("calendar_adj_final.fst"))) %>%
    dplyr::filter(draw==dr) %>%
    dplyr::select(-dtrip, -dtrip_y2, -draw, -good_draw) %>%
    as.data.table()

  # Pull in calibration comparison information about trip-level harvest/discard re-allocations
  calib_comparison_draw<-read_fst(file.path(final_process_misc_cd, "calibrated_model_stats_final.fst")) %>%
    dplyr::filter(draw==dr) %>%
    as.data.table()

  calib_comparison_draw<-calib_comparison_draw %>%
    dplyr::rename(n_legal_rel_hadd=n_legal_hadd_rel,
                  n_legal_rel_cod=n_legal_cod_rel,
                  n_sub_kept_hadd=n_sub_hadd_kept,
                  n_sub_kept_cod=n_sub_cod_kept,
                  prop_legal_rel_hadd=prop_legal_hadd_rel,
                  prop_legal_rel_cod=prop_legal_cod_rel,
                  prop_sub_kept_hadd=prop_sub_hadd_kept,
                  prop_sub_kept_cod=prop_sub_cod_kept,
                  convergence_cod=cod_convergence,
                  convergence_hadd=hadd_convergence)

  ##########
  # List of species suffixes
  species_suffixes <- c("cod", "hadd")

  # Get all variable names
  all_vars <- names(calib_comparison_draw)

  # Identify columns that are species-specific (contain _cod or _hadd)
  species_specific_vars <- all_vars[
    str_detect(all_vars, paste0("(_", species_suffixes, ")$", collapse = "|"))
  ]

  id_vars <- setdiff(all_vars, species_specific_vars)

  ## --- build draw-specific inputs ---
  calib_comparison_draw<-calib_comparison_draw %>%
    dplyr::select(mode, season, all_of(species_specific_vars))

  # Extract base variable names (without __cod or _hadd)
  base_names <- unique(str_replace(species_specific_vars, "_(cod|hadd)$", ""))

  # Pivot the data longer on the species-specific columns
  calib_comparison_draw <- calib_comparison_draw %>%
    pivot_longer(
      cols = all_of(species_specific_vars),
      names_to = c(".value", "species"),
      names_pattern = "(.*)_(cod|hadd)"
    ) %>%
    dplyr::distinct()

  ## --- make draw-specific inputs visible to simulate_cod/hadd by assigning to global names ---

  cod_size_data           <<- cod_size_data_draw
  hadd_size_data          <<- hadd_size_data_draw
  calendar_adjustments    <<- calendar_adjustments_draw
  calib_comparison        <<- calib_comparison_draw
  directed_trips          <<- directed_trips_draw
  catch_data              <<- catch_data_draw
  base_outcomes_angler_dems <<- base_outcomes_angler_dems_draw
  n_choice_occasions      <<- n_choice_occasions_draw
  cod_min_size_FY         <<- cod_min_size_FY_draw
  hadd_min_size_FY        <<- hadd_min_size_FY_draw



