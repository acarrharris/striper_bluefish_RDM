
#This file pulls in the data from step 1, i.e., the differences between model simulated harvest
#and MRIP estimates of harvest, and re-runs the calibration model but this time adjusts per-trip
#outcomes until simulated harvest in numbers of fish is within 5% or 500 fish of the MRIP estimate.

#Set number of original draws. We use 125 for the final run. Choose a lot fewer for test runs
n_simulations<-201

n_draws<-50 #Number of simulated trips per day

MRIP_comparison = read_dta(file.path(input_data_cd,"simulated_catch_totals.dta")) %>%
  dplyr::rename(estimated_trips=tot_dtrip_sim,
                cod_catch=tot_cod_cat_sim,
                hadd_catch=tot_hadd_cat_sim,
                cod_keep=tot_cod_keep_sim,
                hadd_keep=tot_hadd_keep_sim,
                cod_rel=tot_cod_rel_sim,
                hadd_rel=tot_hadd_rel_sim)


baseline_output0<-fst::read_fst(file.path(iterative_input_data_cd, "calibration_comparison.fst"))


mode_draw <- c("pr", "fh")
draws <- 1:n_simulations
season_draw <- c("summer", "winter")


# s<-"open"
# md<-"pr"
# i<-1

# Create an empty list to collect results
calibrated <- list()
calib_comparison <- list()

# Counter for appending to list
k <- 1

# Loop over all combinations
for (s in season_draw) {
  for (md in mode_draw) {
    for (i in draws) {

      calib_comparison<-fst::read_fst(file.path(iterative_input_data_cd, "calibration_comparison.fst")) %>%
        dplyr::filter(season==s & draw==i & mode==md)

      for (p in 1:nrow(calib_comparison)) {
        sp <- calib_comparison$species[p]

        assign(paste0("rel_to_keep_", sp), calib_comparison$rel_to_keep[p])
        assign(paste0("keep_to_rel_", sp), calib_comparison$keep_to_rel[p])
        assign(paste0("harv_diff_", sp), calib_comparison$diff_keep[p])
        assign(paste0("harv_pct_diff_", sp), calib_comparison$pct_diff_keep[p])

        if (calib_comparison$rel_to_keep[p] == 1) {
          assign(paste0("p_rel_to_keep_", sp), calib_comparison$p_rel_to_keep[p])
          assign(paste0("p_keep_to_rel_", sp), 0)

        }

        if (calib_comparison$keep_to_rel[p] == 1) {
          assign(paste0("p_keep_to_rel_", sp), calib_comparison$p_keep_to_rel[p])
          assign(paste0("p_rel_to_keep_", sp), 0)

        }
      }

      all_keep_to_rel_cod<-case_when(p_keep_to_rel_cod==1~1, TRUE~0)
      all_keep_to_rel_hadd<-case_when(p_keep_to_rel_hadd==1~1, TRUE~0)

      source(file.path(code_cd, "calibrate_rec_catch1.R"))

      for (p in 1:nrow(calib_comparison1)) {
        sp <- calib_comparison1$species[p]

        assign(paste0("MRIP_keep_", sp), calib_comparison1$MRIP_keep[p])
        assign(paste0("model_keep_", sp), calib_comparison1$model_keep[p])
        assign(paste0("harv_diff_", sp), calib_comparison1$diff_keep[p])
        assign(paste0("harv_pct_diff_", sp), calib_comparison1$pct_diff_keep[p])
      }

      message("run ", i, " season ", s, " mode ", md)
      message("model_cod_harv: ", model_keep_cod)
      message("mrip_cod_harv: ", MRIP_keep_cod)
      message("diff_cod_harv: ", harv_diff_cod)
      message("pct_diff_cod_harv: ", harv_pct_diff_cod)
      message("rel_to_keep_cod: ", rel_to_keep_cod)
      message("p_rel_to_keep_cod: ", p_rel_to_keep_cod)
      message("p_keep_to_rel_cod: ", p_keep_to_rel_cod)

      message("model_hadd_harv: ", model_keep_hadd)
      message("mrip_hadd_harv: ", MRIP_keep_hadd)
      message("diff_hadd_harv: ", harv_diff_hadd)
      message("pct_diff_hadd_harv: ", harv_pct_diff_hadd)
      message("rel_to_keep_hadd: ", rel_to_keep_hadd)
      message("p_rel_to_keep_hadd: ", p_rel_to_keep_hadd)
      message("p_keep_to_rel_hadd: ", p_keep_to_rel_hadd)

      cod_achieved<-case_when((abs(harv_diff_cod)<500 | (abs(harv_pct_diff_cod)<5 & !is.na(harv_pct_diff_cod))) ~1, TRUE~0)
      hadd_achieved<-case_when((abs(harv_diff_hadd)<500 | (abs(harv_pct_diff_hadd)<5 & !is.na(harv_pct_diff_hadd))) ~1, TRUE~0)


      # Here I add a non-convergence indicator =1 which artificially deems the run as achieved.
      # This changes to zero (in section [A] below) only in cases when (a) we need to re-allocate fish from kept to released,
      # and (b) there are not enough fish within 3-inches of the minimum size for this re-allocation
      # and thus the simulation will never converge.

      cod_convergence<-1
      hadd_convergence<-1


      while(cod_achieved+hadd_achieved<2){

      # Cod

        # For draws where release_to_keep==1:
          # If baseline cod harvest is less than MRIP, but in a new run
          # cod harvest is greater than MRIP,reduce p_rel_to_keep

        if(cod_achieved!=1){
          if(rel_to_keep_cod==1){
            if(harv_diff_cod>0){
              p_rel_to_keep_cod<-p_rel_to_keep_cod - p_rel_to_keep_cod*.19
            }

            # If baseline cod harvest is less than MRIP, and in the new run
            # cod harvest is still less than MRIP, increase p_rel_to_keep

            if(harv_diff_cod<0) {
              p_rel_to_keep_cod<-p_rel_to_keep_cod + p_rel_to_keep_cod*.21
            }
          }

          # For draws where keep_to_release==1:
           # If in the baseline run, harvest is less than MRIP, but in a new run
           # harvest is greater than MRIP, reduce p_keep_to_rel

          if(keep_to_rel_cod==1 & all_keep_to_rel_cod!=1) {
            if(harv_diff_cod>0){
              p_keep_to_rel_cod<-p_keep_to_rel_cod + p_keep_to_rel_cod*.21
            }
            # If in the baseline run, harvest is less than MRIP, and in the new run
            # harvest is still less than MRIP, increase p_keep_to_rel
            if(harv_diff_cod<0){
              p_keep_to_rel_cod<-p_keep_to_rel_cod - p_keep_to_rel_cod*.19
            }
          }

        }

      # Haddock

        # For draws where release_to_keep==1:
          # If baseline hadd harvest is less than MRIP, but in a new run
          # cod harvest is greater than MRIP,reduce p_rel_to_keep

        if(hadd_achieved!=1){
          if(rel_to_keep_hadd==1){
            if(harv_diff_hadd>0){
              p_rel_to_keep_hadd<-p_rel_to_keep_hadd - p_rel_to_keep_hadd*.19
            }

            # If baseline hadd harvest is less than MRIP, and in the new run
            # hadd harvest is still less than MRIP,increase p_rel_to_keep

            if(harv_diff_hadd<0) {
              p_rel_to_keep_hadd<-p_rel_to_keep_hadd + p_rel_to_keep_hadd*.21
            }
          }

          # For draws where keep_to_release==1
            # If in the baseline run, harvest is less than MRIP, but in a new run
            # harvest is greater than MRIP,reduce p_keep_to_rel

          if(keep_to_rel_hadd==1 & all_keep_to_rel_hadd!=1) {
            if(harv_diff_hadd>0){
              p_keep_to_rel_hadd<-p_keep_to_rel_hadd + p_keep_to_rel_hadd*.21
            }

            # If in the baseline run, harvest is less than MRIP, and in the new run
            # harvest is still less than MRIP,increase p_keep_to_rel

            if(harv_diff_hadd<0){
              p_keep_to_rel_hadd<-p_keep_to_rel_hadd - p_keep_to_rel_hadd*.19
            }
          }

        }

        source(file.path(code_cd, "calibrate_rec_catch1.R"))

        for (p in 1:nrow(calib_comparison1)) {
          sp <- calib_comparison1$species[p]

          assign(paste0("MRIP_keep_", sp), calib_comparison1$MRIP_keep[p])
          assign(paste0("model_keep_", sp), calib_comparison1$model_keep[p])
          assign(paste0("harv_diff_", sp), calib_comparison1$diff_keep[p])
          assign(paste0("harv_pct_diff_", sp), calib_comparison1$pct_diff_keep[p])

        }

        message("run ", i, " season ", s, " mode ", md)
        message("model_cod_harv: ", model_keep_cod)
        message("mrip_cod_harv: ", MRIP_keep_cod)
        message("diff_cod_harv: ", harv_diff_cod)
        message("pct_diff_cod_harv: ", harv_pct_diff_cod)
        message("rel_to_keep_cod: ", rel_to_keep_cod)
        message("p_rel_to_keep_cod: ", p_rel_to_keep_cod)
        message("p_keep_to_rel_cod: ", p_keep_to_rel_cod)

        message("model_hadd_harv: ", model_keep_hadd)
        message("mrip_hadd_harv: ", MRIP_keep_hadd)
        message("diff_hadd_harv: ", harv_diff_hadd)
        message("pct_diff_hadd_harv: ", harv_pct_diff_hadd)
        message("rel_to_keep_hadd: ", rel_to_keep_hadd)
        message("p_rel_to_keep_hadd: ", p_rel_to_keep_hadd)
        message("p_keep_to_rel_hadd: ", p_keep_to_rel_hadd)

        cod_achieved<-case_when((abs(harv_diff_cod)<500 | (abs(harv_pct_diff_cod)<5 & !is.na(harv_pct_diff_cod))) ~1, TRUE~0)
        hadd_achieved<-case_when((abs(harv_diff_hadd)<500 | (abs(harv_pct_diff_hadd)<5 & !is.na(harv_pct_diff_hadd))) ~1, TRUE~0)

        # [A]
        # In some cases, there are not enough fish within 3-inches of the minimum size to re-allocate
        # from kept to release. In this case, p_rel_to_keep_SP will continue to grow unbounded and the
        # simulation will never converge.


        if(rel_to_keep_cod==1 & p_rel_to_keep_cod>1){
          cod_convergence<-0
          cod_achieved<-1
        }

        if(rel_to_keep_hadd==1 & p_rel_to_keep_hadd>1){
          hadd_convergence<-0
          hadd_achieved<-1
        }
      }

      k <- k + 1

      calibrated[[k]] <- calib_comparison1 %>%
        dplyr::mutate(keep_to_rel_cod=keep_to_rel_cod,
                      rel_to_keep_cod=rel_to_keep_cod,
                      p_rel_to_keep_cod=p_rel_to_keep_cod,
                      p_keep_to_rel_cod= p_keep_to_rel_cod,
                      cod_convergence=cod_convergence,

                      keep_to_rel_hadd=keep_to_rel_hadd,
                      rel_to_keep_hadd=  rel_to_keep_hadd,
                      p_rel_to_keep_hadd= p_rel_to_keep_hadd,
                      p_keep_to_rel_hadd= p_keep_to_rel_hadd,
                      hadd_convergence=hadd_convergence,

                      n_sub_cod_kept=n_sub_cod_kept,
                      n_legal_cod_rel=n_legal_cod_rel,
                      prop_sub_cod_kept=prop_sub_cod_kept,
                      prop_legal_cod_rel=prop_legal_cod_rel,

                      n_sub_hadd_kept=n_sub_hadd_kept,
                      n_legal_hadd_rel=n_legal_hadd_rel,
                      prop_sub_hadd_kept=prop_sub_hadd_kept,
                      prop_legal_hadd_rel=prop_legal_hadd_rel)

      fst::write_fst(calibrated[[k]], file.path(iterative_input_data_cd, paste0("calib_metrics_", s,"_", md, "_", i, ".fst")))

    }
  }
}


file_list<-list()
mode_draw <- c("pr", "fh")
season_draw <- c("summer", "winter")
k<-1
for(i in 1:n_simulations){
  for (md in mode_draw) {
    for(s in season_draw) {

      calib_metrics<-fst::read_fst(file.path(iterative_input_data_cd, paste0("calib_metrics_", s,"_", md, "_", i, ".fst")))
      file_list[[k]]<-calib_metrics
      k<-k+1
    }
  }
}

calibrated_combined <- do.call(rbind, file_list)

fst::write_fst(calibrated_combined, file.path(iterative_input_data_cd,"calibrated_model_stats_raw.fst"))








