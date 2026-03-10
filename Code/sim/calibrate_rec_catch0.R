

#This is the calibration-year trip simulation WITHOUT any adjustments for illegal harvest or voluntary release

# s<-"summer"
# md<-"pr"
# i<-3

mode_draw <- c("pr", "fh")
draws <- 1:n_simulations
season_draw <- c("summer", "winter")

# Create an empty list to collect results
calib_comparison <- list()

# Counter for appending to list
k <- 1

# Loop over all combinations
for (s in season_draw) {
  for (md in mode_draw) {
    for (i in draws) {

      # import necessary data
      dtripz<-read_csv(file.path(iterative_input_data_cd,"directed_trip_draws.csv"), show_col_types = FALSE) %>%
        tibble::tibble() %>%
        dplyr::filter(draw == i) %>%
        dplyr::select(mode, day, cod_bag, cod_min, hadd_bag,hadd_min, dtrip) %>%
        dplyr::filter(mode == md) %>%
        dplyr::mutate(date=as.Date(day, format = "%d%b%Y"),
                      season = ifelse(lubridate::month(date) %in% c(9, 10, 11, 12, 1, 2, 3, 4), "winter", "summer")) %>%
        dplyr::filter(season == s)

      catch_data <- haven::read_dta(file.path(iterative_input_data_cd, paste0("calib_catch_draws_", i,".dta"))) %>%
        dplyr::mutate(date=as.Date(day, format = "%d%b%Y"),
                      season = ifelse(lubridate::month(date) %in% c(9, 10, 11, 12, 1, 2, 3, 4), "winter", "summer")) %>%
        dplyr::filter(mode==md) %>%
        dplyr::filter(season==s) %>%
        dplyr::select(-season, -day) %>%
        dplyr::left_join(dtripz, by=c("mode", "date"))

      angler_dems<-catch_data %>%
        dplyr::select(date, mode, tripid, total_trips_12, fish_pref_more, educ1, educ2, educ3, own_boat, cost, starts_with("beta")) %>%
        dplyr::filter(mode==md)

      angler_dems<-dplyr::distinct(angler_dems)

      catch_data<-catch_data %>%
        dplyr::select(-total_trips_12, -fish_pref_more, -educ1, -educ2, -educ3, -own_boat, -cost, -age, -day, -dtrip, starts_with("beta"))

      cod_size_data <- read_csv(file.path(input_data_cd, "baseline_catch_at_length.csv"), show_col_types = FALSE) %>%
        dplyr::filter(species=="cod", draw==i, season==s) %>%
        dplyr::filter(!is.na(fitted_prob)) %>%
        dplyr::select(fitted_prob, length)

      hadd_size_data <- read_csv(file.path(input_data_cd, "baseline_catch_at_length.csv"), show_col_types = FALSE)  %>%
        dplyr::filter(species=="hadd", draw==i, season==s) %>%
        dplyr::filter(!is.na(fitted_prob)) %>%
        dplyr::select(fitted_prob, length)


      ### Begin trip simulation ###

      # subset trips with zero catch, as no size draws are required
      cod_zero_catch <- dplyr::filter(catch_data, cod_cat == 0)
      hadd_zero_catch <- dplyr::filter(catch_data, hadd_cat == 0)

      # Check if there is zero catch for any species and if so, pipe code around keep/release determination
      cod_catch_check<-base::sum(catch_data$cod_cat)
      hadd_catch_check<-base::sum(catch_data$hadd_cat)


      # cod trip simulation
      if (cod_catch_check!=0){

        #keep trips with positive cod catch
        cod_catch_data <- dplyr::filter(catch_data, cod_cat > 0)

        row_inds <- seq_len(nrow(cod_catch_data))

        cod_catch_data<-cod_catch_data %>%
          dplyr::slice(rep(row_inds, cod_cat))   %>%
          dplyr::mutate(fishid=dplyr::row_number())

        # generate lengths for each fish
        catch_size_data <- cod_catch_data %>%
          dplyr::mutate(fitted_length = sample(cod_size_data$length,
                                               nrow(.),
                                               prob = cod_size_data$fitted_prob,
                                               replace = TRUE))

        # Impose regulations, calculate keep and release per trip
        catch_size_data <- catch_size_data %>%
          dplyr::mutate(posskeep = ifelse(fitted_length>=cod_min ,1,0)) %>%
          dplyr::group_by(tripid, date, mode, catch_draw) %>%
          dplyr::mutate(csum_keep = cumsum(posskeep)) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(
            keep_adj = dplyr::case_when(
              cod_bag > 0 ~ ifelse(csum_keep<=cod_bag & posskeep==1,1,0),
              TRUE ~ 0))

        catch_size_data <- catch_size_data %>%
          dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)

        catch_size_data <- catch_size_data %>%
          dplyr::mutate(keep = keep_adj,
                        release = ifelse(keep==0,1,0)) %>%
          dplyr::select(fishid, fitted_length, tripid, keep, release, date, catch_draw, mode)

        trip_data <- catch_size_data %>%
          dplyr::group_by(date, catch_draw, tripid, mode) %>%
          dplyr::summarize(tot_keep_cod_new = sum(keep),
                           tot_rel_cod_new = sum(release),
                           .groups = "drop") %>%
          dplyr::ungroup()

        cod_zero_catch<-cod_zero_catch %>%
          dplyr::select(date, catch_draw, tripid, mode) %>%
          dplyr::mutate(tot_keep_cod_new=0,
                        tot_rel_cod_new=0)

        cod_trip_data <- dplyr::bind_rows(trip_data, cod_zero_catch) %>%
          dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>%
          dplyr::select(c("date", "catch_draw","tripid","mode",
                          "tot_keep_cod_new","tot_rel_cod_new"))

        cod_trip_data<- cod_trip_data %>% dplyr::mutate(domain2 = paste0(date, "_", mode, "_", catch_draw, "_", tripid))
        cod_trip_data<-data.table::as.data.table(cod_trip_data)
        data.table::setkey(cod_trip_data, "domain2")
      }

      if (cod_catch_check==0){
        cod_trip_data<-catch_data %>%
          dplyr::select("date", "catch_draw","tripid","mode") %>%
          dplyr::mutate(tot_keep_cod_new = 0,
                        tot_rel_cod_new= 0,
                        domain2 = paste0(date, "_", mode, "_", catch_draw, "_", tripid))

        cod_trip_data<-data.table::as.data.table(cod_trip_data)
        data.table::setkey(cod_trip_data, "domain2")
      }



      # hadd trip simulation
      if (hadd_catch_check!=0){

        # keep trips with positive hadd catch
        hadd_catch_data <- dplyr::filter(catch_data, hadd_cat > 0)

        row_inds <- seq_len(nrow(hadd_catch_data))

        hadd_catch_data<-hadd_catch_data %>%
          dplyr::slice(rep(row_inds, hadd_cat))   %>%
          dplyr::mutate(fishid=dplyr::row_number())

        # generate lengths for each fish
        catch_size_data <- hadd_catch_data %>%
          dplyr::mutate(fitted_length = sample(hadd_size_data$length,
                                               nrow(.),
                                               prob = hadd_size_data$fitted_prob,
                                               replace = TRUE))


        # Impose regulations, calculate keep and release per trip
        catch_size_data <- catch_size_data %>%
          dplyr::mutate(posskeep = ifelse(fitted_length>=hadd_min ,1,0)) %>%
          dplyr::group_by(tripid, date, mode, catch_draw) %>%
          dplyr::mutate(csum_keep = cumsum(posskeep)) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(
            keep_adj = dplyr::case_when(
              hadd_bag > 0 ~ ifelse(csum_keep<=hadd_bag & posskeep==1,1,0),
              TRUE ~ 0))

        catch_size_data <- catch_size_data %>%
          dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)

        catch_size_data <- catch_size_data %>%
          dplyr::mutate(keep = keep_adj,
                        release = ifelse(keep==0,1,0)) %>%
          dplyr::select(fishid, fitted_length, tripid, keep, release, date, catch_draw, mode)

        trip_data <- catch_size_data %>%
          dplyr::group_by(date, catch_draw, tripid, mode) %>%
          dplyr::summarize(tot_keep_hadd_new = sum(keep),
                           tot_rel_hadd_new = sum(release),
                           .groups = "drop") %>%
          dplyr::ungroup()

        hadd_zero_catch<-hadd_zero_catch %>%
          dplyr::select(date, catch_draw, tripid, mode) %>%
          dplyr::mutate(tot_keep_hadd_new=0,
                        tot_rel_hadd_new=0)

        hadd_trip_data <- dplyr::bind_rows(trip_data, hadd_zero_catch) %>%
          dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>%
          dplyr::select(c("date", "catch_draw","tripid","mode",
                          "tot_keep_hadd_new","tot_rel_hadd_new"))

        hadd_trip_data<- hadd_trip_data %>%
          dplyr::mutate(domain2 = paste0(date, "_", mode, "_", catch_draw, "_", tripid)) %>%
          dplyr::select(-c("date", "catch_draw","tripid","mode"))

        hadd_trip_data<-data.table::as.data.table(hadd_trip_data)
        data.table::setkey(hadd_trip_data, "domain2")
      }

      if (hadd_catch_check==0){
        hadd_trip_data<-catch_data %>%
          dplyr::select("date", "catch_draw","tripid","mode") %>%
          dplyr::mutate(tot_keep_hadd_new = 0,
                        tot_rel_hadd_new= 0,
                        domain2 = paste0(date, "_", mode, "_", catch_draw, "_", tripid)) %>%
          dplyr::select(-c("date", "catch_draw","tripid","mode"))

        hadd_trip_data<-data.table::as.data.table(hadd_trip_data)
        data.table::setkey(hadd_trip_data, "domain2")
      }

      # merge the hadd trip data with the rest of the trip data
      trip_data<- cod_trip_data[hadd_trip_data, on = "domain2"]

      trip_data<- trip_data %>%
        dplyr::mutate(tot_hadd_catch = tot_keep_hadd_new + tot_rel_hadd_new,
                      tot_cod_catch = tot_keep_cod_new + tot_rel_cod_new)

      # merge the trip data with angler demographics
      trip_data<- trip_data %>%
        dplyr::left_join(angler_dems, by = c("date", "mode", "tripid"))

      # base_outcomes_s_i data sets will retain trip outcomes from the baseline scenario.
      # We will merge these data to the prediction year outcomes to calculate changes in effort and CS.
      baseline_outcomes<- trip_data %>%
        dplyr::rename(tot_keep_hadd_base = tot_keep_hadd_new,
                      tot_keep_cod_base = tot_keep_cod_new,
                      tot_rel_hadd_base = tot_rel_hadd_new,
                      tot_rel_cod_base = tot_rel_cod_new)

      #  compute utility
      trip_data <-trip_data %>%
        dplyr::mutate(
          vA = beta_sqrt_cod_keep*sqrt(tot_keep_cod_new) +
            beta_sqrt_cod_release*sqrt(tot_rel_cod_new) +
            beta_sqrt_hadd_keep*sqrt(tot_keep_hadd_new) +
            beta_sqrt_hadd_release*sqrt(tot_rel_hadd_new) +
            beta_sqrt_cod_hadd_keep*(sqrt(tot_keep_cod_new)*sqrt(tot_keep_hadd_new)) +
            beta_cost*cost)


      mean_trip_data <- trip_data %>% data.table::data.table() %>%
        .[, group_index := .GRP, by = .(date, mode, catch_draw, tripid)]

      # Now expand the data to create two alternatives, representing the alternatives available in choice survey
      mean_trip_data <- mean_trip_data %>%
        dplyr::mutate(n_alt = rep(2,nrow(.))) %>%
        tidyr::uncount(n_alt) %>%
        dplyr::mutate(alt = rep(1:2,nrow(.)/2),
                      opt_out = ifelse(alt == 2, 1, 0))

      #Calculate the expected utility of alts 2 parameters of the utility function,
      #put the two values in the same column, exponentiate, and calculate their sum (vA_col_sum)

      setDT(mean_trip_data)

      # Filter only alt == 2 once, and calculate vA
      mean_trip_data[alt == 2, "vA" := .(
        beta_opt_out * opt_out +
          beta_opt_out_trips12 * (total_trips_12 * opt_out) +
          beta_opt_out_fish_pref * (fish_pref_more * opt_out)+
          beta_opt_out_educ2 * (educ2 * opt_out)+
          beta_opt_out_educ3 * (educ3 * opt_out)+
          beta_opt_out_ownboat * (own_boat * opt_out)
      )]

      # Pre-compute exponential terms
      mean_trip_data[, `:=`(exp_vA = exp(vA))]

      # Group by group_index and calculate probabilities and log-sums
      mean_trip_data[, `:=`(
        prob0 = exp_vA / sum(exp_vA)
      ), by = group_index]


      mean_trip_data<- subset(mean_trip_data, alt==1) %>%
        dplyr::select(-domain2, -group_index, -exp_vA)

      # Get rid of things we don't need.
      mean_trip_data <- mean_trip_data %>%
        dplyr::filter(alt==1) %>%
        dplyr::select(-matches("beta")) %>%
        dplyr::select(-"alt", -"opt_out", -"vA" ,-"cost", -"total_trips_12", -"catch_draw",
                      -"educ1", -"educ2", -"educ3", -"fish_pref_more", -"own_boat")

      all_vars<-c()
      all_vars <- names(mean_trip_data)[!names(mean_trip_data) %in% c("date","mode", "tripid")]
      all_vars

      # average outcomes across draws
      mean_trip_data<-mean_trip_data  %>% as.data.table() %>%
        .[,lapply(.SD, mean), by = c("date","mode", "tripid"), .SDcols = all_vars]

      # multiply the average trip probability (probA) by each catch variable to get probability-weighted catch
      list_names <- c("tot_keep_cod_new","tot_rel_cod_new","tot_cod_catch",
                      "tot_keep_hadd_new","tot_rel_hadd_new", "tot_hadd_catch")

      mean_trip_data <- mean_trip_data %>%
        as.data.table() %>%
        .[,as.vector(list_names) := lapply(.SD, function(x) x * prob0), .SDcols = list_names] %>%
        .[]

      mean_trip_data<-mean_trip_data %>%
        left_join(dtripz, by = c("mode", "date"))

      mean_trip_data <-mean_trip_data %>%
        group_by(mode, date) %>%
        dplyr::mutate(mean_prob=mean(prob0)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(sims=round(dtrip/mean_prob),
                      expand=sims/50, #number of trips per day,
                      n_choice_occasions=1)

      # Expand outcomes
      list_names <- c("tot_keep_cod_new",   "tot_rel_cod_new",  "tot_cod_catch",
                      "tot_keep_hadd_new",  "tot_rel_hadd_new", "tot_hadd_catch",
                      "n_choice_occasions", "prob0" )

      all_vars <- c(list_names)

      mean_trip_data <- mean_trip_data %>%
        data.table::as.data.table() %>%
        .[,as.vector(all_vars) := lapply(.SD, function(x) x * expand), .SDcols = all_vars] %>%
        .[]

      aggregate_trip_data <- mean_trip_data %>%
        data.table::as.data.table() %>%
        .[,lapply(.SD, sum),  by = c("date", "mode"), .SDcols = list_names]

      aggregate_trip_data<-aggregate_trip_data %>%
        dplyr::rename(estimated_trips=prob0,
                      cod_catch=tot_cod_catch,
                      hadd_catch=tot_hadd_catch,
                      cod_keep=tot_keep_cod_new,
                      hadd_keep=tot_keep_hadd_new,
                      cod_rel=tot_rel_cod_new,
                      hadd_rel=tot_rel_hadd_new)

      #saveRDS(aggregate_trip_data, file = paste0(output_data_cd, "calibration_data_", s,"_", i, ".rds"))


      list_names = c("hadd_catch","hadd_keep","hadd_rel",
                     "cod_catch", "cod_keep","cod_rel",
                     "estimated_trips","n_choice_occasions")

      summed_results <- aggregate_trip_data %>%
        data.table::as.data.table() %>%
        .[,lapply(.SD, sum),  by = c("mode"), .SDcols = list_names]


      ########
      #Compare calibration output to MRIP by state-mode

      #Save simulation results by mode as objects
      # Loop over rows (modes)
      for (r in 1:nrow(summed_results)) {
        mode_val <- summed_results$mode[r]

        # Loop over summary columns
        for (var in names(summed_results)[names(summed_results) != "mode"]) {
          value <- summed_results[[var]][r]
          obj_name <- paste0(var, "_", mode_val, "_model")
          assign(obj_name, value)
        }
      }


      #Save MRIP estimates  by mode as objects
      MRIP_comparison_draw <- MRIP_comparison %>%
        dplyr::filter(draw==i & season==s & mode==md)

      mode_val <- MRIP_comparison_draw$mode

      # Loop over summary columns
      for (var in names(MRIP_comparison_draw)[names(MRIP_comparison_draw) != "mode"]) {
        value <- MRIP_comparison_draw[[var]]
        obj_name <- paste0(var, "_", mode_val, "_MRIP")
        assign(obj_name, value)
      }


      species <- c("cod", "hadd")
      dispositions <- c("keep", "rel", "catch")

      compare <- data.frame()

      for (sp in species) {
        for (disp in dispositions) {

          # Construct variable names
          base_name <- paste(sp, disp, md, sep = "_")
          mrip_var <- paste0(base_name, "_MRIP")
          model_var <- paste0(base_name, "_model")

          # Check if both variables exist
          if (exists(mrip_var) && exists(model_var)) {
            # Retrieve values
            mrip_val <- get(mrip_var)
            model_val <- get(model_var)

            # Calculate differences
            diff_val <- model_val - mrip_val
            pct_diff_val <- if (mrip_val != 0)  (diff_val / mrip_val) * 100 else NA
            abs_diff_val <- abs(model_val - mrip_val)
            abs_pct_diff_val <- if (mrip_val != 0)  abs((diff_val / mrip_val) * 100) else NA

            # Create output variable names
            assign(paste0(base_name, "_diff"), diff_val)
            assign(paste0(base_name, "_pctdiff"), pct_diff_val)
            assign(paste0(base_name, "_abs_diff"), abs_diff_val)
            assign(paste0(base_name, "_abs_pctdiff"), abs_pct_diff_val)

            compare <- rbind(compare, data.frame(
              species = sp,
              disposition = disp,
              mode = md,
              MRIP = mrip_val,
              model = model_val,
              diff = diff_val,
              pct_diff = pct_diff_val,
              abs_diff_val= abs_diff_val,
              abs_pct_diff_val= abs_pct_diff_val
            ))
          }

          else {
            warning(paste("Missing variable:", mrip_var, "or", model_var))


          }
        }
      }


      compare<-compare %>%
        dplyr::mutate(rel_to_keep = if_else(diff < 0, 1, 0),
                      keep_to_rel = if_else(diff > 0, 1, 0))

      compare_k<-compare %>%
        dplyr::filter(disposition=="keep") %>%
        dplyr::select(mode, species, MRIP, model, diff, pct_diff, keep_to_rel, rel_to_keep) %>%
        dplyr::rename(MRIP_keep=MRIP, model_keep=model, diff_keep=diff, pct_diff_keep=pct_diff)

      compare_c<-compare %>%
        dplyr::filter(disposition=="catch") %>%
        dplyr::select(mode, species, MRIP, model, diff, pct_diff) %>%
        dplyr::rename(MRIP_catch=MRIP, model_catch=model, diff_catch=diff, pct_diff_catch=pct_diff)

      compare_r<-compare %>%
        dplyr::filter(disposition=="rel") %>%
        dplyr::select(mode, species, MRIP, model, diff, pct_diff) %>%
        dplyr::rename(MRIP_rel=MRIP, model_rel=model, diff_rel=diff, pct_diff_rel=pct_diff) %>%
        dplyr::left_join(compare_k, by=c("mode", "species")) %>%
        dplyr::left_join(compare_c, by=c("mode", "species"))


      calib_comparison[[k]]<-compare_r %>%
        dplyr::mutate(p_rel_to_keep=abs(diff_keep/model_rel),
                      p_keep_to_rel=abs(diff_keep/model_keep),
                      draw=i, season=s)


      k <- k + 1
    }
  }
}

calib_comparison_combined <- do.call(rbind, calib_comparison)

calib_comparison_combined<-calib_comparison_combined %>%
  dplyr::select(season, mode, species, draw, everything())

write_fst(calib_comparison_combined, file.path(iterative_input_data_cd, "calibration_comparison.fst"))



