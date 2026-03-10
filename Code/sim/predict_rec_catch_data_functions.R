
#Functions for the main loop of predict_rec_catch


########## cod  ##############
simulate_cod <- function(md, s) {
  data.table::setDTthreads(1)
  #Step 2: Reorganize calibration parameters#
  calib_lookup <- calib_comparison %>%
    dplyr::select(mode,season, species, rel_to_keep, keep_to_rel,
                  p_rel_to_keep, p_keep_to_rel,
                  prop_sub_kept, prop_legal_rel) %>%
    tidyr::pivot_wider(
      names_from = species,
      values_from = c(rel_to_keep, keep_to_rel, p_rel_to_keep, p_keep_to_rel, prop_sub_kept, prop_legal_rel),
      names_glue = "{.value}_{species}"
    )

  setDT(calib_lookup)
  #setkey(calib_lookup, mode)

  # Extract calibration parameters
  calib_row <- calib_lookup[mode == md & season==s]

  rel_to_keep_cod     <- calib_row$rel_to_keep_cod
  keep_to_rel_cod     <- calib_row$keep_to_rel_cod
  p_rel_to_keep_cod   <- calib_row$p_rel_to_keep_cod
  p_keep_to_rel_cod   <- calib_row$p_keep_to_rel_cod
  prop_sublegal_kept_cod <- calib_row$prop_sub_kept_cod
  prop_legal_rel_cod     <- calib_row$prop_legal_rel_cod
  all_keep_to_rel_cod <- as.integer(p_keep_to_rel_cod == 1)

  # Filter trip data by mode and season
  directed_trips_sub <- directed_trips[mode == md][season==s]

  # Filter size data by season
  cod_size_data_sub <- cod_size_data[season==s]


  #Create lowest length at which fish may be illegally harvested.
  #1) This "floor" (floor_subl_harvest) size will be 3 inches (*2.54 to convert to cm's)
  #   below the lowest minimum size across the simulation period
  #2) If the fishery is closed for the simulation period, floor_subl_harvest is the
  #   lowest minimum size across the FY
  #3) I don't think there will be any cases where either species is closed the entire season

  #

  floor_subl_cod_harv<-min(directed_trips_sub$cod_min_y2)-3*2.54

  if (min(directed_trips_sub$cod_min_y2)==100){
    floor_subl_cod_harv<-cod_min_size_FY-3*2.54
  }

  # Filter catch data by mode and season
  catch_data_sub <- catch_data[mode == md & season==s]
  cod_catch_check <- sum(catch_data_sub$cod_cat)

  if (cod_catch_check == 0) {

    size_data<-catch_data_sub %>%
      dplyr::select("mode","tripid", "catch_draw","date") %>%
      dplyr::mutate(keep_cod_1=0, release_cod_1=0)

    zero_catch <-data.frame(
      date = character(0),
      catch_draw = numeric(0),
      tripid = numeric(0),
      mode = character(0) ,
      tot_keep_cod_new = numeric(0),
      tot_rel_cod_new = numeric(0)
    )

    return(list(
      trip_data = catch_data_sub[, .(date, catch_draw, tripid, mode,
                                     tot_keep_cod_new = 0L, tot_rel_cod_new = 0L,
                                     domain2 = paste0(date, "_", mode, "_", catch_draw, "_", tripid))],
      zero_catch = zero_catch,
      size_data=size_data))
  }

  if (cod_catch_check != 0) {

    # Expand fish by number caught
    cod_catch_data <- catch_data_sub[cod_cat > 0]
    cod_catch_data <- cod_catch_data[rep(1:.N, cod_cat)]
    cod_catch_data[, fishid := .I]

    # Sample fish lengths
    cod_catch_data[, fitted_length := sample(cod_size_data_sub$length, .N,
                                             prob = cod_size_data_sub$fitted_prob, replace = TRUE)]

    # Identify keepable fish
    cod_catch_data[, posskeep := as.integer(fitted_length >= cod_min_y2)]
    cod_catch_data[, csum_keep := ave(posskeep, tripid, date, mode, catch_draw, FUN = cumsum)]
    cod_catch_data[, keep_adj := as.integer(posskeep == 1 & csum_keep <= cod_bag_y2)]
    cod_catch_data[, `:=`(keep = keep_adj, release = 1L - keep_adj)]
    cod_catch_data[, subl_harv_indicator := as.integer(release == 1 & fitted_length >= floor_subl_cod_harv)]

    # --- Reallocate rel to keep ---
    if (rel_to_keep_cod == 1 && sum(cod_catch_data$release) > 0) {
      sublegal_keeps <- cod_catch_data[subl_harv_indicator == 1]
      base <- cod_catch_data[subl_harv_indicator == 0]

      n_to_keep <- round(prop_sublegal_kept_cod * nrow(sublegal_keeps))
      sublegal_keeps[, uniform := runif(.N)]
      data.table::setorder(sublegal_keeps, uniform)
      sublegal_keeps[, fishid2 := .I]
      sublegal_keeps[, `:=`(
        keep = as.integer(fishid2 <= n_to_keep),
        release = as.integer(fishid2 > n_to_keep)
      )]


      # Drop helper columns *only if they exist*
      cols_to_drop_sub <- intersect(names(sublegal_keeps), c("uniform", "fishid2", "subl_harv_indicator"))
      sublegal_keeps[, (cols_to_drop_sub) := NULL]

      cols_to_drop_base <- intersect(names(base), "subl_harv_indicator")
      base[, (cols_to_drop_base) := NULL]

      cod_catch_data <- data.table::rbindlist(list(sublegal_keeps, base), use.names = TRUE, fill = TRUE)
    }

    # --- Reallocate keep to rel ---
    if (keep_to_rel_cod == 1 && sum(cod_catch_data$keep) > 0) {
      if (all_keep_to_rel_cod == 1) {
        cod_catch_data[, `:=`(release = keep + release, keep = 0L)]
      } else {
        kept <- cod_catch_data[keep == 1]
        base <- cod_catch_data[keep == 0]
        n_to_release <- round(prop_legal_rel_cod * nrow(kept))

        kept[, uniform := runif(.N)]
        data.table::setorder(kept, uniform)
        kept[, fishid2 := .I]
        kept[, `:=`(
          release = as.integer(fishid2 <= n_to_release),
          keep = as.integer(fishid2 > n_to_release)
        )]
        kept[, `:=`(uniform = NULL, fishid2 = NULL)]

        cod_catch_data <- data.table::rbindlist(list(kept, base), use.names = TRUE)
      }
    }

    # --- Append length-specific keep/release summary ---
    cod_catch_data <- data.table::as.data.table(cod_catch_data)

    new_size_data <- cod_catch_data[, .(
      keep = sum(keep),
      release = sum(release)
    ), by = .(mode, date, catch_draw, tripid, fitted_length)]

    keep_size_data <- new_size_data %>%
      dplyr::select(-release) %>%
      tidyr::pivot_wider(
        names_from = fitted_length,
        names_glue = "keep_cod_{fitted_length}",
        names_sort = TRUE,
        values_from = keep,
        values_fill = 0
      )

    release_size_data <- new_size_data %>%
      dplyr::select(-keep) %>%
      tidyr::pivot_wider(
        names_from = fitted_length,
        names_glue = "release_cod_{fitted_length}",
        names_sort = TRUE,
        values_from = release,
        values_fill = 0
      )

    keep_release_size_data <- keep_size_data %>%
      dplyr::left_join(release_size_data, by = c("date", "mode", "tripid", "catch_draw"))


    # Summarize trip-level data
    trip_summary <- cod_catch_data[, .(tot_keep_cod_new = sum(keep), tot_rel_cod_new = sum(release)),
                                   by = .(date, catch_draw, tripid, mode)]

    # Add zero-catch trips
    zero_catch <- catch_data_sub[cod_cat == 0, .(date, catch_draw, tripid, mode)]
    zero_catch[, `:=`(tot_keep_cod_new = 0L, tot_rel_cod_new = 0L)]

    trip_data <- data.table::rbindlist(list(trip_summary, zero_catch))
    trip_data[, domain2 := paste0(date, "_", mode, "_", catch_draw, "_", tripid)]

    output_list<- list(
      trip_data = trip_data,
      zero_catch = zero_catch,
      size_data = keep_release_size_data
    )
    return(output_list)

  }
}

########## haddock ##############
simulate_hadd <- function(md, s) {
  data.table::setDTthreads(1)
  #Step 2: Reorganize calibration parameters#
  calib_lookup <- calib_comparison %>%
    dplyr::select(mode,season, species, rel_to_keep, keep_to_rel,
                  p_rel_to_keep, p_keep_to_rel,
                  prop_sub_kept, prop_legal_rel) %>%
    tidyr::pivot_wider(
      names_from = species,
      values_from = c(rel_to_keep, keep_to_rel, p_rel_to_keep, p_keep_to_rel, prop_sub_kept, prop_legal_rel),
      names_glue = "{.value}_{species}"
    )

  setDT(calib_lookup)
  #setkey(calib_lookup, mode)

  # Extract calibration parameters
  calib_row <- calib_lookup[mode == md & season==s]

  rel_to_keep_hadd     <- calib_row$rel_to_keep_hadd
  keep_to_rel_hadd     <- calib_row$keep_to_rel_hadd
  p_rel_to_keep_hadd   <- calib_row$p_rel_to_keep_hadd
  p_keep_to_rel_hadd   <- calib_row$p_keep_to_rel_hadd
  prop_sublegal_kept_hadd <- calib_row$prop_sub_kept_hadd
  prop_legal_rel_hadd     <- calib_row$prop_legal_rel_hadd
  all_keep_to_rel_hadd <- as.integer(p_keep_to_rel_hadd == 1)

  # Filter trip data by mode and season
  directed_trips_sub <- directed_trips[mode == md][season==s]

  # Filter size data by season
  hadd_size_data_sub <- hadd_size_data[season==s]


  #Create lowest length at which fish may be illegally harvested.
  #1) This "floor" (floor_subl_harvest) size will be 3 inches (*2.54 to convert to cm's)
  #   below the lowest minimum size across the simulation period
  #2) If the fishery is closed for the simulation period, floor_subl_harvest is the
  #   lowest minimum size across the FY
  #3) I don't think there will be any cases where either species is closed the entire season

  floor_subl_hadd_harv<-min(directed_trips_sub$hadd_min_y2)-3*2.54

  if (min(directed_trips_sub$hadd_min_y2)==100){
    floor_subl_hadd_harv<-hadd_min_size_FY-3*2.54
  }

  # Filter catch data by mode and season
  catch_data_sub <- catch_data[mode == md & season==s]
  hadd_catch_check <- sum(catch_data_sub$hadd_cat)

  if (hadd_catch_check == 0) {

    size_data<-catch_data_sub %>%
      dplyr::select("mode","tripid", "catch_draw","date") %>%
      dplyr::mutate(keep_hadd_1=0, release_hadd_1=0)

    zero_catch <-data.frame(
      date = character(0),
      catch_draw = numeric(0),
      tripid = numeric(0),
      mode = character(0) ,
      tot_keep_hadd_new = numeric(0),
      tot_rel_hadd_new = numeric(0)
    )

    return(list(
      trip_data = catch_data_sub[, .(date, catch_draw, tripid, mode,
                                     tot_keep_hadd_new = 0L, tot_rel_hadd_new = 0L,
                                     domain2 = paste0(date, "_", mode, "_", catch_draw, "_", tripid))],
      zero_catch = zero_catch,
      size_data=size_data))
  }

  if (hadd_catch_check != 0) {

    # Expand fish by number caught
    hadd_catch_data <- catch_data_sub[hadd_cat > 0]
    hadd_catch_data <- hadd_catch_data[rep(1:.N, hadd_cat)]
    hadd_catch_data[, fishid := .I]

    # Sample fish lengths
    hadd_catch_data[, fitted_length := sample(hadd_size_data_sub$length, .N,
                                              prob = hadd_size_data_sub$fitted_prob, replace = TRUE)]

    # Identify keepable fish
    hadd_catch_data[, posskeep := as.integer(fitted_length >= hadd_min_y2)]
    hadd_catch_data[, csum_keep := ave(posskeep, tripid, date, mode, catch_draw, FUN = cumsum)]
    hadd_catch_data[, keep_adj := as.integer(posskeep == 1 & csum_keep <= hadd_bag_y2)]
    hadd_catch_data[, `:=`(keep = keep_adj, release = 1L - keep_adj)]
    hadd_catch_data[, subl_harv_indicator := as.integer(release == 1 & fitted_length >= floor_subl_hadd_harv)]

    # --- Reallocate rel to keep ---
    if (rel_to_keep_hadd == 1 && sum(hadd_catch_data$release) > 0) {
      sublegal_keeps <- hadd_catch_data[subl_harv_indicator == 1]
      base <- hadd_catch_data[subl_harv_indicator == 0]

      n_to_keep <- round(prop_sublegal_kept_hadd * nrow(sublegal_keeps))
      sublegal_keeps[, uniform := runif(.N)]
      data.table::setorder(sublegal_keeps, uniform)
      sublegal_keeps[, fishid2 := .I]
      sublegal_keeps[, `:=`(
        keep = as.integer(fishid2 <= n_to_keep),
        release = as.integer(fishid2 > n_to_keep)
      )]


      # Drop helper columns *only if they exist*
      cols_to_drop_sub <- intersect(names(sublegal_keeps), c("uniform", "fishid2", "subl_harv_indicator"))
      sublegal_keeps[, (cols_to_drop_sub) := NULL]

      cols_to_drop_base <- intersect(names(base), "subl_harv_indicator")
      base[, (cols_to_drop_base) := NULL]

      hadd_catch_data <- data.table::rbindlist(list(sublegal_keeps, base), use.names = TRUE, fill = TRUE)
    }

    # --- Reallocate keep to rel ---
    if (keep_to_rel_hadd == 1 && sum(hadd_catch_data$keep) > 0) {
      if (all_keep_to_rel_hadd == 1) {
        hadd_catch_data[, `:=`(release = keep + release, keep = 0L)]
      } else {
        kept <- hadd_catch_data[keep == 1]
        base <- hadd_catch_data[keep == 0]
        n_to_release <- round(prop_legal_rel_hadd * nrow(kept))

        kept[, uniform := runif(.N)]
        data.table::setorder(kept, uniform)
        kept[, fishid2 := .I]
        kept[, `:=`(
          release = as.integer(fishid2 <= n_to_release),
          keep = as.integer(fishid2 > n_to_release)
        )]
        kept[, `:=`(uniform = NULL, fishid2 = NULL)]

        hadd_catch_data <- data.table::rbindlist(list(kept, base), use.names = TRUE)
      }
    }

    # --- Append length-specific keep/release summary ---
    hadd_catch_data <- data.table::as.data.table(hadd_catch_data)

    new_size_data <- hadd_catch_data[, .(
      keep = sum(keep),
      release = sum(release)
    ), by = .(mode, date, catch_draw, tripid, fitted_length)]

    keep_size_data <- new_size_data %>%
      dplyr::select(-release) %>%
      tidyr::pivot_wider(
        names_from = fitted_length,
        names_glue = "keep_hadd_{fitted_length}",
        names_sort = TRUE,
        values_from = keep,
        values_fill = 0
      )

    release_size_data <- new_size_data %>%
      dplyr::select(-keep) %>%
      tidyr::pivot_wider(
        names_from = fitted_length,
        names_glue = "release_hadd_{fitted_length}",
        names_sort = TRUE,
        values_from = release,
        values_fill = 0
      )

    keep_release_size_data <- keep_size_data %>%
      dplyr::left_join(release_size_data, by = c("date", "mode", "tripid", "catch_draw"))


    # Summarize trip-level data
    trip_summary <- hadd_catch_data[, .(tot_keep_hadd_new = sum(keep), tot_rel_hadd_new = sum(release)),
                                    by = .(date, catch_draw, tripid, mode)]

    # Add zero-catch trips
    zero_catch <- catch_data_sub[hadd_cat == 0, .(date, catch_draw, tripid, mode)]
    zero_catch[, `:=`(tot_keep_hadd_new = 0L, tot_rel_hadd_new = 0L)]

    trip_data <- data.table::rbindlist(list(trip_summary, zero_catch))
    trip_data[, domain2 := paste0(date, "_", mode, "_", catch_draw, "_", tripid)]

    output_list<- list(
      trip_data = trip_data,
      zero_catch = zero_catch,
      size_data = keep_release_size_data
    )
    return(output_list)

  }
}
