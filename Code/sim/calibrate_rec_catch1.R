

#This is the calibration-year trip simulation WITH any adjustments for illegal harvest or voluntary release

# Create an empty list to collect results
calib_comparison <- list()

# make some placeholders
n_sub_cod_kept<-0 # number of sublegal sized fish kept
prop_sub_cod_kept<-0 # proportion of the original num. of released fish that were illegally kept (n_sub_scup_kept/total_original_released)
n_legal_cod_rel<-0 # number of legal sized fish released
prop_legal_cod_rel<-0 # proportion of the original num. of kept fish that were voluntarily kept (n_legal_scup_rel/total_original_kept

n_sub_hadd_kept<-0
prop_sub_hadd_kept<-0
n_legal_hadd_rel<-0
prop_legal_hadd_rel<-0


# import necessary data

# Pull in the regulations, but before selecting the season,
# identify the minimum cod size limit for the full FY. Will use this
# to allow sublegal harvest of cod in the closed season

dtripz<-read_csv(file.path(iterative_input_data_cd,"directed_trip_draws.csv"), show_col_types = FALSE) %>%
  tibble::tibble() %>%
  dplyr::filter(draw == i) %>%
  dplyr::select(mode, day, cod_bag, cod_min, hadd_bag,hadd_min, dtrip) %>%
  dplyr::filter(mode == md) %>%
  dplyr::mutate(date=as.Date(day, format = "%d%b%Y"),
                season = ifelse(lubridate::month(date) %in% c(9, 10, 11, 12, 1, 2, 3, 4), "winter", "summer"))

cod_min_size_FY<-min(dtripz$cod_min)
hadd_min_size_FY<-min(dtripz$hadd_min)

dtripz<-dtripz %>%
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


cod_size_data <- read_csv(file.path(input_data_cd, "baseline_catch_at_length.csv"), show_col_types = FALSE)  %>%
  dplyr::filter(species=="cod", draw==i, season==s) %>%
  dplyr::filter(!is.na(fitted_prob)) %>%
  dplyr::select(fitted_prob, length)

hadd_size_data <- read_csv(file.path(input_data_cd, "baseline_catch_at_length.csv"), show_col_types = FALSE)  %>%
  dplyr::filter(species=="hadd", draw==i, season==s) %>%
  dplyr::filter(!is.na(fitted_prob)) %>%
  dplyr::select(fitted_prob, length)


#Create lowest length at which fish may be illegally harvested.
#1) This "floor" (floor_subl_harvest) size will be 3 inches (*2.54 to convert to cm's)
#   below the lowest minimum size across the simulation period
#2) If the fishery is closed for the simulation period, floor_subl_harvest is the
#   lowest minimum size across the FY
#3) I don't think there will be any cases where either species is closed the entire season

floor_subl_cod_harv<-min(dtripz$cod_min)-3*2.54
floor_subl_hadd_harv<-min(dtripz$hadd_min)-3*2.54

if (min(dtripz$cod_min)==100){
  floor_subl_cod_harv<-cod_min_size_FY-3*2.54
}

if (min(dtripz$hadd_min)==100){
  floor_subl_hadd_harv<-hadd_min_size_FY-3*2.54
}

# Begin trip simulation

# subset trips with zero catch, as no size draws are required
cod_zero_catch <- dplyr::filter(catch_data, cod_cat == 0)
hadd_zero_catch <- dplyr::filter(catch_data, hadd_cat == 0)

# Check if there is zero catch for any species and if so, pipe code around keep/release determination
cod_catch_check<-base::sum(catch_data$cod_cat)
hadd_catch_check<-base::sum(catch_data$hadd_cat)

calib_comparison<-fst::read_fst(file.path(iterative_input_data_cd, "calibration_comparison.fst")) %>%
  dplyr::filter(season==s & draw==i & mode==md)

# cod trip simulation
# keep trips with positive cod catch
if (cod_catch_check!=0){

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

  # impose regulations, calculate keep and release per trip
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

  # code for trip level harvest re-allocation
  catch_size_data<- catch_size_data %>%
    dplyr::select(fishid, fitted_length, tripid, keep, release, date, catch_draw, mode) %>%
    dplyr::mutate(subl_harv_indicator=case_when(release==1 & fitted_length>=floor_subl_cod_harv~1,TRUE~0))

  sum_cod_rel<-sum(catch_size_data$release)
  sum_cod_keep<-sum(catch_size_data$keep)

  # reallocate a portion of all releases as kept if needed
  if (rel_to_keep_cod==1 & sum_cod_rel>0){

    catch_size_data_re_allocate<- catch_size_data %>%
      dplyr::filter(subl_harv_indicator==1)

    original_rel_cod<-sum(catch_size_data_re_allocate$release)

    catch_size_data_re_allocate_base<- catch_size_data %>%
      dplyr::filter(subl_harv_indicator==0)

    catch_size_data_re_allocate <- catch_size_data_re_allocate %>%
      dplyr::mutate(uniform=runif(n(), min=0, max=1)) %>%
      dplyr::arrange(uniform) %>%
      dplyr::ungroup()

    n_row_re_allocate<-nrow(catch_size_data_re_allocate)

    n_sub_cod_kept=round(p_rel_to_keep_cod*n_row_re_allocate)

    catch_size_data_re_allocate <- catch_size_data_re_allocate %>%
      dplyr::mutate(fishid2=1:n_row_re_allocate) %>%
      dplyr::mutate(keep_new=case_when(fishid2<=n_sub_cod_kept~1, TRUE~ 0))

    n_rel_cod_kept<-sum(catch_size_data_re_allocate$keep_new)

    catch_size_data_re_allocate <- catch_size_data_re_allocate %>%
      dplyr::mutate(rel_new=case_when(keep_new==0~1, TRUE~ 0)) %>%
      dplyr::select(-keep, -release, -uniform, -fishid2, -uniform) %>%
      dplyr::rename(keep=keep_new, release=rel_new)

    catch_size_data<- rbind.fill(catch_size_data_re_allocate,catch_size_data_re_allocate_base) %>%
      dplyr::select(-subl_harv_indicator)

    n_kept_cod_rel<-0
    prop_sub_cod_kept<-n_rel_cod_kept/original_rel_cod
    n_rel_cod_kept

    rm(catch_size_data_re_allocate,catch_size_data_re_allocate_base)

  }

  # Now reallocate a portion of all keeps as releases if needed
  if (keep_to_rel_cod==1 & sum_cod_keep>0){

    # If all kept must be release, p_keep_to_rel_cod==1
    if (all_keep_to_rel_cod==1){

      n_kept_cod_rel<-sum(catch_size_data$keep)
      prop_legal_cod_rel<-n_kept_cod_rel/sum(catch_size_data$keep)

      catch_size_data<-catch_size_data %>%
        dplyr::mutate(rel_new = keep+release,
                      keep_new = 0) %>%
        dplyr::select(-keep, -release) %>%
        dplyr::rename(release=rel_new,  keep=keep_new)

      n_rel_cod_kept<-0

    }

    #If not all kept must be release, p_keep_to_rel_cod<1
    if (all_keep_to_rel_cod!=1){

      catch_size_data_re_allocate<- catch_size_data %>%
        dplyr::filter(keep==1)

      catch_size_data_re_allocate_base<- catch_size_data %>%
        dplyr::filter(keep==0)

      n_row_re_allocate<-nrow(catch_size_data_re_allocate)

      catch_size_data_re_allocate<-catch_size_data_re_allocate %>%
        dplyr::mutate(uniform=runif(n_row_re_allocate)) %>%
        dplyr::arrange(uniform) %>%
        dplyr::mutate(fishid2=1:n_row_re_allocate)

      n_kept_cod_rel=round(p_keep_to_rel_cod*n_row_re_allocate)

      prop_legal_cod_rel<-n_kept_cod_rel/sum(catch_size_data$keep)

      catch_size_data_re_allocate<-catch_size_data_re_allocate %>%
        dplyr::mutate(rel_new=dplyr::case_when(fishid2<=n_kept_cod_rel~1, TRUE~ 0))

      catch_size_data_re_allocate<-catch_size_data_re_allocate %>%
        dplyr::mutate(keep_new=dplyr::case_when(rel_new==0~1, TRUE~ 0)) %>%
        dplyr::select(-keep, -release, -fishid2, -uniform) %>%
        dplyr::rename(keep=keep_new, release=rel_new)

      sum(catch_size_data$release)
      sum(catch_size_data$keep)

      catch_size_data<-rbind.fill(catch_size_data_re_allocate,catch_size_data_re_allocate_base )

      sum(catch_size_data$release)
      sum(catch_size_data$keep)

      rm(catch_size_data_re_allocate,catch_size_data_re_allocate_base)

      n_rel_cod_kept<-0

    }
  }


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

rm(catch_size_data, trip_data)


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

  # code for trip level harvest re-allocation
  catch_size_data<- catch_size_data %>%
    dplyr::select(fishid, fitted_length, tripid, keep, release, date, catch_draw, mode) %>%
    dplyr::mutate(subl_harv_indicator=case_when(release==1 & fitted_length>=floor_subl_hadd_harv~1,TRUE~0))

  sum_hadd_rel<-sum(catch_size_data$release)
  sum_hadd_keep<-sum(catch_size_data$keep)

  # reallocate a portion of all releases as kept if needed
  if (rel_to_keep_hadd==1 & sum_hadd_rel>0){

    catch_size_data_re_allocate<- catch_size_data %>%
      dplyr::filter(subl_harv_indicator==1)

    original_rel_hadd<-sum(catch_size_data_re_allocate$release)

    catch_size_data_re_allocate_base<- catch_size_data %>%
      dplyr::filter(subl_harv_indicator==0)

    catch_size_data_re_allocate <- catch_size_data_re_allocate %>%
      dplyr::mutate(uniform=runif(n(), min=0, max=1)) %>%
      dplyr::arrange(uniform) %>%
      dplyr::ungroup()

    n_row_re_allocate<-nrow(catch_size_data_re_allocate)

    n_sub_hadd_kept=round(p_rel_to_keep_hadd*n_row_re_allocate)

    catch_size_data_re_allocate <- catch_size_data_re_allocate %>%
      dplyr::mutate(fishid2=1:n_row_re_allocate) %>%
      dplyr::mutate(keep_new=case_when(fishid2<=n_sub_hadd_kept~1, TRUE~ 0))

    n_rel_hadd_kept<-sum(catch_size_data_re_allocate$keep_new)


    catch_size_data_re_allocate <- catch_size_data_re_allocate %>%
      dplyr::mutate(rel_new=case_when(keep_new==0~1, TRUE~ 0)) %>%
      dplyr::select(-keep, -release, -uniform, -fishid2, -uniform) %>%
      dplyr::rename(keep=keep_new, release=rel_new)


    catch_size_data<- rbind.fill(catch_size_data_re_allocate,catch_size_data_re_allocate_base) %>%
      dplyr::select(-subl_harv_indicator)

    n_kept_hadd_rel<-0
    prop_sub_hadd_kept<-n_rel_hadd_kept/original_rel_hadd
    n_rel_hadd_kept

    rm(catch_size_data_re_allocate,catch_size_data_re_allocate_base)

  }

  # reallocate a portion of all keeps as releases if needed
  if (keep_to_rel_hadd==1 & sum_hadd_keep>0){

    # if all kept must be release, p_keep_to_rel_hadd==1
    if (all_keep_to_rel_hadd==1){

      n_kept_hadd_rel<-sum(catch_size_data$keep)
      prop_legal_hadd_rel<-n_kept_hadd_rel/sum(catch_size_data$keep)

      catch_size_data<-catch_size_data %>%
        dplyr::mutate(rel_new = keep+release,
                      keep_new = 0) %>%
        dplyr::select(-keep, -release) %>%
        dplyr::rename(release=rel_new,  keep=keep_new)

      n_rel_hadd_kept<-0

    }

    # if not all kept must be release, p_keep_to_rel_hadd<1
    if (all_keep_to_rel_hadd!=1){

      catch_size_data_re_allocate<- catch_size_data %>%
        dplyr::filter(keep==1)

      catch_size_data_re_allocate_base<- catch_size_data %>%
        dplyr::filter(keep==0)

      n_row_re_allocate<-nrow(catch_size_data_re_allocate)

      catch_size_data_re_allocate<-catch_size_data_re_allocate %>%
        dplyr::mutate(uniform=runif(n_row_re_allocate)) %>%
        dplyr::arrange(uniform) %>%
        dplyr::mutate(fishid2=1:n_row_re_allocate)

      n_kept_hadd_rel=round(p_keep_to_rel_hadd*n_row_re_allocate)
      prop_legal_hadd_rel<-n_kept_hadd_rel/sum(catch_size_data$keep)

      catch_size_data_re_allocate<-catch_size_data_re_allocate %>%
        dplyr::mutate(rel_new=dplyr::case_when(fishid2<=n_kept_hadd_rel~1, TRUE~ 0))

      catch_size_data_re_allocate<-catch_size_data_re_allocate %>%
        dplyr::mutate(keep_new=dplyr::case_when(rel_new==0~1, TRUE~ 0)) %>%
        dplyr::select(-keep, -release, -fishid2, -uniform) %>%
        dplyr::rename(keep=keep_new, release=rel_new)

      sum(catch_size_data$release)
      sum(catch_size_data$keep)

      catch_size_data<-rbind.fill(catch_size_data_re_allocate,catch_size_data_re_allocate_base )

      sum(catch_size_data$release)
      sum(catch_size_data$keep)

      rm(catch_size_data_re_allocate,catch_size_data_re_allocate_base)

      n_rel_hadd_kept<-0

    }
  }

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

rm(catch_size_data,trip_data)


# merge the hadd trip data with the rest of the trip data
trip_data<- cod_trip_data[hadd_trip_data, on = "domain2"]

trip_data<- trip_data %>%
  dplyr::mutate(tot_hadd_catch = tot_keep_hadd_new + tot_rel_hadd_new,
                tot_cod_catch = tot_keep_cod_new + tot_rel_cod_new)

# merge the trip data with angler demograohics
trip_data<- trip_data %>%
  dplyr::left_join(angler_dems, by = c("date", "mode", "tripid"))

# base_outcomes_s_md_i data sets will retain trip outcomes from the baseline scenario.
# we will merge these data to the prediction year outcomes to calculate changes in effort and CS.

baseline_outcomes<- trip_data %>%
  dplyr::rename(tot_keep_hadd_base = tot_keep_hadd_new,
                tot_keep_cod_base = tot_keep_cod_new,
                tot_rel_hadd_base = tot_rel_hadd_new,
                tot_rel_cod_base = tot_rel_cod_new)

#write_rds(baseline_outcomes, file.path(iterative_input_data_cd, paste0("base_outcomes_", s,"_", md, "_", i,".rds")))
fst::write_fst(baseline_outcomes, file.path(iterative_input_data_cd, paste0("base_outcomes_", s,"_", md, "_", i, ".fst")))

#  utility
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

# expand the data to create two alternatives, representing the alternatives available in choice survey
mean_trip_data <- mean_trip_data %>%
  dplyr::mutate(n_alt = rep(2,nrow(.))) %>%
  tidyr::uncount(n_alt) %>%
  dplyr::mutate(alt = rep(1:2,nrow(.)/2),
                opt_out = ifelse(alt == 2, 1, 0))

# calculate the expected utility of alts 2 parameters of the utility function,
# put the two values in the same column, exponentiate, and calculate their sum (vA_col_sum)

setDT(mean_trip_data)

# filter only alt == 2 once, and calculate vA
mean_trip_data[alt == 2, "vA" := .(
  beta_opt_out * opt_out +
    beta_opt_out_trips12 * (total_trips_12 * opt_out) +
    beta_opt_out_fish_pref * (fish_pref_more * opt_out)+
    beta_opt_out_educ2 * (educ2 * opt_out)+
    beta_opt_out_educ3 * (educ3 * opt_out)+
    beta_opt_out_ownboat * (own_boat * opt_out)
)]

# pre-compute exponential terms
mean_trip_data[, `:=`(exp_vA = exp(vA))]

# group by group_index and calculate probabilities and log-sums
mean_trip_data[, `:=`(
  prob0 = exp_vA / sum(exp_vA)
), by = group_index]


mean_trip_data<- subset(mean_trip_data, alt==1) %>%
  dplyr::select(-domain2, -group_index, -exp_vA)

# get rid of things we don't need.
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


# compute how many trips each simulated trip represents
mean_trip_data<-mean_trip_data %>%
  left_join(dtripz, by = c("mode", "date"))

mean_trip_data <-mean_trip_data %>%
  group_by(mode, date) %>%
  dplyr::mutate(mean_prob=mean(prob0)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(sims=round(dtrip/mean_prob),
                expand=sims/50, #number of trips per day,
                n_choice_occasions=1)

# expand trip outcomes
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

list_names = c("hadd_catch","hadd_keep","hadd_rel",
               "cod_catch", "cod_keep","cod_rel",
               "estimated_trips","n_choice_occasions")

summed_results <- aggregate_trip_data %>%
  data.table::as.data.table() %>%
  .[,lapply(.SD, sum),  by = c("mode"), .SDcols = list_names]

aggregate_trip_data<-aggregate_trip_data %>%
  dplyr::select(date, mode, n_choice_occasions, estimated_trips)

# write_rds(aggregate_trip_data, file.path(iterative_input_data_cd, paste0("n_choice_occasions_", s,"_", md, "_", i, ".rds")))
fst::write_fst(aggregate_trip_data, file.path(iterative_input_data_cd, paste0("n_choice_occasions_", s,"_", md, "_", i, ".fst")))

# compare calibration output to MRIP by state-mode

# save simulation results by mode as objects
# loop over rows (modes)

for (r in 1:nrow(summed_results)) {
  mode_val <- summed_results$mode[r]

  # loop over summary columns
  for (var in c("hadd_catch","hadd_keep","hadd_rel","cod_catch","cod_keep","cod_rel")) {
    value <- summed_results[[var]][r]
    obj_name <- paste0(var, "_", "model")
    assign(obj_name, value)
  }
}


# save MRIP estimates by mode as objects
MRIP_comparison_draw <- baseline_output0 %>%
  dplyr::filter(draw==i & season==s & mode==md)

for (p in 1:nrow(MRIP_comparison_draw)) {
  sp <- MRIP_comparison_draw$species[p]

  assign(paste0(sp,"_catch_MRIP" ), MRIP_comparison_draw$MRIP_catch[p])
  assign(paste0(sp,"_keep_MRIP" ), MRIP_comparison_draw$MRIP_keep[p])
  assign(paste0(sp,"_rel_MRIP" ), MRIP_comparison_draw$MRIP_rel[p])

}


species <- c("cod", "hadd")
dispositions <- c("keep", "rel", "catch")

compare1 <- data.frame()

# Initialize a vector to track intermediate variable names
intermediate_vars <- c()

for (sp in species) {
  for (disp in dispositions) {

    # Construct variable names
    base_name <- paste(sp, disp, sep = "_")
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

      # Create variable names and assign them
      diff_name <- paste0(base_name, "_diff")
      pct_diff_name <- paste0(base_name, "_pct_diff")
      abs_diff_name <- paste0(base_name, "_abs_diff")
      abs_pct_diff_name <- paste0(base_name, "_abs_pct_diff")

      assign(diff_name, diff_val)
      assign(pct_diff_name, pct_diff_val)
      assign(abs_diff_name, abs_diff_val)
      assign(abs_pct_diff_name, abs_pct_diff_val)

      # Store names to delete later
      intermediate_vars <- c(intermediate_vars,
                             diff_name, pct_diff_name,
                             abs_diff_name, abs_pct_diff_name)

      compare1 <- rbind(compare1, data.frame(
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

# Remove all intermediate variables created with assign()
rm(list = intermediate_vars)
rm(cod_keep_model, cod_keep_MRIP, cod_catch_model, cod_catch_MRIP, cod_rel_model, cod_rel_MRIP,
   hadd_keep_model, hadd_keep_MRIP, hadd_catch_model, hadd_catch_MRIP, hadd_rel_model, hadd_rel_MRIP)

compare1_k<-compare1 %>%
  dplyr::filter(disposition=="keep") %>%
  dplyr::select(mode, species, MRIP, model, diff, pct_diff) %>%
  dplyr::rename(MRIP_keep=MRIP, model_keep=model, diff_keep=diff, pct_diff_keep=pct_diff)

compare1_r<-compare1 %>%
  dplyr::filter(disposition=="rel") %>%
  dplyr::select(mode, species, MRIP, model, diff, pct_diff) %>%
  dplyr::rename(MRIP_rel=MRIP, model_rel=model, diff_rel=diff, pct_diff_rel=pct_diff) %>%
  dplyr::left_join(compare1_k, by=c("mode", "species"))

compare1_c<-compare1 %>%
  dplyr::filter(disposition=="catch") %>%
  dplyr::select(mode, species, MRIP, model, diff, pct_diff) %>%
  dplyr::rename(MRIP_catch=MRIP, model_catch=model, diff_catch=diff, pct_diff_catch=pct_diff) %>%
  dplyr::left_join(compare1_r, by=c("mode", "species"))

calib_comparison1<-compare1_c %>%
  dplyr::mutate(draw=i, season=s)


# Vector of object names you want to remove
objects_to_remove <- c( "baseline_outcomes",
                       "hadd_catch_data", "hadd_trip_data", "hadd_zero_catch",
                       "cod_catch_data", "cod_trip_data", "cod_zero_catch",
                       "catch_data", "trip_data", "parameters", "mean_trip_data",
                       "results")

# Only remove those that exist
rm(list = objects_to_remove[objects_to_remove %in% ls()], envir = .GlobalEnv)


