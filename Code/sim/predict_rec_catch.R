


# Cod trip simulation
# furrr
# results_list <- furrr::future_pmap(param_grid, simulate_cod,
#                                 .options = furrr::furrr_options(seed = TRUE))
#purrr
results_list <- purrr::pmap(param_grid, simulate_cod)

names(results_list) <- paste(param_grid$md, param_grid$s, sep = "_")

trip_data_cod <- rbindlist(lapply(results_list, `[[`, "trip_data"))

data.table::setkey(trip_data_cod, domain2)

zero_catch_cod <- rbindlist(lapply(results_list, `[[`, "zero_catch"))

size_data_cod <- rbindlist(lapply(results_list, `[[`, "size_data"), fill=TRUE) %>%
  dplyr::mutate(dplyr::across(everything(), ~tidyr::replace_na(., 0)))


# Haddock trip simulation
# furrr
# results_list <- furrr::future_pmap(param_grid, simulate_hadd,
#                                      .options = furrr::furrr_options(seed = TRUE))

#purrr
results_list <- purrr::pmap(param_grid, simulate_hadd)

names(results_list) <- paste(param_grid$md, param_grid$s, sep = "_")

trip_data_hadd <- rbindlist(lapply(results_list, `[[`, "trip_data")) %>%
  dplyr::select(-date, -mode, -catch_draw, -tripid)

data.table::setkey(trip_data_hadd, domain2)

zero_catch_hadd <- rbindlist(lapply(results_list, `[[`, "zero_catch"))

size_data_hadd <- rbindlist(lapply(results_list, `[[`, "size_data"), fill=TRUE) %>%
  dplyr::mutate(dplyr::across(everything(), ~tidyr::replace_na(., 0)))

# merge the trip data
trip_data <- merge(trip_data_cod, trip_data_hadd, by = "domain2", all = TRUE)

#replace NAs with zeroes
num_cols_trip <- names(trip_data)[vapply(trip_data, is.numeric, logical(1))]
data.table::setnafill(trip_data, cols = num_cols_trip, fill = 0)

# length data cod and hadd
length_data <- merge(size_data_cod, size_data_hadd,
                     by = c("date", "mode", "tripid", "catch_draw"),
                     all = TRUE)

#First merge cod and hadd zero catches
zero_catch_check<- merge(zero_catch_cod, zero_catch_hadd,
                         by = c("date", "mode", "tripid", "catch_draw"),
                         all = TRUE)[
                           tot_keep_cod_new == 0 & tot_rel_cod_new == 0 &
                             tot_keep_hadd_new == 0 & tot_rel_hadd_new == 0,
                           .(date, mode, tripid, catch_draw)
                         ]


# Bind rows (rbindlist is faster and more memory-efficient)
length_data <- data.table::rbindlist(list(length_data, zero_catch_check), fill = TRUE)


# Replace NA values with 0 again (if necessary)
num_cols_len <- names(length_data)[vapply(length_data, is.numeric, logical(1))]
data.table::setnafill(length_data, cols = num_cols_len, fill = 0)

rm(zero_catch_cod,zero_catch_hadd,zero_catch_check)


trip_data[, date_parsed := lubridate::ymd(date)]
trip_data[, `:=`(
  tot_cat_cod_new  = tot_keep_cod_new + tot_rel_cod_new,
  tot_cat_hadd_new   = tot_keep_hadd_new + tot_rel_hadd_new,
  date = NULL
)]

length_data[, date_parsed := lubridate::ymd(date)][, date := NULL]

trip_data <- trip_data[base_outcomes_angler_dems, on = .(date_parsed, mode, tripid, catch_draw), nomatch = 0L]
trip_data[, domain2 := NULL]

rm(trip_data_cod,trip_data_hadd ,
   size_data_cod, size_data_hadd,
   results_list, catch_data)

# compute utility/choice probabilites/welfare
# Precompute square roots once
trip_data[, `:=`(
  sqrt_keep_cod_new = sqrt(tot_keep_cod_new),
  sqrt_rel_cod_new = sqrt(tot_rel_cod_new),
  sqrt_keep_hadd_new = sqrt(tot_keep_hadd_new),
  sqrt_rel_hadd_new = sqrt(tot_rel_hadd_new),
  sqrt_keep_cod_base = sqrt(tot_keep_cod_base),
  sqrt_rel_cod_base = sqrt(tot_rel_cod_base),
  sqrt_keep_hadd_base = sqrt(tot_keep_hadd_base),
  sqrt_rel_hadd_base = sqrt(tot_rel_hadd_base)
)]

#  utility
# Compute vA and v0
trip_data[, `:=`(
  vA = beta_sqrt_cod_keep*sqrt_keep_cod_new +
    beta_sqrt_cod_release*sqrt_rel_cod_new +
    beta_sqrt_hadd_keep*sqrt_keep_hadd_new +
    beta_sqrt_hadd_release*sqrt_rel_hadd_new +
    beta_sqrt_cod_hadd_keep*(sqrt_keep_cod_new*sqrt_keep_hadd_new) +
    beta_cost*cost,

  v0 = beta_sqrt_cod_keep*sqrt_keep_cod_base +
    beta_sqrt_cod_release*sqrt_rel_cod_base +
    beta_sqrt_hadd_keep*sqrt_keep_hadd_base +
    beta_sqrt_hadd_release*sqrt_rel_hadd_base +
    beta_sqrt_cod_hadd_keep*(sqrt_keep_cod_base*sqrt_keep_hadd_base) +
    beta_cost*cost
)]


# remove the temp sqrt columns to save memory
trip_data[, c("sqrt_keep_cod_new", "sqrt_rel_cod_new", "sqrt_keep_hadd_new", "sqrt_rel_hadd_new",
              "sqrt_keep_cod_base", "sqrt_rel_cod_base", "sqrt_keep_hadd_base", "sqrt_rel_hadd_base") := NULL]


# Expand to 2 alternatives per choice situation
mean_trip_data <- data.table::as.data.table(trip_data)
mean_trip_data[, group_index := .GRP, by = .(date_parsed, mode, catch_draw, tripid)]
mean_trip_data <- mean_trip_data[rep(seq_len(.N), each = 2L)]
mean_trip_data[, alt := rep(1:2, times = .N / 2L)]
mean_trip_data[, opt_out := as.integer(alt == 2L)]


# Calculate the expected utility of alts 2 parameters of the utility function,
# put the two values in the same column, exponentiate, and calculate their sum (vA_col_sum)

# Filter only alt == 2 once, and calculate vA and v0
mean_trip_data[alt == 2, c("vA", "v0") := .(
  beta_opt_out * opt_out +
    beta_opt_out_trips12 * (total_trips_12 * opt_out) +
    beta_opt_out_fish_pref * (fish_pref_more * opt_out)+
    beta_opt_out_educ2 * (educ2 * opt_out)+
    beta_opt_out_educ3 * (educ3 * opt_out)+
    beta_opt_out_ownboat * (own_boat * opt_out)
)]

# Pre-compute exponential terms
mean_trip_data[, `:=`(exp_vA = exp(vA), exp_v0 = exp(v0))]

# Group by group_index and calculate probabilities and log-sums
mean_trip_data[, `:=`(
  probA = exp_vA / sum(exp_vA),
  prob0 = exp_v0 / sum(exp_v0),
  log_sum_alt = log(sum(exp_vA)),
  log_sum_base = log(sum(exp_v0))
), by = group_index]

# Calculate consumer surplus
# mean_trip_data[, `:=`(
#   CS_base = log_sum_base / -beta_cost,
#   CS_alt = log_sum_alt / -beta_cost
# )]

# Calculate change consumer surplus
# Here I take the negative of the CS formula for easeier interpretability of model output
mean_trip_data[, `:=`(
  CV = -(1/beta_cost)*(log_sum_alt - log_sum_base)
)]

# Get rid of things we don't need.
mean_trip_data <- mean_trip_data %>%
  dplyr::filter(alt==1) %>%
  dplyr::select(-matches("beta")) %>%
  dplyr::select(-"alt", -"opt_out", -"vA" , -"v0",-"exp_v0", -"exp_vA",
                -"cost", -"total_trips_12", -"catch_draw", -"group_index",
                -"log_sum_alt", -"log_sum_base",  -"educ1", -"educ2", -"educ3",
                -"fish_pref_more", -"own_boat",
                -"tot_keep_cod_base",  -"tot_rel_cod_base",
                -"tot_keep_hadd_base",  -"tot_rel_hadd_base")

all_vars<-c()
all_vars <- names(mean_trip_data)[!names(mean_trip_data) %in% c("date_parsed","mode", "tripid")]

# average outcomes across draws
mean_trip_data<-mean_trip_data  %>%
  .[,lapply(.SD, mean), by = c("date_parsed","mode", "tripid"), .SDcols = all_vars]

# multiply the average trip probability in the new scenario (probA) by each catch variable to get probability-weighted catch
list_names <- c("tot_keep_cod_new",   "tot_rel_cod_new",  "tot_cat_cod_new",
                "tot_keep_hadd_new",  "tot_rel_hadd_new", "tot_cat_hadd_new")

all_vars <- c(list_names)

mean_trip_data <- mean_trip_data %>%
  .[,as.vector(all_vars) := lapply(.SD, function(x) x * probA), .SDcols = all_vars] %>%
  .[]


## select the same number of choice occasions in the prediction year as in the calibration year
# We will multiply each simulated choice equation by an appropriate expansion factor,
# then multiply this expansion factor by the projection-year calendar adjustment to account for
# different numbers of weekend vs. weekday in the projection year versus the calibration

setDT(n_choice_occasions)
setDT(calendar_adjustments)

mean_trip_data <- n_choice_occasions[mean_trip_data, on = .(mode, date_parsed)]
mean_trip_data[, month := lubridate::month(date_parsed)]
mean_trip_data <- calendar_adjustments[mean_trip_data, on = .(mode, month)]

num_cols <- names(mean_trip_data)[vapply(mean_trip_data, is.numeric, logical(1))]
data.table::setnafill(mean_trip_data, cols = num_cols, fill = 0)

mean_trip_data[, `:=`(
  n_choice_occasions0 = n_choice_occasions,
  estimated_trips0    = estimated_trips
)]

mean_trip_data[, `:=`(
  n_choice_occasions = n_choice_occasions0 * expansion_factor,
  expand = n_choice_occasions / ndraws
)]


#retain expansion factors by strata to multiply with length data
expansion_factors<-mean_trip_data %>%
  dplyr::select("date_parsed","mode", "tripid", "expand", "probA")

# Expand outcomes for projection year
list_names <- c("tot_keep_cod_new",   "tot_rel_cod_new",  "tot_cat_cod_new",
                "tot_keep_hadd_new",  "tot_rel_hadd_new", "tot_cat_hadd_new",
                "probA", "CV", "prob0")

all_vars <- c(list_names)

mean_trip_data <- mean_trip_data %>%
  .[,as.vector(all_vars) := lapply(.SD, function(x) x * expand), .SDcols = all_vars] %>%
  .[]

#process length data
pattern_vars <- grep("^keep_(cod_|hadd_)[0-9.]*$|^release_(cod_|hadd_)[0-9.]*$",
                     names(length_data), value = TRUE)

setDT(length_data)
setDT(expansion_factors)

length_data <- length_data[
  , lapply(.SD, mean),
  by = .(date_parsed, mode, tripid),
  .SDcols = pattern_vars
]

length_data <- expansion_factors[length_data, on = .(date_parsed, mode, tripid)]

# mulitply length data first by the average probability, then by the expansion factor
length_data[
  , (pattern_vars) := lapply(.SD, function(x) x * probA * expand),
  .SDcols = pattern_vars
]

## Compute welfare and predicted trips
# Aggregate by mode
mean_trip_data <- mean_trip_data %>%
  dplyr::rename(predicted_trips = probA, base_trips = prob0)

# Ensure mean_trip_data is a data.table
data.table::setDT(mean_trip_data)
list_names <- c("CV","predicted_trips", "base_trips")

aggregate_trip_data_mode <- mean_trip_data[, lapply(.SD, sum), by = .(mode), .SDcols = list_names]

# Aggregate for all modes
aggregate_trip_data_allmodes <- mean_trip_data[, lapply(.SD, sum), .SDcols = list_names][
  , mode := "all modes"
]

# Combine and reshape
model_output1 <- data.table::rbindlist(list(aggregate_trip_data_mode, aggregate_trip_data_allmodes), use.names=TRUE)
model_output1_long <- data.table::melt(
  model_output1,
  id.vars = c("mode"),   # keep these as identifiers
  measure.vars = c("CV", "predicted_trips", "base_trips"),
  variable.name = "metric",
  value.name = "value"
)

model_output1_long$species<-"NA"

model_output1_long_base<-model_output1_long %>%
  dplyr::filter(metric=="base_trips") %>%
  dplyr::select(mode, value, species) %>%
  dplyr::rename(value_base=value)

model_output1_long_new<-model_output1_long %>%
  dplyr::filter(metric=="predicted_trips") %>%
  dplyr::select(mode, value, species) %>%
  dplyr::rename(value_new=value) %>%
  dplyr::left_join(model_output1_long_base, by=c("mode", "species")) %>%
  dplyr::mutate(additional_trips=value_new-value_base) %>%
  dplyr::mutate(metric="additional_trips") %>%
  dplyr::select(metric, mode, additional_trips, species) %>%
  dplyr::rename(value=additional_trips)

model_output1_long <- model_output1_long %>%
  dplyr::bind_rows(model_output1_long_new)


# Compute catch weight estimates
# Process length-frequency data

# Select needed columns and add month
length_data1 <- length_data[, c("date_parsed", "mode", pattern_vars), with = FALSE]
length_data1[, month := lubridate::month(date_parsed)]

# Aggregate sums by mode + month
length_data1 <- length_data1[
  , lapply(.SD, sum),
  by = .(mode, month),
  .SDcols = pattern_vars
]

# MELT to long
length_data1 <- data.table::melt(
  length_data1,
  id.vars = c("month", "mode"),
  variable.name = "Var",
  value.name = "number_at_length"
)

# Split Var into keep_release, species, length
length_data1[, c("keep_release", "species", "length") :=
               data.table::tstrsplit(Var, "_", fixed = TRUE)]
length_data1[, length := as.numeric(length)]

# Define spp2
length_data1[
  , spp2 := data.table::fifelse(
    species == "hadd" & length > 50, "had_lg",
    data.table::fifelse(
      species == "hadd" & length <= 50, "had_sm",
      species
    )
  )
]

# Join disc_mort
disc_mort <- data.table::as.data.table(disc_mort)
length_data1 <- disc_mort[length_data1, on = .(month, spp2)]

# Compute weight
length_data1[
  , weight := data.table::fcase(
    species == "cod",  cod_lw_a*length^cod_lw_b,
    species == "hadd", had_lw_a*length^had_lw_b,
    default = NA_real_
  )
]
# Convert to lbs
length_data1[, weight := weight * 2.20462262185]

# Totals
length_data1[, keep_weight := data.table::fifelse(keep_release == "keep",
                                                  number_at_length * weight,
                                                  0)]

length_data1[, release_weight := data.table::fifelse(keep_release == "release",
                                                     number_at_length * weight,
                                                     0)]

length_data1[, keep_numbers := data.table::fifelse(keep_release == "keep",
                                                   number_at_length,
                                                   0)]

length_data1[, release_numbers := data.table::fifelse(keep_release == "release",
                                                      number_at_length,
                                                      0)]

# Discard mortality weight
length_data1[, discmort_weight := data.table::fcase(
  keep_release == "release" & species == "cod", Discard_mortality * number_at_length * weight,
  keep_release == "release" & species == "hadd", Discard_mortality * number_at_length * weight,
  default = 0
)]

# Discard mortality numbers
length_data1[, discmort_number := data.table::fcase(
  keep_release == "release" & species == "cod", Discard_mortality * number_at_length,
  keep_release == "release" & species == "hadd", Discard_mortality * number_at_length,
  default = 0
)]


# Summarise by species, mode
length_data1 <- length_data1[, .(
  keep_numbers = sum(keep_numbers),
  release_numbers = sum(release_numbers),
  keep_weight = sum(keep_weight),
  release_weight = sum(release_weight),
  discmort_weight = sum(discmort_weight),
  discmort_number = sum(discmort_number)
), by = .(species, mode)]


# total removals numbers and weights
length_data1[, removals_weight :=  keep_weight + discmort_weight]
length_data1[, removals_number :=  keep_numbers + discmort_number]

length_data_long <- data.table::melt(
  length_data1,
  id.vars = c("species", "mode"),   # keep these as identifiers
  measure.vars = c("keep_numbers", "release_numbers",
                   "keep_weight", "release_weight",
                   "discmort_weight", "discmort_number",
                   "removals_weight", "removals_number"),
  variable.name = "metric",
  value.name = "value"
)

## Remove NAs
length_data_long <- length_data_long[!is.na(value)]

## Split and classify
length_data_long_all <- length_data_long[, .(value = sum(value)),
                                         by = .(metric, species)]

length_data_long_all[, mode := "all modes"]

## Final bind
length_output <- data.table::rbindlist(list(length_data_long_all, length_data_long) ,
                                       use.names = TRUE,
                                       fill = TRUE)

predictions <- data.table::rbindlist(
  list(length_output, model_output1_long),
  use.names = TRUE,
  fill = TRUE)

predictions[, draw := dr]

#})

return(predictions)




