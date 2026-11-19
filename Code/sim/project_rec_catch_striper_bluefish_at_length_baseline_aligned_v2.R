# Project striped bass / bluefish recreational outcomes from calibrated simulation objects
#
# Purpose
#   Re-run the calibrated striped bass / bluefish simulation under alternate
#   catch-at-length distributions and/or regulations.  By default, the script
#   uses the same baseline_catch_at_length_multi_region.csv, baseline regulations,
#   calibration catch draws, and deterministic region-mode-draw seed used in calibration.
#   With unchanged inputs, fish-level outcomes should reproduce the final calibration run.
#
# Main differences from the cod/haddock projection script
#   * Unit of work is region x draw, with modes processed inside each worker.
#   * Uses the striped bass slot limit: min and max size both bind harvest.
#   * Preserves the calibration accounting convention: keep->release fish remain
#     in keep_util, so utility retains the value of legally keepable fish.
#   * Uses the striper and bluefish length-weight relationships from calibration.
#
# Most efficient places to change regulations locally
#   1. For broad scenario changes, edit apply_regulation_scenario() below.
#      That function modifies the region/date/mode directed-trip table once
#      before the fish-level simulation expands catch into individual fish.
#   2. For alternate catch-at-length distributions, replace or edit the
#      size_lookup object in read_projection_common_inputs_str_blu().
#
# Expected inputs from calibration
#   final_process_misc_cd/calibrated_model_stats_7_9_26.fst
#   input_data_cd/baseline_catch_at_length.csv
#   iterative_input_data_cd/directied_trip_draws_<region>.fst
#   iterative_input_data_cd/calib_catch_draws_processed_<region>_<draw>.fst
#   iterative_input_data_cd/pref_params_and_dems_<region>_<draw>.fst
#   final_process_outcomes_cd/base_outcomes_<region>_<mode>_<draw>.fst
#   final_process_choice_occasions_cd/n_choice_occasions_<region>_<mode>_<draw>.fst

library(data.table)
library(fst)
library(readr)
conflicted::conflicts_prefer(lubridate::month)
# -----------------------------------------------------------------------------
# User-facing controls
# -----------------------------------------------------------------------------

# Set these in your wrapper/local session before sourcing this file, or edit here.
draws <- if (exists("draws")) draws else 1:50
n_simulations <- if (exists("n_simulations")) n_simulations else max(draws)
mode_draw <- if (exists("mode_draw")) mode_draw else c("pr", "fh", "sh")
region_draw <- if (exists("region_draw")) region_draw else NULL
n_catch_draws <- if (exists("n_catch_draws")) n_catch_draws else 50L

# Length-level biological output controls. Lengths are retained in one-inch bins.
retain_length_output <- if (exists("retain_length_output")) retain_length_output else TRUE
length_bin_width_in <- if (exists("length_bin_width_in")) length_bin_width_in else 1
striper_discard_mortality <- 0.09
bluefish_discard_mortality <- 0.094

# Directory defaults are placeholders; override locally before sourcing.
# input_data_cd <- if (exists("input_data_cd")) input_data_cd else "PATH/TO/input_data"
# iterative_input_data_cd <- if (exists("iterative_input_data_cd")) iterative_input_data_cd else "PATH/TO/iterative_input_data"
# final_process_misc_cd <- if (exists("final_process_misc_cd")) final_process_misc_cd else iterative_input_data_cd
# final_process_outcomes_cd <- if (exists("final_process_outcomes_cd")) final_process_outcomes_cd else file.path(iterative_input_data_cd, "base_outcomes")
# final_process_choice_occasions_cd <- if (exists("final_process_choice_occasions_cd")) final_process_choice_occasions_cd else file.path(iterative_input_data_cd, "n_choice_occasions")
projection_output_cd <- if (exists("projection_output_cd")) projection_output_cd else file.path(iterative_input_data_cd, "projection_outputs")
if (!dir.exists(projection_output_cd)) dir.create(projection_output_cd, recursive = TRUE)

# Parallel controls.  multisession works on Windows.
use_parallel <- if (exists("use_parallel")) use_parallel else TRUE
n_workers <- if (exists("n_workers")) n_workers else max(1L, parallel::detectCores(logical = FALSE) - 1L)

# -----------------------------------------------------------------------------
# Helper functions
# -----------------------------------------------------------------------------

cm_to_inch <- function(x) x / 2.54
kg_to_lb <- 2.20462262185

striper_weight_lb <- function(length_in) {
  # ln(W_lb) = -8.185 + 3.027 * ln(total length inches)
  exp(-8.185 + 3.027 * log(length_in))
}

bluefish_weight_lb <- function(length_cm) {
  # W kg = a * FL_cm^b, then kg -> lb.  This matches calibration.
  (0.0000137 * length_cm^2.972) * kg_to_lb
}

parse_date_any <- function(x) {
  data.table::as.IDate(as.Date(
    x,
    tryFormats = c("%d%b%Y", "%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y")
  ))
}

# Must match calibration_routine_striper_bluefish_new1.R exactly.
# Calibration resets this seed before the final catch1 run for each
# region x mode x draw cell, making both length draws and reallocation ranks
# reproducible.
make_cell_seed <- function(region, mode, draw) {
  region_int <- utf8ToInt(as.character(region))
  mode_int <- utf8ToInt(as.character(mode))

  region_num <- sum(region_int * seq_along(region_int))
  mode_num <- sum(mode_int * seq_along(mode_int))

  as.integer(
    100000L +
      region_num * 100L +
      mode_num * 10L +
      as.integer(draw)
  )
}

safe_divide <- function(num, den) {
  data.table::fifelse(is.na(den) | den == 0, NA_real_, num / den)
}

safe_sum_numeric <- function(x) sum(x, na.rm = TRUE)

zero_missing_cols <- function(dt, cols) {
  cols <- intersect(cols, names(dt))
  for (cc in cols) data.table::set(dt, which(is.na(dt[[cc]])), cc, 0)
  invisible(dt)
}

check_required_cols <- function(dt, cols, object_name) {
  missing_cols <- setdiff(cols, names(dt))
  if (length(missing_cols)) {
    stop(object_name, " is missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

wtd_mean_safe <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  den <- sum(w[ok], na.rm = TRUE)
  if (!is.finite(den) || den == 0) return(NA_real_)
  sum(x[ok] * w[ok], na.rm = TRUE) / den
}

calc_prob_trip_3alt <- function(v_trip, v_optout, v_other) {
  m <- pmax(v_trip, v_optout, v_other)
  exp_trip <- exp(v_trip - m)
  exp_optout <- exp(v_optout - m)
  exp_other <- exp(v_other - m)
  exp_trip / (exp_trip + exp_optout + exp_other)
}

logsum_3alt <- function(v_trip, v_optout, v_other) {
  m <- pmax(v_trip, v_optout, v_other)
  m + log(exp(v_trip - m) + exp(v_optout - m) + exp(v_other - m))
}

first_existing_file <- function(path_candidates, required = TRUE) {
  path <- path_candidates[file.exists(path_candidates)][1]
  if (is.na(path) && isTRUE(required)) {
    stop("None of these files exist: ", paste(path_candidates, collapse = "; "), call. = FALSE)
  }
  path
}

# -----------------------------------------------------------------------------
# Scenario controls: edit this function for regulation changes
# -----------------------------------------------------------------------------

apply_regulation_scenario <- function(dtripz,
                                      scenario = list(
                                        name = "baseline",
                                        striper_bag = NULL,
                                        striper_min_in = NULL,
                                        striper_max_in = NULL,
                                        bluefish_bag = NULL,
                                        bluefish_min_in = NULL
                                      )) {
  # This is the most efficient place to change regulations.  dtripz is still a
  # compact date x mode x region table; changing regs here avoids touching the
  # much larger fish-level simulation after expansion.
  out <- data.table::copy(dtripz)

  if (!is.null(scenario$striper_bag)) out[, striper_bag := scenario$striper_bag]
  if (!is.null(scenario$striper_min_in)) out[, striper_min := scenario$striper_min_in * 2.54]
  if (!is.null(scenario$striper_max_in)) out[, striper_max := scenario$striper_max_in * 2.54]

  if (!is.null(scenario$bluefish_bag)) out[, bluefish_bag := scenario$bluefish_bag]
  if (!is.null(scenario$bluefish_min_in)) out[, bluefish_min := scenario$bluefish_min_in * 2.54]

  out[]
}

# draw<-1
# mode_draw<-"pr"
# region_draw<-"CTALL"
# scenario<-"baseline"
projection_scenario <- if (exists("projection_scenario")) projection_scenario else list(name = "baseline")

# -----------------------------------------------------------------------------
# Common inputs
# -----------------------------------------------------------------------------

read_projection_common_inputs_str_blu <- function(final_process_misc_cd,
                                                  input_data_cd,
                                                  mode_draw,
                                                  draws,
                                                  region_draw = NULL) {

  calib_path <- first_existing_file(c(
    file.path(final_process_misc_cd, "calibrated_model_stats_7_9_26.fst"),
    file.path(final_process_misc_cd, "calibrated_model_stats_7_9_26.csv")
  ))

  if (grepl("\\.fst$", calib_path)) {
    calib <- data.table::as.data.table(fst::read_fst(calib_path))
  } else {
    calib <- data.table::fread(calib_path)
  }

  calib[, species := data.table::fifelse(species %in% c("str", "striped_bass", "striped bass"), "striper", species)]
  calib[, species := data.table::fifelse(species %in% c("blu"), "bluefish", species)]
  if (!"all_keep_to_rel" %in% names(calib)) calib[, all_keep_to_rel := as.integer(p_keep_to_rel >= 1)]
  calib[is.na(all_keep_to_rel), all_keep_to_rel := as.integer(p_keep_to_rel >= 1)]

  calib <- calib[mode %in% mode_draw & draw %in% draws]
  if (!is.null(region_draw)) calib <- calib[region %in% region_draw]
  data.table::setkey(calib, region, mode, draw, species)

  size_lookup <- data.table::as.data.table(
    readr::read_csv(file.path(input_data_cd, "baseline_catch_at_length_multi_region.csv"), show_col_types = FALSE)
  )
  check_required_cols(size_lookup, c("region", "draw", "species", "fitted_prob", "length"), "baseline_catch_at_length_multi_region.csv")
  size_lookup[, species := data.table::fifelse(species %in% c("striper", "str", "striped_bass"), "str", species)]
  size_lookup[, species := data.table::fifelse(species %in% c("bluefish", "blu"), "blu", species)]
  size_lookup <- size_lookup[draw %in% draws & !is.na(fitted_prob), .(region, draw, species, fitted_prob, length)]
  if (!is.null(region_draw)) size_lookup <- size_lookup[region %in% region_draw]
  data.table::setkey(size_lookup, region, draw, species)

  if (is.null(region_draw)) region_draw <- sort(unique(calib$region))

  list(calib = calib, size_lookup = size_lookup, region_draw = region_draw)
}


lw_params <- data.table::as.data.table(
  haven::read_dta(file.path(input_data_cd, "mrip_lw_params.dta"))
)

data.table::setnames(
  lw_params,
  old = c("alpha", "beta", "sigma2"),
  new = c("lw_alpha", "lw_beta", "lw_sigma2"),
  skip_absent = TRUE
)

lw_params[, species := as.character(species)]
lw_params[, lw_region := as.character(lw_region)]
# -----------------------------------------------------------------------------
# Species simulator
# -----------------------------------------------------------------------------

simulate_species_project_str_blu <- function(catch_dt,
                                             catch_col,
                                             bag_col,
                                             min_col,
                                             max_col = NULL,
                                             size_dt,
                                             species_name = c("striper", "bluefish"),
                                             floor_sublegal,
                                             rel_to_keep = 0,
                                             keep_to_rel = 0,
                                             p_rel_to_keep = 0,
                                             p_keep_to_rel = 0,
                                             all_keep_to_rel = 0,
                                             region_id,
                                             discard_mortality_rate,
                                             length_bin_width_in = 1,
                                             retain_length_output = TRUE,
                                             length_summary = FALSE) {

  species_name <- match.arg(species_name)
  prefix <- if (species_name == "striper") "str" else "blu"

  if (!is.numeric(discard_mortality_rate) || length(discard_mortality_rate) != 1L ||
      !is.finite(discard_mortality_rate) || discard_mortality_rate < 0 ||
      discard_mortality_rate > 1) {
    stop("discard_mortality_rate must be a finite scalar between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(length_bin_width_in) || length(length_bin_width_in) != 1L ||
      !is.finite(length_bin_width_in) || length_bin_width_in <= 0) {
    stop("length_bin_width_in must be a positive finite scalar.", call. = FALSE)
  }

  empty_length_output <- data.table::data.table(
    date_parsed = as.IDate(character()),
    mode = character(),
    tripid = numeric(),
    catch_draw = integer(),
    species = character(),
    length_bin_in = numeric(),
    length_bin_upper_in = numeric(),
    length_bin_mid_in = numeric(),
    harvest_n = numeric(),
    harvest_weight_lb = numeric(),
    released_n = numeric(),
    released_weight_lb = numeric(),
    dead_discard_n = numeric(),
    dead_discard_weight_lb = numeric()
  )

  key_cols <- c("date_parsed", "mode", "tripid", "catch_draw")
  keep_col <- paste0("tot_keep_", prefix, "_new")
  rel_col <- paste0("tot_rel_", prefix, "_new")
  cat_col <- paste0("tot_cat_", prefix, "_new")
  keep_util_col <- paste0("tot_keep_", prefix, "_util")
  rel_util_col <- paste0("tot_rel_", prefix, "_util")
  keep_wt_col <- paste0("tot_keep_", prefix, "_weight_lb_new")
  rel_wt_col <- paste0("tot_rel_", prefix, "_weight_lb_new")

  check_required_cols(catch_dt, c(key_cols, catch_col, bag_col, min_col), "catch_dt")
  check_required_cols(size_dt, c("length", "fitted_prob"), "size_dt")

  out_zero <- unique(catch_dt[, ..key_cols])
  out_zero[, c(keep_col, rel_col, cat_col, keep_util_col, rel_util_col, keep_wt_col, rel_wt_col) := .(0, 0, 0, 0, 0, 0, 0)]

  if (length_summary) {
    out_zero[, `:=`(
      mean_len_keep_str_in = NA_real_,
      keep_length_str_in = 0,
      keep_length_str_in2 = 0,
      mean_len_rel_str_in = NA_real_,
      rel_log_length_str = 0
    )]
  }

  if (!nrow(catch_dt) || !nrow(size_dt) || sum(size_dt$fitted_prob, na.rm = TRUE) <= 0) {
    data.table::setkeyv(out_zero, key_cols)
    return(list(trip = out_zero[], length = empty_length_output[]))
  }

  pos_dt <- catch_dt[get(catch_col) > 0,
                     .(date_parsed, mode, tripid, catch_draw,
                       catch_n = as.integer(round(get(catch_col))),
                       bag = get(bag_col),
                       min_sz = get(min_col),
                       max_sz = if (!is.null(max_col)) get(max_col) else Inf)]

  if (!nrow(pos_dt)) {
    data.table::setkeyv(out_zero, key_cols)
    return(list(trip = out_zero[], length = empty_length_output[]))
  }

  fish_dt <- pos_dt[rep(seq_len(.N), pmax(catch_n, 0L))]
  fish_dt[, fishid := seq_len(.N)]
  fish_dt[, fitted_length := sample(size_dt$length, .N, replace = TRUE, prob = size_dt$fitted_prob)]
  data.table::setorder(fish_dt, date_parsed, mode, tripid, catch_draw, fishid)

  if (is.null(max_col)) {
    fish_dt[, posskeep := data.table::fifelse(fitted_length >= min_sz, 1L, 0L)]
  } else {
    fish_dt[, posskeep := data.table::fifelse(fitted_length >= min_sz & fitted_length <= max_sz, 1L, 0L)]
  }

  fish_dt[, csum_keep := cumsum(posskeep), by = key_cols]
  fish_dt[, keep := data.table::fifelse(bag > 0 & posskeep == 1L & csum_keep <= bag, 1L, 0L)]
  fish_dt[, release := data.table::fifelse(keep == 1L, 0L, 1L)]
  fish_dt[, kept_to_released_flag := 0L]
  fish_dt[, length_in := cm_to_inch(fitted_length)]

  fish_dt[, species := ifelse(species_name == "striper", "str", "blu")]

  fish_dt[, reg := region_id]
  fish_dt[, state := substr(reg, 1, 2)]

  # fish_dt[, lw_region := data.table::fcase(
  #   state %in% c("ME", "NH", "MA", "RI", "CT", "NY"), "north",
  #   state %in% c("NJ", "DE", "MD", "VA", "NC"), "south",
  #   default = "south"
  # )]
  #
  # fish_dt <- merge(
  #   fish_dt,
  #   lw_params,
  #   by = c("species", "lw_region"),
  #   all.x = TRUE
  # )
  #
  # fish_dt[, fish_weight_lb := exp(
  #   lw_alpha + lw_beta * log(length_in) + 0.5 * lw_sigma2
  # )]

  if (species_name == "striper") {

    fish_dt[, fish_weight_lb := striper_weight_lb(length_in)]

    fish_dt[, rel_to_keep_eligible := data.table::fifelse(
      release == 1L & (fitted_length >= floor_sublegal | fitted_length > max_sz), 1L, 0L
    )]
  } else {

    fish_dt[, fish_weight_lb := bluefish_weight_lb(fitted_length)]
    fish_dt[, rel_to_keep_eligible := data.table::fifelse(
      release == 1L & fitted_length >= floor_sublegal, 1L, 0L
    )]
  }

  if (isTRUE(rel_to_keep == 1) && fish_dt[, sum(rel_to_keep_eligible, na.rm = TRUE)] > 0) {
    realloc_dt <- fish_dt[rel_to_keep_eligible == 1L]
    base_dt <- fish_dt[rel_to_keep_eligible != 1L]

    # Match calibration release-to-keep priority exactly. This step is
    # deterministic; random ordering is retained only for keep-to-release.
    if (species_name == "striper") {
      realloc_dt[, eligibility_group := data.table::fcase(
        fitted_length >= min_sz & fitted_length <= max_sz, 1L,
        fitted_length > max_sz, 2L,
        fitted_length < min_sz, 3L,
        default = 4L
      )]

      data.table::setorder(
        realloc_dt,
        eligibility_group,
        -fitted_length,
        date_parsed,
        mode,
        tripid,
        catch_draw,
        fishid
      )
    } else {
      data.table::setorder(
        realloc_dt,
        -fitted_length,
        date_parsed,
        mode,
        tripid,
        catch_draw,
        fishid
      )
    }

    n_take <- max(0L, min(round(p_rel_to_keep * nrow(realloc_dt)), nrow(realloc_dt)))
    realloc_dt[, idx := seq_len(.N)]
    realloc_dt[idx <= n_take, `:=`(keep = 1L, release = 0L)]

    drop_realloc_cols <- intersect(c("idx", "eligibility_group"), names(realloc_dt))
    realloc_dt[, (drop_realloc_cols) := NULL]

    fish_dt <- data.table::rbindlist(list(realloc_dt, base_dt), use.names = TRUE, fill = TRUE)
  }

  if (isTRUE(keep_to_rel == 1) && fish_dt[, sum(keep, na.rm = TRUE)] > 0) {
    if (isTRUE(all_keep_to_rel == 1)) {
      fish_dt[, kept_to_released_flag := keep]
      fish_dt[, release := release + keep]
      fish_dt[, keep := 0L]
    } else {
      realloc_dt <- fish_dt[keep == 1L]
      base_dt <- fish_dt[keep != 1L]
      realloc_dt[, u := runif(.N)]
      data.table::setorder(realloc_dt, u)
      n_move <- max(0L, min(round(p_keep_to_rel * nrow(realloc_dt)), nrow(realloc_dt)))
      realloc_dt[, idx := seq_len(.N)]
      realloc_dt[idx <= n_move, `:=`(kept_to_released_flag = 1L, release = 1L, keep = 0L)]
      realloc_dt[, c("u", "idx") := NULL]
      fish_dt <- data.table::rbindlist(list(realloc_dt, base_dt), use.names = TRUE, fill = TRUE)
    }
  }

  fish_dt[, keep_util := data.table::fifelse(keep == 1L | kept_to_released_flag == 1L, 1L, 0L)]
  fish_dt[, release_util := data.table::fifelse(release == 1L & kept_to_released_flag == 0L, 1L, 0L)]
  fish_dt[, keep_weight_lb := keep * fish_weight_lb]
  fish_dt[, release_weight_lb := release * fish_weight_lb]

  # Retain biological outcomes by one-inch length bin before trip aggregation.
  # Dead discards are expected mortality among all released fish.
  if (isTRUE(retain_length_output)) {
    fish_dt[, length_bin_in :=
              floor(length_in / length_bin_width_in) * length_bin_width_in]
    fish_dt[, `:=`(
      length_bin_upper_in = length_bin_in + length_bin_width_in,
      length_bin_mid_in = length_bin_in + length_bin_width_in / 2,
      dead_discard_n = release * discard_mortality_rate,
      dead_discard_weight_lb = release * discard_mortality_rate * fish_weight_lb
    )]

    length_out <- fish_dt[, .(
      harvest_n = sum(keep, na.rm = TRUE),
      harvest_weight_lb = sum(keep_weight_lb, na.rm = TRUE),
      released_n = sum(release, na.rm = TRUE),
      released_weight_lb = sum(release_weight_lb, na.rm = TRUE),
      dead_discard_n = sum(dead_discard_n, na.rm = TRUE),
      dead_discard_weight_lb = sum(dead_discard_weight_lb, na.rm = TRUE)
    ), by = c(key_cols, "length_bin_in", "length_bin_upper_in", "length_bin_mid_in")]

    length_out[, species := species_name]
    data.table::setcolorder(
      length_out,
      c(key_cols, "species", "length_bin_in", "length_bin_upper_in",
        "length_bin_mid_in", "harvest_n", "harvest_weight_lb",
        "released_n", "released_weight_lb", "dead_discard_n",
        "dead_discard_weight_lb")
    )
  } else {
    length_out <- empty_length_output
  }

  if (length_summary) {
    trip_pos <- fish_dt[, .(
      keep_n = sum(keep, na.rm = TRUE),
      rel_n = sum(release, na.rm = TRUE),
      keep_util_n = sum(keep_util, na.rm = TRUE),
      rel_util_n = sum(release_util, na.rm = TRUE),
      keep_weight_lb = sum(keep_weight_lb, na.rm = TRUE),
      release_weight_lb = sum(release_weight_lb, na.rm = TRUE),
      mean_len_keep_str_in = data.table::fifelse(sum(keep_util, na.rm = TRUE) > 0, mean(length_in[keep_util == 1L], na.rm = TRUE), NA_real_),
      mean_len_rel_str_in = data.table::fifelse(sum(release_util, na.rm = TRUE) > 0, mean(length_in[release_util == 1L], na.rm = TRUE), NA_real_),
      rel_log_length_str = data.table::fifelse(sum(release_util, na.rm = TRUE) > 0, mean(log(length_in[release_util == 1L]), na.rm = TRUE), 0)
    ), by = key_cols]
    trip_pos[, `:=`(
      keep_length_str_in = data.table::fifelse(keep_util_n > 0 & !is.na(mean_len_keep_str_in), mean_len_keep_str_in, 0),
      keep_length_str_in2 = data.table::fifelse(keep_util_n > 0 & !is.na(mean_len_keep_str_in), mean_len_keep_str_in^2, 0)
    )]
  } else {
    trip_pos <- fish_dt[, .(
      keep_n = sum(keep, na.rm = TRUE),
      rel_n = sum(release, na.rm = TRUE),
      keep_util_n = sum(keep_util, na.rm = TRUE),
      rel_util_n = sum(release_util, na.rm = TRUE),
      keep_weight_lb = sum(keep_weight_lb, na.rm = TRUE),
      release_weight_lb = sum(release_weight_lb, na.rm = TRUE)
    ), by = key_cols]
  }

  trip_pos[, (cat_col) := keep_n + rel_n]
  data.table::setnames(
    trip_pos,
    old = c("keep_n", "rel_n", "keep_util_n", "rel_util_n", "keep_weight_lb", "release_weight_lb"),
    new = c(keep_col, rel_col, keep_util_col, rel_util_col, keep_wt_col, rel_wt_col)
  )

  trip_out <- data.table::rbindlist(list(trip_pos, out_zero[!trip_pos, on = key_cols]), use.names = TRUE, fill = TRUE)
  zero_cols <- c(keep_col, rel_col, cat_col, keep_util_col, rel_util_col, keep_wt_col, rel_wt_col,
                 "keep_length_str_in", "keep_length_str_in2", "rel_log_length_str")
  zero_missing_cols(trip_out, zero_cols)
  data.table::setkeyv(trip_out, key_cols)
  list(trip = trip_out[], length = length_out[])
}
# modes<-"pr"
# md<-"pr"
# -----------------------------------------------------------------------------
# One region x draw worker, all modes
# -----------------------------------------------------------------------------

project_one_str_blu_region_draw <- function(region_id,
                                            dr,
                                            common_inputs,
                                            modes = mode_draw,
                                            scenario = projection_scenario,
                                            n_catch_draws = get("n_catch_draws", envir = .GlobalEnv)) {

  dtrip_path <- file.path(
    iterative_input_data_cd,
    paste0("directed_trip_draws_", region_id, ".fst")
  )

  angler_path <- file.path(
    iterative_input_data_cd,
    paste0("pref_params_and_dems_", region_id, "_", dr, ".fst")
  )

  if (!file.exists(dtrip_path) || !file.exists(angler_path)) {
    warning("Skipping region=", region_id, ", draw=", dr, ": missing dtrip or angler file.")
    return(NULL)
  }

  dtrip_region <- data.table::as.data.table(fst::read_fst(dtrip_path))

  if (!"date_parsed" %in% names(dtrip_region)) {
    dtrip_region[, date_parsed := parse_date_any(date)]
  }

  dtripz <- dtrip_region[
    draw == dr & mode %in% modes,
    .(
      mode, date_parsed,
      striper_bag, striper_min, striper_max,
      bluefish_bag, bluefish_min,
      dtrip
    )
  ]

  # Most efficient place to alter regulations later:
  dtripz <- apply_regulation_scenario(dtripz, scenario = scenario)

  if (!nrow(dtripz) || sum(dtripz$dtrip, na.rm = TRUE) == 0) return(NULL)

  # ============================================================
  # BASELINE-ALIGNED INPUT PATH
  # Use the same unallocated catch draws used by calibration. Do not recover
  # str_cat/blu_cat from base_outcomes: those files already contain outcomes
  # from the final calibrated run and are used below only as the welfare baseline.
  # ============================================================

  catch_path <- file.path(
    iterative_input_data_cd,
    paste0("calib_catch_draws_processed_", region_id, "_", dr, ".fst")
  )

  if (!file.exists(catch_path)) {
    warning(
      "Skipping region=", region_id, ", draw=", dr,
      ": missing calibration catch file: ", catch_path
    )
    return(NULL)
  }

  catch_data_all <- data.table::as.data.table(fst::read_fst(catch_path))

  if (!"date_parsed" %in% names(catch_data_all)) {
    catch_data_all[, date_parsed := parse_date_any(date)]
  }

  catch_data_all <- catch_data_all[
    mode %in% modes,
    .(date_parsed, mode, tripid, catch_draw, str_cat, blu_cat)
  ]

  catch_data_all <- merge(
    catch_data_all,
    dtripz,
    by = c("mode", "date_parsed"),
    all.x = TRUE
  )

  angler_all <- data.table::as.data.table(fst::read_fst(angler_path))

  if (!"date_parsed" %in% names(angler_all)) {
    angler_all[, date_parsed := parse_date_any(date)]
  }

  angler_all <- angler_all[mode %in% modes]

  check_required_cols(
    catch_data_all,
    c(
      "date_parsed", "mode", "tripid", "catch_draw",
      "str_cat", "blu_cat",
      "striper_bag", "striper_min", "striper_max",
      "bluefish_bag", "bluefish_min"
    ),
    "catch_data_all"
  )

  size_lookup <- common_inputs$size_lookup
  calib <- common_inputs$calib

  mode_outputs <- vector("list", length(modes))
  names(mode_outputs) <- modes
  mode_length_outputs <- vector("list", length(modes))
  names(mode_length_outputs) <- modes

  catch_by_mode <- split(catch_data_all, by = "mode", keep.by = TRUE)
  angler_by_mode <- split(angler_all, by = "mode", keep.by = TRUE)

  for (md in modes) {

    catch_data <- catch_by_mode[[md]]
    if (is.null(catch_data) || !nrow(catch_data)) next

    angler_dems <- angler_by_mode[[md]]
    if (is.null(angler_dems) || !nrow(angler_dems)) next

    str_size_data <- size_lookup[list(region_id, dr, "str"), .(fitted_prob, length)]
    blu_size_data <- size_lookup[list(region_id, dr, "blu"), .(fitted_prob, length)]

    str_calib <- calib[list(region_id, md, dr, "striper")]
    blu_calib <- calib[list(region_id, md, dr, "bluefish")]

    if (!nrow(str_calib)) {
      str_calib <- data.table::data.table(
        rel_to_keep = 0, keep_to_rel = 0,
        p_rel_to_keep = 0, p_keep_to_rel = 0,
        all_keep_to_rel = 0
      )
    }

    if (!nrow(blu_calib)) {
      blu_calib <- data.table::data.table(
        rel_to_keep = 0, keep_to_rel = 0,
        p_rel_to_keep = 0, p_keep_to_rel = 0,
        all_keep_to_rel = 0
      )
    }

    dtrip_md <- dtripz[mode == md]

    floor_subl_str_harv <- min(dtrip_md$striper_min, na.rm = TRUE) - 3 * 2.54

    min_blu <- min(dtrip_md$bluefish_min, na.rm = TRUE)
    floor_subl_blu_harv <- if (is.na(min_blu) || min_blu == 0) {
      10 * 2.54
    } else {
      min_blu - 2 * 2.54
    }

    # Recreate the exact final calibration random stream for this cell.
    # Set the seed once before simulating striper and bluefish; do not reset
    # between species because catch1 uses a single sequential random stream.
    set.seed(make_cell_seed(region_id, md, dr))

    str_res <- simulate_species_project_str_blu(
      catch_dt = catch_data,
      catch_col = "str_cat",
      bag_col = "striper_bag",
      min_col = "striper_min",
      max_col = "striper_max",
      size_dt = str_size_data,
      species_name = "striper",
      floor_sublegal = floor_subl_str_harv,
      rel_to_keep = str_calib$rel_to_keep[1],
      keep_to_rel = str_calib$keep_to_rel[1],
      p_rel_to_keep = str_calib$p_rel_to_keep[1],
      p_keep_to_rel = str_calib$p_keep_to_rel[1],
      all_keep_to_rel = str_calib$all_keep_to_rel[1],
      region_id = region_id,
      discard_mortality_rate = striper_discard_mortality,
      length_bin_width_in = length_bin_width_in,
      retain_length_output = retain_length_output,
      length_summary = TRUE
    )

    blu_res <- simulate_species_project_str_blu(
      catch_dt = catch_data,
      catch_col = "blu_cat",
      bag_col = "bluefish_bag",
      min_col = "bluefish_min",
      max_col = NULL,
      size_dt = blu_size_data,
      species_name = "bluefish",
      floor_sublegal = floor_subl_blu_harv,
      rel_to_keep = blu_calib$rel_to_keep[1],
      keep_to_rel = blu_calib$keep_to_rel[1],
      p_rel_to_keep = blu_calib$p_rel_to_keep[1],
      p_keep_to_rel = blu_calib$p_keep_to_rel[1],
      all_keep_to_rel = blu_calib$all_keep_to_rel[1],
      region_id = region_id,
      discard_mortality_rate = bluefish_discard_mortality,
      length_bin_width_in = length_bin_width_in,
      retain_length_output = retain_length_output,
      length_summary = FALSE
    )

    key_cols <- c("date_parsed", "mode", "tripid", "catch_draw")

    trip_data <- merge(str_res$trip, blu_res$trip, by = key_cols, all = TRUE)

    length_draw_data <- if (isTRUE(retain_length_output)) {
      data.table::rbindlist(
        list(str_res$length, blu_res$length),
        use.names = TRUE,
        fill = TRUE
      )
    } else {
      data.table::data.table()
    }
    zero_missing_cols(trip_data, setdiff(names(trip_data), key_cols))

    trip_data[, `:=`(
      tot_cat_str_new = tot_keep_str_new + tot_rel_str_new,
      tot_cat_blu_new = tot_keep_blu_new + tot_rel_blu_new
    )]


    trip_data <- merge(
      trip_data,
      angler_dems,
      by = c("date_parsed", "mode", "tripid"),
      all.x = TRUE
    )

    base_path <- file.path(
      final_process_outcomes_cd,
      paste0("base_outcomes_", region_id, "_", md, "_", dr, ".fst")
    )

    if (file.exists(base_path)) {

      base_outcomes <- data.table::as.data.table(fst::read_fst(base_path))

      if (!"date_parsed" %in% names(base_outcomes)) {
        base_outcomes[, date_parsed := parse_date_any(date)]
      }

      base_keep <- intersect(
        c(
          "date_parsed", "mode", "tripid", "catch_draw",
          "tot_blu_catch_base", "tot_keep_blu_base", "tot_keep_blu_util_base",
          "tot_rel_blu_base", "tot_rel_blu_util_base",
          "tot_str_catch_base", "tot_keep_str_base", "tot_keep_str_util_base",
          "tot_rel_str_base", "tot_rel_str_util_base",
          "keep_length_str_in_base", "keep_length_str_in2_base",
          "rel_log_length_str_base"
        ),
        names(base_outcomes)
      )

      base_outcomes <- base_outcomes[, ..base_keep]

      data.table::setkeyv(base_outcomes, key_cols)
      data.table::setkeyv(trip_data, key_cols)

      trip_data <- base_outcomes[trip_data]

    } else {

      warning(
        "Missing baseline outcome file for region=", region_id,
        ", mode=", md,
        ", draw=", dr,
        ". Using projected outcomes as baseline."
      )

      trip_data[, `:=`(
        tot_blu_catch_base = tot_cat_blu_new,
        tot_keep_blu_base = tot_keep_blu_new,
        tot_keep_blu_util_base = tot_keep_blu_util,
        tot_rel_blu_base = tot_rel_blu_new,
        tot_rel_blu_util_base = tot_rel_blu_util,
        tot_str_catch_base = tot_cat_str_new,
        tot_keep_str_base = tot_keep_str_new,
        tot_keep_str_util_base = tot_keep_str_util,
        tot_rel_str_base = tot_rel_str_new,
        tot_rel_str_util_base = tot_rel_str_util,
        keep_length_str_in_base = keep_length_str_in,
        keep_length_str_in2_base = keep_length_str_in2,
        rel_log_length_str_base = rel_log_length_str
      )]
    }

    fill_cols <- grep("^tot_|_base$|length_str|rel_log", names(trip_data), value = TRUE)
    zero_missing_cols(trip_data, fill_cols)

    trip_data[, `:=`(
      v0_trip =
        beta_str_kp * sqrt(tot_keep_str_util_base) +
        beta_str_kp_L * (sqrt(tot_keep_str_util_base) * keep_length_str_in_base) +
        beta_str_kp_L2 * (sqrt(tot_keep_str_util_base) * keep_length_str_in2_base) +
        beta_str_rel * sqrt(tot_rel_str_util_base) +
        beta_str_rel_L * (sqrt(tot_rel_str_util_base) * rel_log_length_str_base) +
        beta_blu_keep * sqrt(tot_keep_blu_util_base) +
        beta_blu_rel * sqrt(tot_rel_blu_util_base) +
        beta_cost * cost_sim,

      vA_trip =
        beta_str_kp * sqrt(tot_keep_str_util) +
        beta_str_kp_L * (sqrt(tot_keep_str_util) * keep_length_str_in) +
        beta_str_kp_L2 * (sqrt(tot_keep_str_util) * keep_length_str_in2) +
        beta_str_rel * sqrt(tot_rel_str_util) +
        beta_str_rel_L * (sqrt(tot_rel_str_util) * rel_log_length_str) +
        beta_blu_keep * sqrt(tot_keep_blu_util) +
        beta_blu_rel * sqrt(tot_rel_blu_util) +
        beta_cost * cost_sim,

      v0_optout = beta_optout + beta_optout_male * male,
      vA_optout = beta_optout + beta_optout_male * male,

      v0_other =
        beta_optoth +
        beta_optoth_days_non_str * days_non_str_blue12 +
        beta_optoth_likely * likely_to_fish +
        beta_cost * cost_sim,

      vA_other =
        beta_optoth +
        beta_optoth_days_non_str * days_non_str_blue12 +
        beta_optoth_likely * likely_to_fish +
        beta_cost * cost_sim
    )]

    mean_trip_data <- data.table::copy(trip_data)

    beta_drop <- setdiff(grep("^beta", names(mean_trip_data), value = TRUE), "beta_cost")
    drop_cols <- intersect(
      c(beta_drop, "male", "age", "total_trips_12", "cost", "opt_out"),
      names(mean_trip_data)
    )

    if (length(drop_cols)) mean_trip_data[, (drop_cols) := NULL]

    keep_vars <- setdiff(names(mean_trip_data), c("date_parsed", "mode", "tripid"))

    mean_trip_data <- mean_trip_data[
      ,
      lapply(.SD, mean, na.rm = TRUE),
      by = .(date_parsed, mode, tripid),
      .SDcols = keep_vars
    ]

    # Average length-specific outcomes across catch draws at the same point
    # where the aggregate trip outcomes are averaged. Probability and expansion
    # weights are attached below after they have been calculated once per trip.
    if (isTRUE(retain_length_output) && nrow(length_draw_data) > 0L) {
      # A length bin absent from a catch draw represents zero fish, not a
      # missing observation. Divide the sum by the full number of catch draws
      # rather than taking the mean only over draws where the bin appeared.
      mean_length_data <- length_draw_data[, .(
        harvest_n = sum(harvest_n, na.rm = TRUE) / n_catch_draws,
        harvest_weight_lb = sum(harvest_weight_lb, na.rm = TRUE) / n_catch_draws,
        released_n = sum(released_n, na.rm = TRUE) / n_catch_draws,
        released_weight_lb = sum(released_weight_lb, na.rm = TRUE) / n_catch_draws,
        dead_discard_n = sum(dead_discard_n, na.rm = TRUE) / n_catch_draws,
        dead_discard_weight_lb = sum(dead_discard_weight_lb, na.rm = TRUE) / n_catch_draws
      ), by = .(
        date_parsed, mode, tripid, species, length_bin_in,
        length_bin_upper_in, length_bin_mid_in
      )]
    } else {
      mean_length_data <- data.table::data.table()
    }

    mean_trip_data[, `:=`(
      probA = calc_prob_trip_3alt(vA_trip, vA_optout, vA_other),
      prob0 = calc_prob_trip_3alt(v0_trip, v0_optout, v0_other),
      log_sum_alt = logsum_3alt(vA_trip, vA_optout, vA_other),
      log_sum_base = logsum_3alt(v0_trip, v0_optout, v0_other)
    )]

    mean_trip_data[, CV := -1 * ((log_sum_alt - log_sum_base) / beta_cost)]
    mean_trip_data[, probA_raw := probA]

    outcome_cols <- intersect(
      c(
        "tot_keep_str_new", "tot_rel_str_new", "tot_cat_str_new",
        "tot_keep_blu_new", "tot_rel_blu_new", "tot_cat_blu_new",
        "tot_keep_str_weight_lb_new", "tot_rel_str_weight_lb_new",
        "tot_keep_blu_weight_lb_new", "tot_rel_blu_weight_lb_new"
      ),
      names(mean_trip_data)
    )

    mean_trip_data[, (outcome_cols) := lapply(.SD, function(x) x * probA), .SDcols = outcome_cols]

    nchoice_path <- file.path(
      final_process_choice_occasions_cd,
      paste0("n_choice_occasions_", region_id, "_", md, "_", dr, ".fst")
    )

    if (!file.exists(nchoice_path)) {
      warning("Missing n_choice_occasions file: ", nchoice_path)
      next
    }

    n_choice_occasions <- data.table::as.data.table(fst::read_fst(nchoice_path))
    n_choice_occasions <- n_choice_occasions[,
      .(date_parsed, mode, n_choice_occasions, estimated_trips)
    ]

    mean_trip_data <- merge(
      mean_trip_data,
      n_choice_occasions,
      by = c("date_parsed", "mode"),
      all.x = TRUE
    )

    mean_trip_data[
      ,
      n_sample_trips := data.table::uniqueN(tripid),
      by = .(date_parsed, mode)
    ]

    mean_trip_data[
      ,
      expand := data.table::fifelse(
        !is.na(n_choice_occasions) & n_sample_trips > 0,
        n_choice_occasions / n_sample_trips,
        NA_real_
      )
    ]

    scale_cols <- intersect(c(outcome_cols, "probA", "prob0", "CV"), names(mean_trip_data))

    mean_trip_data[, (scale_cols) := lapply(.SD, function(x) x * expand), .SDcols = scale_cols]

    # Apply the exact same projected trip probability and expansion factor to
    # every length cell belonging to that sampled trip.
    if (isTRUE(retain_length_output) && nrow(mean_length_data) > 0L) {
      trip_weights <- unique(mean_trip_data[, .(
        date_parsed, mode, tripid, probA_raw, expand
      )])
      trip_weights[!is.finite(probA_raw), probA_raw := 0]
      trip_weights[!is.finite(expand), expand := 0]

      mean_length_data <- merge(
        mean_length_data,
        trip_weights,
        by = c("date_parsed", "mode", "tripid"),
        all.x = TRUE
      )
      mean_length_data[is.na(probA_raw), probA_raw := 0]
      mean_length_data[is.na(expand), expand := 0]

      length_metric_cols <- c(
        "harvest_n", "harvest_weight_lb",
        "released_n", "released_weight_lb",
        "dead_discard_n", "dead_discard_weight_lb"
      )
      mean_length_data[, (length_metric_cols) := lapply(
        .SD,
        function(x) x * probA_raw * expand
      ), .SDcols = length_metric_cols]

      mean_length_data[, `:=`(
        region = region_id,
        month = lubridate::month(date_parsed),
        iteration = dr,
        scenario = scenario$name %||% "scenario"
      )]

      mode_length_outputs[[md]] <- mean_length_data[,
        lapply(.SD, safe_sum_numeric),
        by = .(
          scenario, iteration, region, mode, month, species,
          length_bin_in, length_bin_upper_in, length_bin_mid_in
        ),
        .SDcols = length_metric_cols
      ]
    }

    data.table::setnames(
      mean_trip_data,
      c("probA", "prob0"),
      c("n_trips_alt", "n_trips_base"),
      skip_absent = TRUE
    )

    metric_cols <- intersect(
      c(
        "CV", "n_trips_alt", "n_trips_base",
        "tot_keep_str_new", "tot_rel_str_new", "tot_cat_str_new",
        "tot_keep_blu_new", "tot_rel_blu_new", "tot_cat_blu_new",
        "tot_keep_str_weight_lb_new", "tot_rel_str_weight_lb_new",
        "tot_keep_blu_weight_lb_new", "tot_rel_blu_weight_lb_new"
      ),
      names(mean_trip_data)
    )

    mean_trip_data[, region := region_id]
    mean_trip_data[, proj_month := lubridate::month(date_parsed)]

    mode_outputs[[md]] <- mean_trip_data[
      ,
      lapply(.SD, safe_sum_numeric),
      .SDcols = metric_cols,
      by = .(region, mode, month = proj_month)
    ]
  }

  model_output <- data.table::rbindlist(mode_outputs, use.names = TRUE, fill = TRUE)

  if (!nrow(model_output)) return(NULL)

  model_output_all <- model_output[
    ,
    lapply(.SD, safe_sum_numeric),
    by = .(region, month),
    .SDcols = setdiff(names(model_output), c("region", "mode", "month"))
  ]

  model_output_all[, mode := "all modes"]
  data.table::setcolorder(model_output_all, names(model_output))

  model_output <- data.table::rbindlist(
    list(model_output, model_output_all),
    use.names = TRUE,
    fill = TRUE
  )

  metric_cols <- setdiff(names(model_output), c("region", "mode", "month"))

  model_output_long <- data.table::melt(
    model_output,
    id.vars = c("region", "mode", "month"),
    measure.vars = metric_cols,
    variable.name = "metric",
    value.name = "projected_value"
  )

  model_output_long[, `:=`(
    iteration = dr,
    scenario = scenario$name %||% "scenario"
  )]

  length_output <- data.table::rbindlist(
    mode_length_outputs,
    use.names = TRUE,
    fill = TRUE
  )

  if (nrow(length_output) > 0L) {
    length_output_all <- length_output[,
      lapply(.SD, safe_sum_numeric),
      by = .(
        scenario, iteration, region, month, species,
        length_bin_in, length_bin_upper_in, length_bin_mid_in
      ),
      .SDcols = c(
        "harvest_n", "harvest_weight_lb",
        "released_n", "released_weight_lb",
        "dead_discard_n", "dead_discard_weight_lb"
      )
    ]
    length_output_all[, mode := "all modes"]
    data.table::setcolorder(length_output_all, names(length_output))
    length_output <- data.table::rbindlist(
      list(length_output, length_output_all),
      use.names = TRUE,
      fill = TRUE
    )
  }

  list(summary = model_output_long[], length = length_output[])
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# -----------------------------------------------------------------------------
# Parallel wrapper over region x draw jobs
# -----------------------------------------------------------------------------
draws <- 1:50
region_draw<- c("CTALL", "DEDERIV", "DENANT", "DEOCN", "MAALL", "MDCHES", "MDOCN", "MDPOT", "MEALL",
                         "NCALB", "NCCNTRL", "NCOCN", "NHALL", "NJALL", "NYHUDN", "NYHUDS", "RIALL", "VACHES", "VAOCN", "VAPOTO")


run_str_blu_projection <- function(region_draw = get("region_draw", envir = .GlobalEnv),
                                   mode_draw = get("mode_draw", envir = .GlobalEnv),
                                   draws = get("draws", envir = .GlobalEnv),
                                   scenario = get("projection_scenario", envir = .GlobalEnv),
                                   n_workers = get("n_workers", envir = .GlobalEnv),
                                   use_parallel = get("use_parallel", envir = .GlobalEnv),
                                   common_inputs = NULL) {

  if (is.null(common_inputs)) {
    common_inputs <- read_projection_common_inputs_str_blu(
      final_process_misc_cd = final_process_misc_cd,
      input_data_cd = input_data_cd,
      mode_draw = mode_draw,
      draws = draws,
      region_draw = region_draw
    )
  }
  if (is.null(region_draw)) region_draw <- common_inputs$region_draw

  jobs <- data.table::CJ(region = region_draw, draw = draws, sorted = FALSE)

  run_one <- function(ii) {
    rg <- jobs$region[ii]
    dr <- jobs$draw[ii]
    message("Projection: region=", rg, ", draw=", dr, ", modes=", paste(mode_draw, collapse = ","))
    project_one_str_blu_region_draw(
      region_id = rg,
      dr = dr,
      common_inputs = common_inputs,
      modes = mode_draw,
      scenario = scenario,
      n_catch_draws = n_catch_draws
    )
  }

  if (isTRUE(use_parallel) && nrow(jobs) > 1L && requireNamespace("future.apply", quietly = TRUE)) {
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multisession, workers = n_workers)
    out <- future.apply::future_lapply(seq_len(nrow(jobs)), run_one, future.seed = TRUE)
  } else {
    out <- lapply(seq_len(nrow(jobs)), run_one)
  }

  list(
    summary = data.table::rbindlist(
      lapply(out, function(x) if (is.null(x)) NULL else x$summary),
      use.names = TRUE,
      fill = TRUE
    ),
    length = data.table::rbindlist(
      lapply(out, function(x) if (is.null(x)) NULL else x$length),
      use.names = TRUE,
      fill = TRUE
    )
  )
}

# -----------------------------------------------------------------------------
# Baseline reproduction diagnostic
# -----------------------------------------------------------------------------

compare_projection_to_calibration <- function(prediction_long, calib) {
  proj <- make_str_blu_projection_compare(prediction_long)[
    mode != "all modes" &
      metric %in% c("harvest (#s)", "discards (#s)", "catch (#s)")
  ]

  proj <- proj[, .(
    projected_value = sum(projected_value, na.rm = TRUE)
  ), by = .(iteration, region, mode, species, metric)]

  calib_keep <- data.table::copy(calib)[, .(
    iteration = draw,
    region,
    mode,
    species,
    `harvest (#s)` = model_keep,
    `discards (#s)` = model_rel,
    `catch (#s)` = model_catch
  )]

  calib_long <- data.table::melt(
    calib_keep,
    id.vars = c("iteration", "region", "mode", "species"),
    variable.name = "metric",
    value.name = "calibrated_value"
  )

  ans <- merge(
    proj,
    calib_long,
    by = c("iteration", "region", "mode", "species", "metric"),
    all = TRUE
  )

  ans[, `:=`(
    difference = projected_value - calibrated_value,
    pct_difference = 100 * safe_divide(
      projected_value - calibrated_value,
      calibrated_value
    )
  )]

  data.table::setorder(ans, iteration, region, mode, species, metric)
  ans[]
}

# -----------------------------------------------------------------------------
# Final comparison helper
# -----------------------------------------------------------------------------

make_str_blu_projection_compare <- function(prediction_long, calib = NULL) {
  out <- data.table::copy(prediction_long)
  out[, species := data.table::fcase(
    grepl("_str_", metric), "striper",
    grepl("_blu_", metric), "bluefish",
    default = NA_character_
  )]
  out[, metric_clean := data.table::fcase(
    metric == "CV", "compensating variation ($)",
    metric == "n_trips_alt", "projected trips",
    metric == "n_trips_base", "baseline trips",
    grepl("tot_keep_.*weight_lb", metric), "harvest (lbs.)",
    grepl("tot_rel_.*weight_lb", metric), "discards (lbs.)",
    grepl("tot_keep_", metric), "harvest (#s)",
    grepl("tot_rel_", metric), "discards (#s)",
    grepl("tot_cat_", metric), "catch (#s)",
    default = as.character(metric)
  )]
  out[, metric := metric_clean]
  out[, metric_clean := NULL]
  out[]
}

# -----------------------------------------------------------------------------
# Main execution
# -----------------------------------------------------------------------------

system.time({
  common_inputs <- read_projection_common_inputs_str_blu(
    final_process_misc_cd = final_process_misc_cd,
    input_data_cd = input_data_cd,
    mode_draw = mode_draw,
    draws = draws,
    region_draw = region_draw
  )

  projection_results <- run_str_blu_projection(
    region_draw = common_inputs$region_draw,
    mode_draw = mode_draw,
    draws = draws,
    scenario = projection_scenario,
    n_workers = n_workers,
    use_parallel = use_parallel,
    common_inputs = common_inputs
  )

  prediction_long <- projection_results$summary
  prediction_length <- projection_results$length
})

# Save retained projected biological outcomes by one-inch length bin.
# The main length file has one row per scenario x draw x region x mode x month
# x species x length bin. Summing over length reproduces the corresponding
# projected harvest and dead-discard totals (subject to floating-point precision).
if (isTRUE(retain_length_output) && nrow(prediction_length) > 0L) {
  scenario_file_tag <- gsub(
    "[^A-Za-z0-9_-]",
    "_",
    projection_scenario$name %||% "scenario"
  )

  fst::write_fst(
    prediction_length,
    file.path(
      projection_output_cd,
      paste0("str_blu_projection_at_length_", scenario_file_tag, ".fst")
    )
  )

  data.table::fwrite(
    prediction_length,
    file.path(
      projection_output_cd,
      paste0("str_blu_projection_at_length_", scenario_file_tag, ".csv")
    )
  )
}

final_compare <- make_str_blu_projection_compare(prediction_long)

projection_output_cd
list.files(
  projection_output_cd,
  pattern = "str_blu_projection",
  full.names = TRUE
)

scenario_file_tag <- gsub(
  "[^A-Za-z0-9_-]",
  "_",
  projection_scenario$name %||% "scenario"
)

data.table::fwrite(
  final_compare,
  file.path(
    projection_output_cd,
    paste0("str_blu_projection_", scenario_file_tag, ".csv")
  )
)

fst::write_fst(
  final_compare,
  file.path(
    projection_output_cd,
    paste0("str_blu_projection_", scenario_file_tag, ".fst")
  )
)

fst::write_fst(
  prediction_long,
  file.path(
    projection_output_cd,
    paste0("str_blu_projection_raw_", scenario_file_tag, ".fst")
  )
)

file.path(
  projection_output_cd,
  "baseline_projection_vs_calibration_check.csv"
)

baseline_reproduction_check <- compare_projection_to_calibration(
  prediction_long = prediction_long,
  calib = common_inputs$calib
)

data.table::fwrite(
  baseline_reproduction_check,
  file.path(
    projection_output_cd,
    "baseline_projection_vs_calibration_check.csv"
  )
)

data.table::data.table(
  file = list.files(
    projection_output_cd,
    full.names = TRUE
  )
)





