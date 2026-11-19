# Calibration-year trip simulation for striped bass and bluefish
# Simulation 0: no trip-level keep-to-release or release-to-keep reallocation.
#
# Main changes relative to older striper/bluefish calibration0 file:
#   1. Simulates striped bass totals only; no striper keep length-bin calibration.
#   2. Retains trip-level mean striped bass lengths for utility:
#        - mean_len_keep_str_in
#        - mean_len_rel_str_in
#        - mean_log_len_rel_str
#      Harvestable striped bass utility uses a quadratic function of mean length.
#      Released striped bass utility uses a logarithmic function of mean length.
#   3. Keeps the region x mode x draw structure used for the striper/bluefish project.
#   4. Retains the original 3-alternative choice probability: trip, opt-out, other fishing.

library(data.table)
library(arrow)
library(readr)
library(fst)
library(haven)

# ----------------------------
# Helpers
# ----------------------------

cm_to_inch <- function(x) x / 2.54

safe_divide <- function(num, den) {
  fifelse(is.na(den) | den == 0, NA_real_, num / den)
}

calc_prob_trip <- function(v_trip, v_optout, v_other) {
  # Numerically stable 3-alternative softmax probability for trip alternative.
  m <- pmax(v_trip, v_optout, v_other)
  exp_trip   <- exp(v_trip   - m)
  exp_optout <- exp(v_optout - m)
  exp_other  <- exp(v_other  - m)
  exp_trip / (exp_trip + exp_optout + exp_other)
}

as_calib_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  as.Date(x, format = "%d%b%Y")
}

check_required_cols <- function(dt, cols, object_name) {
  missing_cols <- setdiff(cols, names(dt))
  if (length(missing_cols)) {
    stop(object_name, " is missing required columns: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

resolve_col <- function(dt, candidates, object_name, required = TRUE) {
  hit <- candidates[candidates %in% names(dt)]
  if (length(hit) > 0L) return(hit[1L])
  if (required) {
    stop(object_name, " is missing one of these columns: ",
         paste(candidates, collapse = ", "), call. = FALSE)
  }
  NA_character_
}

# Weighted mean for length diagnostics after probability/expansion weights are applied.
wtd_mean_safe <- function(x, w) {
  den <- sum(w[!is.na(x)], na.rm = TRUE)
  if (is.na(den) || den == 0) return(NA_real_)
  sum(x * w, na.rm = TRUE) / den
}


safe_sum_numeric <- function(x) {
  # sum(..., na.rm = TRUE) returns 0 for all-NA groups.
  # For expansion/count variables, all-NA usually indicates a failed merge
  # or missing directed-trip estimate, so preserve NA instead of silently
  # converting it to zero.
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0L || all(is.na(x))) return(NA_real_)
  sum(x, na.rm = TRUE)
}


simulate_species <- function(catch_dt,
                             catch_col,
                             bag_col,
                             min_col,
                             max_col = NULL,
                             size_dt,
                             species_prefix = c("str", "blu")) {

  species_prefix <- match.arg(species_prefix)

  key_cols <- c("date_parsed", "mode", "tripid", "catch_draw")

  keep_col <- paste0("tot_keep_", species_prefix, "_new")
  rel_col  <- paste0("tot_rel_",  species_prefix, "_new")

  check_required_cols(catch_dt,
                      c(key_cols, catch_col, bag_col, min_col),
                      "catch_dt")
  check_required_cols(size_dt, c("length", "fitted_prob"), "size_dt")

  if (nrow(size_dt) == 0L || all(is.na(size_dt$fitted_prob))) {
    stop("No usable catch-at-length probabilities for species = ", species_prefix,
         call. = FALSE)
  }

  out_zero <- unique(catch_dt[, ..key_cols])
  out_zero[, c(keep_col, rel_col) := .(0L, 0L)]

  if (species_prefix == "str") {
    out_zero[, `:=`(
      mean_len_keep_str_in = NA_real_,
      mean_len_rel_str_in = NA_real_,
      mean_log_len_rel_str = NA_real_
    )]
  }

  pos_dt <- catch_dt[get(catch_col) > 0,
                     .(date_parsed, mode, tripid, catch_draw,
                       catch_n = as.integer(round(get(catch_col))),
                       bag     = get(bag_col),
                       min_sz  = get(min_col),
                       max_sz  = if (!is.null(max_col)) get(max_col) else Inf)]

  if (nrow(pos_dt) == 0L) {
    setkeyv(out_zero, key_cols)
    return(out_zero[])
  }

  fish_dt <- pos_dt[rep(seq_len(.N), catch_n)]
  fish_dt[, fishid := seq_len(.N)]

  fish_dt[, fitted_length := sample(size_dt$length,
                                    .N,
                                    replace = TRUE,
                                    prob = size_dt$fitted_prob)]

  setorder(fish_dt, date_parsed, mode, tripid, catch_draw, fishid)

  if (is.null(max_col)) {
    fish_dt[, posskeep := fifelse(fitted_length >= min_sz, 1L, 0L)]
  } else {
    fish_dt[, posskeep := fifelse(fitted_length >= min_sz & fitted_length <= max_sz, 1L, 0L)]
  }

  fish_dt[, csum_keep := cumsum(posskeep), by = key_cols]
  fish_dt[, keep := fifelse(bag > 0 & posskeep == 1L & csum_keep <= bag, 1L, 0L)]
  fish_dt[, release := fifelse(keep == 1L, 0L, 1L)]

  if (species_prefix == "str") {
    fish_dt[, length_in := cm_to_inch(fitted_length)]

    trip_pos <- fish_dt[, .(
      keep_n = sum(keep),
      rel_n  = sum(release),
      mean_len_keep_str_in = fifelse(sum(keep) > 0,
                                     mean(length_in[keep == 1L], na.rm = TRUE),
                                     NA_real_),
      mean_len_rel_str_in = fifelse(sum(release) > 0,
                                    mean(length_in[release == 1L], na.rm = TRUE),
                                    NA_real_),
      mean_log_len_rel_str = fifelse(sum(release) > 0,
                                     mean(log(length_in[release == 1L]), na.rm = TRUE),
                                     NA_real_)
    ), by = key_cols]
  } else {
    trip_pos <- fish_dt[, .(
      keep_n = sum(keep),
      rel_n  = sum(release)
    ), by = key_cols]
  }

  setnames(trip_pos, c("keep_n", "rel_n"), c(keep_col, rel_col))

  trip_out <- rbindlist(list(trip_pos, out_zero[!trip_pos, on = key_cols]),
                        use.names = TRUE, fill = TRUE)

  fill_cols <- c(keep_col, rel_col)
  for (cc in fill_cols) set(trip_out, which(is.na(trip_out[[cc]])), cc, 0L)

  setkeyv(trip_out, key_cols)
  trip_out[]
}

build_compare_table <- function(summed_results, MRIP_comparison_draw, md) {

  metric_cols <- c(
    "striper_keep", "striper_rel", "striper_catch",
    "bluefish_keep", "bluefish_rel", "bluefish_catch"
  )

  model_metrics  <- intersect(metric_cols, names(summed_results))
  mrip_metrics   <- intersect(metric_cols, names(MRIP_comparison_draw))
  common_metrics <- intersect(model_metrics, mrip_metrics)

  if (length(common_metrics) == 0L) {
    stop("No common metric columns found between summed_results and MRIP_comparison_draw.",
         call. = FALSE)
  }

  model_long <- melt(
    as.data.table(summed_results)[, c("mode", common_metrics), with = FALSE],
    id.vars = "mode",
    measure.vars = common_metrics,
    variable.name = "metric",
    value.name = "model"
  )

  mrip_long <- melt(
    as.data.table(MRIP_comparison_draw)[, c("mode", common_metrics), with = FALSE],
    id.vars = "mode",
    measure.vars = common_metrics,
    variable.name = "metric",
    value.name = "MRIP"
  )

  cmp <- data.table::merge.data.table(
    model_long, mrip_long,
    by = c("mode", "metric"),
    all = FALSE
  )

  data.table::setDT(cmp)

  cmp[, c("species", "disposition") :=
        data.table::tstrsplit(metric, "_", fixed = TRUE, keep = 1:2)]

  cmp[, `:=`(
    model = as.numeric(model),
    MRIP  = as.numeric(MRIP)
  )]
  cmp[, diff := model - MRIP]
  cmp[, pct_diff := fifelse(MRIP != 0, 100 * diff / MRIP, NA_real_)]
  cmp[, abs_diff_val := abs(diff)]
  cmp[, abs_pct_diff_val := fifelse(MRIP != 0, abs(100 * diff / MRIP), NA_real_)]
  cmp[, mode := md]

  compare_k <- cmp[disposition == "keep",
                   .(mode, species,
                     MRIP_keep = MRIP,
                     model_keep = model,
                     diff_keep = diff,
                     pct_diff_keep = pct_diff)]

  compare_c <- cmp[disposition == "catch",
                   .(mode, species,
                     MRIP_catch = MRIP,
                     model_catch = model,
                     diff_catch = diff,
                     pct_diff_catch = pct_diff)]

  compare_r <- cmp[disposition == "rel",
                   .(mode, species,
                     MRIP_rel = MRIP,
                     model_rel = model,
                     diff_rel = diff,
                     pct_diff_rel = pct_diff)]

  out <- data.table::merge.data.table(compare_r, compare_k,
                                      by = c("mode", "species"),
                                      all = TRUE)
  out <- data.table::merge.data.table(out, compare_c,
                                      by = c("mode", "species"),
                                      all = TRUE)

  data.table::setDT(out)

  # These first-pass values are retained for downstream calibration1, but no
  # reallocation is performed in this simulation0 file.
  out[, rel_to_keep := fifelse(diff_keep < 0, 1L, 0L)]
  out[, keep_to_rel := fifelse(diff_keep > 0, 1L, 0L)]
  out[, p_rel_to_keep := abs(safe_divide(diff_keep, model_rel))]
  out[, p_keep_to_rel := abs(safe_divide(diff_keep, model_keep))]

  out[]
}

# ----------------------------
# Main inputs
# ----------------------------

MRIP_comparison <- as.data.table(
  haven::read_dta(file.path(iterative_input_data_cd, "simulated_catch_totals_final.dta"))
)

setnames(
  MRIP_comparison,
  old = c("tot_dtrip_sim",
          "tot_str_cat_sim", "tot_blu_cat_sim",
          "tot_str_keep_sim", "tot_blu_keep_sim",
          "tot_str_rel_sim", "tot_blu_rel_sim"),
  new = c("estimated_trips",
          "striper_catch", "bluefish_catch",
          "striper_keep", "bluefish_keep",
          "striper_rel", "bluefish_rel"),
  skip_absent = TRUE
)

mode_draw   <- c("pr", "fh", "sh")
draws       <- seq_len(n_simulations)

# If you do not already define region_draw upstream, use the region values in MRIP.
if (!exists("region_draw")) {
  region_draw <- sort(unique(MRIP_comparison$region))
}

# Preload baseline catch-at-length once.
size_lookup <- as.data.table(
  readr::read_csv(file.path(input_data_cd, "baseline_catch_at_length_multi_region.csv"),
                  show_col_types = FALSE)
)
check_required_cols(size_lookup,
                    c("region", "draw", "species", "fitted_prob", "length"),
                    "baseline_catch_at_length_multi_region.csv")
size_lookup <- size_lookup[!is.na(fitted_prob), .(region, draw, species, fitted_prob, length)]
setkey(size_lookup, region, draw, species)

calib_comparison <- vector("list", length(region_draw) * length(mode_draw) * length(draws))
k <- 1L

# ----------------------------
# Main loop
# ----------------------------

#region_draw<-c("NCALB", "NCCNTRL", "NCOCN")
#draws<-1:101
#mode_draw   <- c("pr", "fh", "sh")


for (s in region_draw) {

  dtrip_region <- as.data.table(
    fst::read_fst(file.path(iterative_input_data_cd, paste0("directed_trip_draws_", s, ".fst")))
  )


  for (i in draws) {

    catch_draw_dt <- as.data.table(
      fst::read_fst(file.path(iterative_input_data_cd, paste0("calib_catch_draws_processed_", s,"_",  i, ".fst")))
    )

    angler_draw_dt <- as.data.table(
      fst::read_fst(file.path(iterative_input_data_cd, paste0("pref_params_and_dems_", s, "_", i, ".fst")))
    )

    angler_draw_dt <- angler_draw_dt %>%
      dplyr::mutate(date_parsed = parse_date_any(date),
                    month=data.table::month(date_parsed)) %>%
      dplyr::select(-date)


    # Keep compatibility with either old separate keep/release simulated inputs
    # or newer total-catch columns.
    if (!"str_cat" %in% names(catch_draw_dt)) {
      check_required_cols(catch_draw_dt, c("str_cat"), "catch_draw_dt")
    }
    if (!"blu_cat" %in% names(catch_draw_dt)) {
      check_required_cols(catch_draw_dt, c("blu_cat"), "catch_draw_dt")
    }

    striper_size_data <- size_lookup[list(s, i, "str"), .(fitted_prob, length)]
    bluefish_size_data <- size_lookup[list(s, i, "blu"), .(fitted_prob, length)]

    for (md in mode_draw) {

      dtripz <- dtrip_region[draw == i & mode == md,
                             .(mode, date_parsed, striper_bag, striper_min, striper_max,
                               bluefish_bag, bluefish_min, dtrip)]

      MRIP_comparison_draw <- MRIP_comparison[draw == i & region == s & mode == md,
                                              .(mode,
                                                striper_keep, striper_rel, striper_catch,
                                                bluefish_keep, bluefish_rel, bluefish_catch)]

      if (nrow(MRIP_comparison_draw) == 0L) {
        MRIP_comparison_draw <- data.table(
          mode = md,
          striper_keep = NA_real_, striper_rel = NA_real_, striper_catch = NA_real_,
          bluefish_keep = NA_real_, bluefish_rel = NA_real_, bluefish_catch = NA_real_
        )
      }

      catch_data <- merge(
        catch_draw_dt[mode == md],
        dtripz,
        by = c("mode", "date_parsed"),
        all.x = TRUE
      )

      if (nrow(catch_data) == 0L || nrow(dtripz) == 0L || sum(dtripz$dtrip, na.rm = TRUE) == 0) {
        summed_results <- data.table(
          mode = md,
          striper_catch = 0, striper_keep = 0, striper_rel = 0,
          bluefish_catch = 0, bluefish_keep = 0, bluefish_rel = 0,
          estimated_trips = 0, n_choice_occasions = 0,
          mean_len_keep_str_in = NA_real_, mean_len_rel_str_in = NA_real_
        )

        compare_out <- build_compare_table(summed_results, MRIP_comparison_draw, md)
        compare_out[, `:=`(draw = i, region = s)]
        calib_comparison[[k]] <- compare_out
        k <- k + 1L
        next
      }

      angler_dems <- angler_draw_dt[mode == md]

      str_trip_data <- simulate_species(
        catch_dt = catch_data,
        catch_col = "str_cat",
        bag_col   = "striper_bag",
        min_col   = "striper_min",
        max_col   = "striper_max",
        size_dt   = striper_size_data,
        species_prefix = "str"
      )

      blu_trip_data <- simulate_species(
        catch_dt = catch_data,
        catch_col = "blu_cat",
        bag_col   = "bluefish_bag",
        min_col   = "bluefish_min",
        max_col   = NULL,
        size_dt   = bluefish_size_data,
        species_prefix = "blu"
      )

      key_cols <- c("date_parsed", "mode", "tripid", "catch_draw")
      trip_data <- merge(str_trip_data, blu_trip_data, by = key_cols, all = TRUE)

      zero_fill_cols <- grep("^tot_(keep|rel)_(str|blu)_new$", names(trip_data), value = TRUE)
      for (cc in zero_fill_cols) set(trip_data, which(is.na(trip_data[[cc]])), cc, 0L)

      trip_data[, `:=`(
        tot_str_catch = tot_keep_str_new + tot_rel_str_new,
        tot_blu_catch = tot_keep_blu_new + tot_rel_blu_new
      )]

      setkey(angler_dems, date_parsed, mode, tripid)
      trip_data <- merge(trip_data, angler_dems, by = c("date_parsed", "mode", "tripid"), all.x = TRUE)


      # Missing mean lengths are set to 0 only inside interactions where catch count is 0.
      trip_data[, `:=`(
        keep_length_str_in = fifelse(tot_keep_str_new > 0 & !is.na(mean_len_keep_str_in),
                                     mean_len_keep_str_in, 0),
        keep_length_str_in2 = fifelse(tot_keep_str_new > 0 & !is.na(mean_len_keep_str_in),
                                      mean_len_keep_str_in^2, 0),
        rel_log_length_str = fifelse(tot_rel_str_new > 0 & !is.na(mean_log_len_rel_str),
                                     mean_log_len_rel_str, 0)
      )]


      trip_data[, `:=`(
        vA_trip =
          beta_str_kp * sqrt(tot_keep_str_new) +
          beta_str_kp_L * (sqrt(tot_keep_str_new) * keep_length_str_in) +
          beta_str_kp_L2 * (sqrt(tot_keep_str_new) * keep_length_str_in2) +
          beta_str_rel * sqrt(tot_rel_str_new) +
          beta_str_rel_L * (sqrt(tot_rel_str_new) * rel_log_length_str) +
          beta_blu_keep * sqrt(tot_keep_blu_new) +
          beta_blu_rel * sqrt(tot_rel_blu_new) +
          beta_cost * cost_sim,

        vA_optout =
          beta_optout + beta_optout_male*male ,

        vA_other =
          beta_optoth +
          beta_optoth_days_non_str*days_non_str_blue12 +
          beta_optoth_likely*likely_to_fish+
          beta_cost * cost_sim
      )]


      mean_trip_data <- copy(trip_data)
      drop_cols <- intersect(c(grep("^beta", names(mean_trip_data), value = TRUE),
                               "male", "age", "total_trips_12", "cost", "opt_out"),
                             names(mean_trip_data))
      if (length(drop_cols)) mean_trip_data[, (drop_cols) := NULL]

      mean_cols <- setdiff(names(mean_trip_data), c("date_parsed", "mode", "tripid"))
      mean_trip_data <- mean_trip_data[, lapply(.SD, mean, na.rm = TRUE),
                                       by = .(date_parsed, mode, tripid),
                                       .SDcols = mean_cols]

      mean_trip_data <- haven::zap_labels(mean_trip_data)
      mean_trip_data[, prob0 := calc_prob_trip(vA_trip, vA_optout, vA_other)]
      mean_trip_data[, c("vA_trip", "vA_optout", "vA_other", "catch_draw") := NULL]

      accounting_cols <- c("tot_keep_str_new", "tot_rel_str_new", "tot_str_catch",
                           "tot_keep_blu_new", "tot_rel_blu_new", "tot_blu_catch")
      mean_trip_data[, (accounting_cols) := lapply(.SD, function(x) x * prob0),
                     .SDcols = accounting_cols]

      mean_trip_data <- merge(mean_trip_data,
                              dtripz[, .(mode, date_parsed, dtrip)],
                              by = c("mode", "date_parsed"),
                              all.x = TRUE)

      # Diagnose failed directed-trip matches before expansion. If dtrip is NA,
      # multiplying by expand creates NA values that can otherwise be hidden by
      # sum(..., na.rm = TRUE) in the aggregate output.
      missing_dtrip_keys <- unique(mean_trip_data[is.na(dtrip), .(mode, date_parsed)])
      if (nrow(missing_dtrip_keys) > 0L) {
        warning("Missing directed-trip totals for ", nrow(missing_dtrip_keys),
                " mode/date cells. n_choice_occasions and estimated_trips ",
                "will remain NA for these cells instead of being converted to zero.")
      }

      mean_trip_data[, mean_prob := mean(prob0, na.rm = TRUE), by = .(mode, date_parsed)]
      mean_trip_data[!is.finite(mean_prob) | mean_prob <= 0, mean_prob := NA_real_]
      mean_trip_data[, sims := data.table::fifelse(
        !is.na(dtrip) & !is.na(mean_prob),
        round(dtrip / mean_prob),
        NA_real_
      )]
      mean_trip_data[, expand := sims / 50]
      mean_trip_data[, n_choice_occasions := 1]

      expand_cols <- c(accounting_cols, "n_choice_occasions", "prob0")
      mean_trip_data[, (expand_cols) := lapply(.SD, function(x) x * expand),
                     .SDcols = expand_cols]

      for (j in names(mean_trip_data)) setattr(mean_trip_data[[j]], "label", NULL)

      aggregate_trip_data <- mean_trip_data[, .(
        tot_keep_str_new = sum(tot_keep_str_new, na.rm = TRUE),
        tot_rel_str_new  = sum(tot_rel_str_new, na.rm = TRUE),
        tot_str_catch    = sum(tot_str_catch, na.rm = TRUE),
        tot_keep_blu_new = sum(tot_keep_blu_new, na.rm = TRUE),
        tot_rel_blu_new  = sum(tot_rel_blu_new, na.rm = TRUE),
        tot_blu_catch    = sum(tot_blu_catch, na.rm = TRUE),
        estimated_trips  = safe_sum_numeric(prob0),
        n_choice_occasions = safe_sum_numeric(n_choice_occasions),
        mean_len_keep_str_in = wtd_mean_safe(mean_len_keep_str_in, tot_keep_str_new),
        mean_len_rel_str_in  = wtd_mean_safe(mean_len_rel_str_in,  tot_rel_str_new)
      ), by = .(date_parsed, mode)]

      setnames(
        aggregate_trip_data,
        old = c("tot_str_catch", "tot_blu_catch",
                "tot_keep_str_new", "tot_keep_blu_new",
                "tot_rel_str_new", "tot_rel_blu_new"),
        new = c("striper_catch", "bluefish_catch",
                "striper_keep", "bluefish_keep",
                "striper_rel", "bluefish_rel"),
        skip_absent = TRUE
      )

      summed_results <- aggregate_trip_data[, .(
        bluefish_catch = sum(bluefish_catch, na.rm = TRUE),
        bluefish_keep  = sum(bluefish_keep, na.rm = TRUE),
        bluefish_rel   = sum(bluefish_rel, na.rm = TRUE),
        striper_catch  = sum(striper_catch, na.rm = TRUE),
        striper_keep   = sum(striper_keep, na.rm = TRUE),
        striper_rel    = sum(striper_rel, na.rm = TRUE),
        estimated_trips = safe_sum_numeric(estimated_trips),
        n_choice_occasions = safe_sum_numeric(n_choice_occasions),
        mean_len_keep_str_in = wtd_mean_safe(mean_len_keep_str_in, striper_keep),
        mean_len_rel_str_in  = wtd_mean_safe(mean_len_rel_str_in,  striper_rel)
      ), by = mode]

      compare_out <- build_compare_table(summed_results, MRIP_comparison_draw, md)
      compare_out[, `:=`(
        draw = i,
        region = s,
        mean_len_keep_str_in = summed_results$mean_len_keep_str_in[1],
        mean_len_rel_str_in = summed_results$mean_len_rel_str_in[1]
      )]

      calib_comparison[[k]] <- compare_out
      k <- k + 1L
    }
  }
}

calib_comparison_combined <- rbindlist(calib_comparison, use.names = TRUE, fill = TRUE)
calib_comparison_combined <- calib_comparison_combined[!is.na(mode)]

setcolorder(calib_comparison_combined,
            c("region", "mode", "species", "draw",
              setdiff(names(calib_comparison_combined),
                      c("region", "mode", "species", "draw"))))

fst::write_fst(calib_comparison_combined,
               file.path(final_process_misc_cd, "calibration_comparison.fst"))
readr::write_csv(calib_comparison_combined,
                 file.path(final_process_misc_cd, "calibration_comparison.csv"))
