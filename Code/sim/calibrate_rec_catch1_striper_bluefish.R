# Calibration-year trip simulation WITH optional trip-level keep/release reallocation
# for striped bass / bluefish.
#
# This file is designed to be called by calibration_routine_striper_bluefish_simplified.R.
# It matches the simplified simulation0 specification:
#   - no striped bass length-bin calibration;
#   - harvest calibration is based on total keep numbers only;
#   - trip-level striped bass mean keep length, keep length squared, and release log-length
#     are retained for the updated utility function.
#
# Accounting convention:
#   - tot_*_new columns are used for MRIP catch accounting after reallocation.
#   - tot_*_util columns are used in utility. Fish moved from keep -> release remain
#     keep_util, so the calibration correction does not remove the value of legally
#     keepable fish from the expected utility calculation.

library(data.table)
library(readr)
library(fst)
library(haven)

cm_to_inch <- function(x) x / 2.54

parse_date_any <- function(x) {
  data.table::as.IDate(as.Date(
    x,
    tryFormats = c("%d%b%Y", "%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y")
  ))
}

safe_divide <- function(num, den) {
  data.table::fifelse(is.na(den) | den == 0, NA_real_, num / den)
}

calc_prob_trip <- function(v_trip, v_optout, v_other) {
  m <- pmax(v_trip, v_optout, v_other)
  exp_trip   <- exp(v_trip   - m)
  exp_optout <- exp(v_optout - m)
  exp_other  <- exp(v_other  - m)
  exp_trip / (exp_trip + exp_optout + exp_other)
}

check_required_cols <- function(dt, cols, object_name) {
  missing_cols <- setdiff(cols, names(dt))
  if (length(missing_cols)) {
    stop(object_name, " is missing required columns: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

wtd_mean_safe <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  den <- sum(w[ok], na.rm = TRUE)
  if (!is.finite(den) || den == 0) return(NA_real_)
  sum(x[ok] * w[ok], na.rm = TRUE) / den
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

# Length-weight conversions
kg_to_lb <- 2.20462262185

striper_weight_lb <- function(length_in) {
  exp(-8.185 + 3.027 * log(length_in))
}

bluefish_weight_lb <- function(length_cm) {
  # W kg = a * L^b, where L is fork length in cm
  (0.0000137 * length_cm^2.972) * kg_to_lb
}


build_compare_table <- function(summed_results, MRIP_comparison_draw, md, eligible_dt = NULL) {
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

  model_long <- data.table::melt(
    data.table::as.data.table(summed_results)[, c("mode", common_metrics), with = FALSE],
    id.vars = "mode",
    measure.vars = common_metrics,
    variable.name = "metric",
    value.name = "model"
  )

  mrip_long <- data.table::melt(
    data.table::as.data.table(MRIP_comparison_draw)[, c("mode", common_metrics), with = FALSE],
    id.vars = "mode",
    measure.vars = common_metrics,
    variable.name = "metric",
    value.name = "MRIP"
  )

  cmp <- merge(model_long, mrip_long, by = c("mode", "metric"), all = FALSE)
  cmp[, c("species", "disposition") := data.table::tstrsplit(metric, "_", fixed = TRUE, keep = 1:2)]
  cmp[, `:=`(model = as.numeric(model), MRIP = as.numeric(MRIP))]
  cmp[, diff := model - MRIP]
  cmp[, pct_diff := data.table::fifelse(MRIP != 0, 100 * diff / MRIP, NA_real_)]
  cmp[, abs_diff_val := abs(diff)]
  cmp[, abs_pct_diff_val := data.table::fifelse(MRIP != 0, abs(100 * diff / MRIP), NA_real_)]
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

  out <- merge(compare_r, compare_k, by = c("mode", "species"), all = TRUE)
  out <- merge(out, compare_c, by = c("mode", "species"), all = TRUE)

  if (!is.null(eligible_dt)) {
    out <- merge(out, eligible_dt, by = c("mode", "species"), all.x = TRUE)
  } else {
    out[, original_rel_eligible := NA_real_]
  }
  out[is.na(original_rel_eligible), original_rel_eligible := 0]

  out[, rel_to_keep_new := data.table::fifelse(diff_keep < 0, 1L, 0L)]
  out[, keep_to_rel_new := data.table::fifelse(diff_keep > 0, 1L, 0L)]
  out[, p_rel_to_keep_new := pmin(1, abs(safe_divide(diff_keep, original_rel_eligible)))]
  out[, p_keep_to_rel_new := pmin(1, abs(safe_divide(diff_keep, model_keep)))]

  out[]
}

# Defaults supplied by the outer calibration routine.
for (sp in c("striper", "bluefish")) {
  if (!exists(paste0("keep_to_rel_", sp), inherits = FALSE)) assign(paste0("keep_to_rel_", sp), 0)
  if (!exists(paste0("rel_to_keep_", sp), inherits = FALSE)) assign(paste0("rel_to_keep_", sp), 0)
  if (!exists(paste0("p_rel_to_keep_", sp), inherits = FALSE)) assign(paste0("p_rel_to_keep_", sp), 0)
  if (!exists(paste0("p_keep_to_rel_", sp), inherits = FALSE)) assign(paste0("p_keep_to_rel_", sp), 0)
  if (!exists(paste0("all_keep_to_rel_", sp), inherits = FALSE)) assign(paste0("all_keep_to_rel_", sp), 0)
}

n_sub_striper_kept <- 0L
prop_sub_striper_kept <- 0
n_legal_striper_rel <- 0L
prop_legal_striper_rel <- 0
original_rel_eligible_striper <- 0L

n_sub_bluefish_kept <- 0L
prop_sub_bluefish_kept <- 0
n_legal_bluefish_rel <- 0L
prop_legal_bluefish_rel <- 0
original_rel_eligible_bluefish <- 0L

simulate_species_realloc <- function(catch_dt,
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
                                     length_summary = FALSE) {

  species_name <- match.arg(species_name)
  prefix <- if (species_name == "striper") "str" else "blu"

  key_cols <- c("date_parsed", "mode", "tripid", "catch_draw")
  keep_col <- paste0("tot_keep_", prefix, "_new")
  rel_col  <- paste0("tot_rel_",  prefix, "_new")
  keep_util_col <- paste0("tot_keep_", prefix, "_util")
  rel_util_col  <- paste0("tot_rel_",  prefix, "_util")
  keep_wt_col <- paste0("tot_keep_", prefix, "_weight_lb_new")
  rel_wt_col  <- paste0("tot_rel_",  prefix, "_weight_lb_new")

  check_required_cols(catch_dt, c(key_cols, catch_col, bag_col, min_col), "catch_dt")
  check_required_cols(size_dt, c("length", "fitted_prob"), "size_dt")

  if (nrow(size_dt) == 0L || all(is.na(size_dt$fitted_prob))) {
    stop("No usable catch-at-length probabilities for species = ", species_name,
         call. = FALSE)
  }

  out_zero <- unique(catch_dt[, ..key_cols])
  out_zero[, c(keep_col, rel_col, keep_util_col, rel_util_col,
               keep_wt_col, rel_wt_col) := .(0, 0, 0, 0, 0, 0)]

  if (length_summary) {
    out_zero[, `:=`(
      mean_len_keep_str_in = NA_real_,
      keep_length_str_in = 0,
      keep_length_str_in2 = 0,
      mean_len_rel_str_in = NA_real_,
      rel_log_length_str = 0
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
    return(list(
      trip = out_zero[],
      n_sub_kept = 0L,
      prop_sub_kept = 0,
      n_legal_rel = 0L,
      prop_legal_rel = 0,
      original_rel_eligible = 0L
    ))
  }

  fish_dt <- pos_dt[rep(seq_len(.N), catch_n)]
  fish_dt[, fishid := seq_len(.N)]
  fish_dt[, fitted_length := sample(size_dt$length, .N, replace = TRUE, prob = size_dt$fitted_prob)]
  setorder(fish_dt, date_parsed, mode, tripid, catch_draw, fishid)

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

  if (species_name == "striper") {
    fish_dt[, fish_weight_lb := striper_weight_lb(length_in)]
  } else {
    fish_dt[, fish_weight_lb := bluefish_weight_lb(fitted_length)]
  }
  # Candidate pool for release -> keep. For striped bass, allow fish no more than
  # 3 inches below the minimum OR fish above the maximum slot. For bluefish,
  # use the floor supplied by the outer routine.
  if (species_name == "striper") {
    fish_dt[, rel_to_keep_eligible := data.table::fifelse(
      release == 1L & (fitted_length >= floor_sublegal | fitted_length > max_sz), 1L, 0L
    )]
  } else {
    fish_dt[, rel_to_keep_eligible := data.table::fifelse(
      release == 1L & fitted_length >= floor_sublegal, 1L, 0L
    )]
  }

  original_rel_eligible <- fish_dt[, sum(rel_to_keep_eligible, na.rm = TRUE)]
  n_sub_kept <- 0L
  prop_sub_kept <- 0
  n_legal_rel <- 0L
  prop_legal_rel <- 0

  if (isTRUE(rel_to_keep == 1) && original_rel_eligible > 0) {
    realloc_dt <- fish_dt[rel_to_keep_eligible == 1L]
    base_dt    <- fish_dt[rel_to_keep_eligible != 1L]

    realloc_dt[, u := runif(.N)]
    setorder(realloc_dt, u)
    n_take <- round(p_rel_to_keep * nrow(realloc_dt))
    n_take <- max(0L, min(n_take, nrow(realloc_dt)))

    realloc_dt[, idx := seq_len(.N)]
    realloc_dt[, keep_new := data.table::fifelse(idx <= n_take, 1L, 0L)]
    realloc_dt[, rel_new  := data.table::fifelse(keep_new == 1L, 0L, release)]
    realloc_dt[, `:=`(keep = data.table::fifelse(keep_new == 1L, 1L, keep),
                      release = rel_new)]
    realloc_dt[, c("u", "idx", "keep_new", "rel_new") := NULL]

    n_sub_kept <- n_take
    prop_sub_kept <- safe_divide(n_sub_kept, original_rel_eligible)
    fish_dt <- rbindlist(list(realloc_dt, base_dt), use.names = TRUE, fill = TRUE)
  }

  if (isTRUE(keep_to_rel == 1) && fish_dt[, sum(keep, na.rm = TRUE)] > 0) {
    if (isTRUE(all_keep_to_rel == 1)) {
      n_legal_rel <- fish_dt[, sum(keep, na.rm = TRUE)]
      prop_legal_rel <- 1
      fish_dt[, kept_to_released_flag := keep]
      fish_dt[, release := release + keep]
      fish_dt[, keep := 0L]
    } else {
      realloc_dt <- fish_dt[keep == 1L]
      base_dt    <- fish_dt[keep != 1L]

      realloc_dt[, u := runif(.N)]
      setorder(realloc_dt, u)
      n_move <- round(p_keep_to_rel * nrow(realloc_dt))
      n_move <- max(0L, min(n_move, nrow(realloc_dt)))

      realloc_dt[, idx := seq_len(.N)]
      realloc_dt[, move := data.table::fifelse(idx <= n_move, 1L, 0L)]
      realloc_dt[, kept_to_released_flag := move]
      realloc_dt[, release := data.table::fifelse(move == 1L, 1L, release)]
      realloc_dt[, keep := data.table::fifelse(move == 1L, 0L, keep)]
      realloc_dt[, c("u", "idx", "move") := NULL]

      n_legal_rel <- n_move
      prop_legal_rel <- safe_divide(n_legal_rel, nrow(realloc_dt))
      fish_dt <- rbindlist(list(realloc_dt, base_dt), use.names = TRUE, fill = TRUE)
    }
  }

  fish_dt[, keep_util := data.table::fifelse(keep == 1L | kept_to_released_flag == 1L, 1L, 0L)]
  fish_dt[, release_util := data.table::fifelse(release == 1L & kept_to_released_flag == 0L, 1L, 0L)]
  fish_dt[, keep_weight_lb := keep * fish_weight_lb]
  fish_dt[, release_weight_lb := release * fish_weight_lb]

  if (length_summary) {
    trip_pos <- fish_dt[, .(
      keep_n = sum(keep, na.rm = TRUE),
      rel_n  = sum(release, na.rm = TRUE),
      keep_util_n = sum(keep_util, na.rm = TRUE),
      rel_util_n  = sum(release_util, na.rm = TRUE),
      keep_weight_lb = sum(keep_weight_lb, na.rm = TRUE),
      release_weight_lb = sum(release_weight_lb, na.rm = TRUE),
      mean_len_keep_str_in = data.table::fifelse(sum(keep_util, na.rm = TRUE) > 0,
                                                 mean(length_in[keep_util == 1L], na.rm = TRUE),
                                                 NA_real_),
      mean_len_rel_str_in = data.table::fifelse(sum(release_util, na.rm = TRUE) > 0,
                                                mean(length_in[release_util == 1L], na.rm = TRUE),
                                                NA_real_),
      rel_log_length_str = data.table::fifelse(sum(release_util, na.rm = TRUE) > 0,
                                               mean(log(length_in[release_util == 1L]), na.rm = TRUE),
                                               0)
    ), by = key_cols]

    trip_pos[, `:=`(
      keep_length_str_in = data.table::fifelse(keep_util_n > 0 & !is.na(mean_len_keep_str_in),
                                               mean_len_keep_str_in, 0),
      keep_length_str_in2 = data.table::fifelse(keep_util_n > 0 & !is.na(mean_len_keep_str_in),
                                                mean_len_keep_str_in^2, 0)
    )]
  } else {
    trip_pos <- fish_dt[, .(
      keep_n = sum(keep, na.rm = TRUE),
      rel_n  = sum(release, na.rm = TRUE),
      keep_util_n = sum(keep_util, na.rm = TRUE),
      rel_util_n  = sum(release_util, na.rm = TRUE),
      keep_weight_lb = sum(keep_weight_lb, na.rm = TRUE),
      release_weight_lb = sum(release_weight_lb, na.rm = TRUE)
    ), by = key_cols]
  }

  setnames(
    trip_pos,
    old = c("keep_n", "rel_n", "keep_util_n", "rel_util_n",
            "keep_weight_lb", "release_weight_lb"),
    new = c(keep_col, rel_col, keep_util_col, rel_util_col,
            keep_wt_col, rel_wt_col)
  )

  trip_out <- rbindlist(list(trip_pos, out_zero[!trip_pos, on = key_cols]),
                        use.names = TRUE, fill = TRUE)

  zero_cols <- c(keep_col, rel_col, keep_util_col, rel_util_col,
                 keep_wt_col, rel_wt_col,
                 "keep_length_str_in", "keep_length_str_in2", "rel_log_length_str")

  for (cc in intersect(zero_cols, names(trip_out))) {
    set(trip_out, which(is.na(trip_out[[cc]])), cc, 0)
  }

  setkeyv(trip_out, key_cols)
  list(
    trip = trip_out[],
    n_sub_kept = n_sub_kept,
    prop_sub_kept = prop_sub_kept,
    n_legal_rel = n_legal_rel,
    prop_legal_rel = prop_legal_rel,
    original_rel_eligible = original_rel_eligible
  )
}

#-------------------------------------------------
# One region-draw-mode run. Expects s, i, md, MRIP_comparison, and directory globals.
#-------------------------------------------------

# Preload size lookup if the routine did not already do so.
if (!exists("size_lookup", inherits = TRUE)) {
  size_lookup <- as.data.table(
    readr::read_csv(file.path(input_data_cd, "baseline_catch_at_length_multi_region.csv"),
                    show_col_types = FALSE)
  )
  check_required_cols(size_lookup, c("region", "draw", "species", "fitted_prob", "length"),
                      "baseline_catch_at_length_multi_region.csv")
  size_lookup <- size_lookup[!is.na(fitted_prob), .(region, draw, species, fitted_prob, length)]
  setkey(size_lookup, region, draw, species)
}

MRIP_comparison_draw <- as.data.table(MRIP_comparison)[draw == i & region == s & mode == md,
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

dtrip_region <- as.data.table(
  fst::read_fst(file.path(iterative_input_data_cd, paste0("directed_trip_draws_", s, ".fst")))
)
if (!"date_parsed" %in% names(dtrip_region)) dtrip_region[, date_parsed := parse_date_any(date)]

dtripz <- dtrip_region[draw == i & mode == md,
                       .(mode, date_parsed, striper_bag, striper_min, striper_max,
                         bluefish_bag, bluefish_min, dtrip)]

if (nrow(dtripz) == 0L || sum(dtripz$dtrip, na.rm = TRUE) == 0) {
  summed_results <- data.table(
    mode = md,
    striper_catch = 0, striper_keep = 0, striper_rel = 0,
    bluefish_catch = 0, bluefish_keep = 0, bluefish_rel = 0,
    estimated_trips = 0, n_choice_occasions = 0,
    mean_len_keep_str_in = NA_real_, mean_len_rel_str_in = NA_real_
  )
  calib_comparison1 <- build_compare_table(summed_results, MRIP_comparison_draw, md)
  calib_comparison1[, `:=`(draw = i, region = s)]
} else {
  catch_draw_dt <- as.data.table(
    fst::read_fst(file.path(iterative_input_data_cd, paste0("calib_catch_draws_processed_", s,"_", i, ".fst")))
  )
  if (!"date_parsed" %in% names(catch_draw_dt)) catch_draw_dt[, date_parsed := parse_date_any(date)]

  angler_draw_dt <- as.data.table(
    fst::read_fst(file.path(iterative_input_data_cd, paste0("pref_params_and_dems_", s,"_", i, ".fst")))
  )
  if (!"date_parsed" %in% names(angler_draw_dt)) angler_draw_dt[, date_parsed := parse_date_any(date)]

  if (!"str_cat" %in% names(catch_draw_dt)) {
    check_required_cols(catch_draw_dt, c("str_cat"), "catch_draw_dt")
  }
  if (!"blu_cat" %in% names(catch_draw_dt)) {
    check_required_cols(catch_draw_dt, c("blu_cat"), "catch_draw_dt")
  }

  striper_size_data <- size_lookup[list(s, i, "str"), .(fitted_prob, length)]
  bluefish_size_data <- size_lookup[list(s, i, "blu"), .(fitted_prob, length)]

  catch_data <- merge(catch_draw_dt[mode == md], dtripz, by = c("mode", "date_parsed"), all.x = TRUE)
  angler_dems <- angler_draw_dt[mode == md]

  floor_subl_str_harv <- min(dtripz$striper_min, na.rm = TRUE) - 1.5 * 2.54

  min_blu <- min(dtripz$bluefish_min, na.rm = TRUE)
  floor_subl_blu_harv <- if (is.na(min_blu) || min_blu == 0) 10 * 2.54 else min_blu - 2 * 2.54

  str_res <- simulate_species_realloc(
    catch_dt = catch_data,
    catch_col = "str_cat",
    bag_col = "striper_bag",
    min_col = "striper_min",
    max_col = "striper_max",
    size_dt = striper_size_data,
    species_name = "striper",
    floor_sublegal = floor_subl_str_harv,
    rel_to_keep = rel_to_keep_striper,
    keep_to_rel = keep_to_rel_striper,
    p_rel_to_keep = p_rel_to_keep_striper,
    p_keep_to_rel = p_keep_to_rel_striper,
    all_keep_to_rel = all_keep_to_rel_striper,
    length_summary = TRUE
  )

  blu_res <- simulate_species_realloc(
    catch_dt = catch_data,
    catch_col = "blu_cat",
    bag_col = "bluefish_bag",
    min_col = "bluefish_min",
    max_col = NULL,
    size_dt = bluefish_size_data,
    species_name = "bluefish",
    floor_sublegal = floor_subl_blu_harv,
    rel_to_keep = rel_to_keep_bluefish,
    keep_to_rel = keep_to_rel_bluefish,
    p_rel_to_keep = p_rel_to_keep_bluefish,
    p_keep_to_rel = p_keep_to_rel_bluefish,
    all_keep_to_rel = all_keep_to_rel_bluefish,
    length_summary = FALSE
  )

  n_sub_striper_kept <- str_res$n_sub_kept
  prop_sub_striper_kept <- str_res$prop_sub_kept
  n_legal_striper_rel <- str_res$n_legal_rel
  prop_legal_striper_rel <- str_res$prop_legal_rel
  original_rel_eligible_striper <- str_res$original_rel_eligible

  n_sub_bluefish_kept <- blu_res$n_sub_kept
  prop_sub_bluefish_kept <- blu_res$prop_sub_kept
  n_legal_bluefish_rel <- blu_res$n_legal_rel
  prop_legal_bluefish_rel <- blu_res$prop_legal_rel
  original_rel_eligible_bluefish <- blu_res$original_rel_eligible

  key_cols <- c("date_parsed", "mode", "tripid", "catch_draw")
  trip_data <- merge(str_res$trip, blu_res$trip, by = key_cols, all = TRUE)

  zero_fill_cols <- grep("^tot_(keep|rel)_(str|blu)_(new|util)$", names(trip_data), value = TRUE)
  zero_fill_cols <- c(zero_fill_cols, "keep_length_str_in", "keep_length_str_in2", "rel_log_length_str")
  for (cc in intersect(zero_fill_cols, names(trip_data))) set(trip_data, which(is.na(trip_data[[cc]])), cc, 0)

  trip_data[, `:=`(
    tot_str_catch = tot_keep_str_new + tot_rel_str_new,
    tot_blu_catch = tot_keep_blu_new + tot_rel_blu_new
  )]

  setkey(angler_dems, date_parsed, mode, tripid)
  trip_data <- merge(trip_data, angler_dems, by = c("date_parsed", "mode", "tripid"), all.x = TRUE)

  baseline_outcomes <- copy(trip_data)
  setnames(
    baseline_outcomes,
    old = c("keep_length_str_in", "keep_length_str_in2",
            "mean_len_keep_str_in", "mean_len_rel_str_in",
            "rel_log_length_str","tot_blu_catch",
            "tot_keep_blu_new","tot_keep_blu_util",
            "tot_keep_str_new", "tot_keep_str_util",
            "tot_rel_blu_new","tot_rel_blu_util",
            "tot_rel_str_new","tot_rel_str_util", "tot_str_catch"),
    new = c("keep_length_str_in_base", "keep_length_str_in2_base",
            "mean_len_keep_str_in_base", "mean_len_rel_str_in_base",
            "rel_log_length_str_base","tot_blu_catch_base",
            "tot_keep_blu_base","tot_keep_blu_util_base",
            "tot_keep_str_base", "tot_keep_str_util_base",
            "tot_rel_blu_base","tot_rel_blu_util_base",
            "tot_rel_str_base","tot_rel_str_util_base", "tot_str_catch_base"),
    skip_absent = TRUE
  )

  # save baseline outcomes
  fst::write_fst(
    baseline_outcomes,
    file.path(final_process_outcomes_cd,
              paste0("base_outcomes_", s, "_", md, "_", i, ".fst"))
  )

  trip_data[, `:=`(
    vA_trip =
      beta_str_kp * sqrt(tot_keep_str_util) +
      beta_str_kp_L * (sqrt(tot_keep_str_util) * keep_length_str_in) +
      beta_str_kp_L2 * (sqrt(tot_keep_str_util) * keep_length_str_in2) +
      beta_str_rel * sqrt(tot_rel_str_util) +
      beta_str_rel_L * (sqrt(tot_rel_str_util) * rel_log_length_str) +
      beta_blu_keep * sqrt(tot_keep_blu_util) +
      beta_blu_rel * sqrt(tot_rel_blu_util) +
      beta_cost * cost_sim,

    vA_optout = beta_optout + beta_optout_male * male,

    vA_other =
      beta_optoth +
      beta_optoth_days_non_str * days_non_str_blue12 +
      beta_optoth_likely * likely_to_fish +
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

  mean_trip_data[, prob0 := calc_prob_trip(vA_trip, vA_optout, vA_other)]
  mean_trip_data[, c("vA_trip", "vA_optout", "vA_other", "catch_draw") := NULL]

  accounting_cols <- c(
    "tot_keep_str_new", "tot_rel_str_new", "tot_str_catch",
    "tot_keep_blu_new", "tot_rel_blu_new", "tot_blu_catch",
    "tot_keep_str_weight_lb_new", "tot_rel_str_weight_lb_new",
    "tot_keep_blu_weight_lb_new", "tot_rel_blu_weight_lb_new"
  )

  mean_trip_data[, (accounting_cols) := lapply(.SD, function(x) x * prob0),
                 .SDcols = accounting_cols]

  mean_trip_data <- merge(mean_trip_data, dtripz[, .(mode, date_parsed, dtrip)],
                          by = c("mode", "date_parsed"), all.x = TRUE)

  # Diagnose failed directed-trip matches before expansion. If dtrip is NA,
  # multiplying by expand creates NA values that can otherwise be hidden by
  # sum(..., na.rm = TRUE) in the aggregate output.
  missing_dtrip_keys <- unique(mean_trip_data[is.na(dtrip), .(mode, date_parsed)])
  if (nrow(missing_dtrip_keys) > 0L) {
    warning("Missing directed-trip totals for ", nrow(missing_dtrip_keys),
            " mode/date_parsed cells. n_choice_occasions and estimated_trips ",
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
  mean_trip_data[, (expand_cols) := lapply(.SD, function(x) x * expand), .SDcols = expand_cols]

  for (jj in names(mean_trip_data)) data.table::setattr(mean_trip_data[[jj]], "label", NULL)

  aggregate_trip_data <- mean_trip_data[, .(
    tot_keep_str_new = sum(tot_keep_str_new, na.rm = TRUE),
    tot_rel_str_new  = sum(tot_rel_str_new, na.rm = TRUE),
    tot_str_catch    = sum(tot_str_catch, na.rm = TRUE),
    tot_keep_blu_new = sum(tot_keep_blu_new, na.rm = TRUE),
    tot_rel_blu_new  = sum(tot_rel_blu_new, na.rm = TRUE),
    tot_blu_catch    = sum(tot_blu_catch, na.rm = TRUE),
    tot_keep_str_weight_lb_new = sum(tot_keep_str_weight_lb_new, na.rm = TRUE),
    tot_rel_str_weight_lb_new  = sum(tot_rel_str_weight_lb_new, na.rm = TRUE),
    tot_keep_blu_weight_lb_new = sum(tot_keep_blu_weight_lb_new, na.rm = TRUE),
    tot_rel_blu_weight_lb_new  = sum(tot_rel_blu_weight_lb_new, na.rm = TRUE),
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

  aggregate_trip_data[, month := lubridate::month(date_parsed)]

  calib_monthly_stats1 <- data.table::rbindlist(list(
    aggregate_trip_data[
      ,
      .(
        model_catch = sum(striper_catch, na.rm = TRUE),
        model_keep  = sum(striper_keep, na.rm = TRUE),
        model_rel   = sum(striper_rel, na.rm = TRUE),
        model_keep_weight_lb = sum(tot_keep_str_weight_lb_new, na.rm = TRUE),
        model_rel_weight_lb  = sum(tot_rel_str_weight_lb_new, na.rm = TRUE),
        estimated_trips = safe_sum_numeric(estimated_trips),
        n_choice_occasions = safe_sum_numeric(n_choice_occasions),
        mean_len_keep_str_in = wtd_mean_safe(mean_len_keep_str_in, striper_keep),
        mean_len_rel_str_in  = wtd_mean_safe(mean_len_rel_str_in, striper_rel)
      ),
      by = .(mode, month)
    ][, species := "striper"],

    aggregate_trip_data[
      ,
      .(
        model_catch = sum(bluefish_catch, na.rm = TRUE),
        model_keep  = sum(bluefish_keep, na.rm = TRUE),
        model_rel   = sum(bluefish_rel, na.rm = TRUE),
        model_keep_weight_lb = sum(tot_keep_blu_weight_lb_new, na.rm = TRUE),
        model_rel_weight_lb  = sum(tot_rel_blu_weight_lb_new, na.rm = TRUE),
        estimated_trips = safe_sum_numeric(estimated_trips),
        n_choice_occasions = safe_sum_numeric(n_choice_occasions),
        mean_len_keep_str_in = NA_real_,
        mean_len_rel_str_in  = NA_real_
      ),
      by = .(mode, month)
    ][, species := "bluefish"]
  ), use.names = TRUE, fill = TRUE)

  calib_monthly_stats1[, `:=`(
    region = s,
    draw = i
  )]

  data.table::setcolorder(
    calib_monthly_stats1,
    c(
      "region", "mode", "draw", "species", "month",
      setdiff(names(calib_monthly_stats1), c("region", "mode", "draw", "species", "month"))
    )
  )


  # save # of choice ocassions
  n_choice_out <- aggregate_trip_data[, .(date_parsed, mode, n_choice_occasions, estimated_trips)]

   fst::write_fst(
    n_choice_out,
    file.path(final_process_choice_occasions_cd,
              paste0("n_choice_occasions_", s, "_", md, "_", i,".fst"))
  )

  summed_results <- aggregate_trip_data[, .(
    bluefish_catch = sum(bluefish_catch, na.rm = TRUE),
    bluefish_keep  = sum(bluefish_keep, na.rm = TRUE),
    bluefish_rel   = sum(bluefish_rel, na.rm = TRUE),
    striper_catch  = sum(striper_catch, na.rm = TRUE),
    striper_keep   = sum(striper_keep, na.rm = TRUE),
    striper_rel    = sum(striper_rel, na.rm = TRUE),
    striper_keep_weight_lb  = sum(tot_keep_str_weight_lb_new, na.rm = TRUE),
    striper_rel_weight_lb   = sum(tot_rel_str_weight_lb_new, na.rm = TRUE),
    bluefish_keep_weight_lb = sum(tot_keep_blu_weight_lb_new, na.rm = TRUE),
    bluefish_rel_weight_lb  = sum(tot_rel_blu_weight_lb_new, na.rm = TRUE),
    estimated_trips = safe_sum_numeric(estimated_trips),
    n_choice_occasions = safe_sum_numeric(n_choice_occasions),
    mean_len_keep_str_in = wtd_mean_safe(mean_len_keep_str_in, striper_keep),
    mean_len_rel_str_in  = wtd_mean_safe(mean_len_rel_str_in,  striper_rel)
  ), by = mode]

  eligible_dt <- data.table(
    mode = md,
    species = c("striper", "bluefish"),
    original_rel_eligible = c(original_rel_eligible_striper, original_rel_eligible_bluefish)
  )

  calib_comparison1 <- build_compare_table(summed_results, MRIP_comparison_draw, md, eligible_dt = eligible_dt)
  weight_long <- data.table::rbindlist(list(
    data.table::data.table(
      mode = md,
      species = "striper",
      model_keep_weight_lb = summed_results$striper_keep_weight_lb,
      model_rel_weight_lb  = summed_results$striper_rel_weight_lb
    ),
    data.table::data.table(
      mode = md,
      species = "bluefish",
      model_keep_weight_lb = summed_results$bluefish_keep_weight_lb,
      model_rel_weight_lb  = summed_results$bluefish_rel_weight_lb
    )
  ), use.names = TRUE, fill = TRUE)

  calib_comparison1 <- merge(
    calib_comparison1,
    weight_long,
    by = c("mode", "species"),
    all.x = TRUE
  )

  calib_comparison1[, `:=`(
    draw = i,
    region = s,
    mean_len_keep_str_in = summed_results$mean_len_keep_str_in[1],
    mean_len_rel_str_in  = summed_results$mean_len_rel_str_in[1]
  )]
}
