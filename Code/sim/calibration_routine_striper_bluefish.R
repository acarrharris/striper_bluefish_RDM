# Iterative calibration routine for striped bass / bluefish.
#
# This routine calibrates total harvest numbers only, by region x mode x draw x species.
# It no longer attempts to calibrate striped bass harvest by length bin.
#
# Convergence target:
#   abs(model_keep - MRIP_keep) < 500 fish OR abs(percent difference) < 5%.
#
# It calls calibrate_rec_catch1_striper_bluefish_simplified.R, which applies
# trip-level release->keep or keep->release reallocations and recomputes expected
# utility using the simplified striper length specification.

library(data.table)
library(fst)
library(haven)
library(readr)

# ----------------------------
# MRIP targets and simulation0 output
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

baseline_output0 <- as.data.table(
  fst::read_fst(file.path(final_process_misc_cd, "calibration_comparison50.fst"))
)

# Reconstruct catch comparison defensively if simulation0 did not write it.
if (!("MRIP_catch" %in% names(baseline_output0)) && all(c("MRIP_keep", "MRIP_rel") %in% names(baseline_output0))) {
  baseline_output0[, MRIP_catch := MRIP_keep + MRIP_rel]
}
if (!("model_catch" %in% names(baseline_output0)) && all(c("model_keep", "model_rel") %in% names(baseline_output0))) {
  baseline_output0[, model_catch := model_keep + model_rel]
}
if (!("diff_catch" %in% names(baseline_output0)) && all(c("model_catch", "MRIP_catch") %in% names(baseline_output0))) {
  baseline_output0[, diff_catch := model_catch - MRIP_catch]
}
if (!("pct_diff_catch" %in% names(baseline_output0)) && all(c("diff_catch", "MRIP_catch") %in% names(baseline_output0))) {
  baseline_output0[, pct_diff_catch := fifelse(MRIP_catch != 0, 100 * diff_catch / MRIP_catch, NA_real_)]
}

mode_draw <- if (exists("mode_draw")) mode_draw else c("pr", "fh", "sh")
draws <- if (exists("draws")) draws else seq_len(n_simulations)
region_draw <- if (exists("region_draw")) region_draw else sort(unique(baseline_output0$region))

species_vec <- c("striper", "bluefish")

tol_abs_fish <- 500
tol_abs_pct  <- 5
max_iter <- 25
p_tol <- 1e-4

# Preload baseline catch-at-length once so calibrate_rec_catch1 does not reread it each iteration.
size_lookup <- as.data.table(
  readr::read_csv(file.path(input_data_cd, "baseline_catch_at_length_multi_region.csv"), show_col_types = FALSE)
)
size_lookup <- size_lookup[!is.na(fitted_prob), .(region, draw, species, fitted_prob, length)]
setkey(size_lookup, region, draw, species)

# ----------------------------
# Helpers
# ----------------------------

is_achieved <- function(diff_keep, pct_diff_keep, MRIP_keep = NA_real_) {
  if (is.finite(MRIP_keep) && MRIP_keep == 0) {
    return(is.finite(diff_keep) && abs(diff_keep) < tol_abs_fish)
  }

  (is.finite(diff_keep) && abs(diff_keep) < tol_abs_fish) ||
    (is.finite(pct_diff_keep) && abs(pct_diff_keep) < tol_abs_pct)
}

score_species <- function(diff_keep, pct_diff_keep, diff_catch, pct_diff_catch,
                          MRIP_keep = NA_real_, MRIP_catch = NA_real_) {
  keep_score <- if (is.finite(MRIP_keep) && MRIP_keep == 0) {
    if (is.finite(diff_keep)) abs(diff_keep) / tol_abs_fish else Inf
  } else {
    min(
      if (is.finite(diff_keep)) abs(diff_keep) / tol_abs_fish else Inf,
      if (is.finite(pct_diff_keep)) abs(pct_diff_keep) / tol_abs_pct else Inf
    )
  }

  catch_score <- if (is.finite(MRIP_catch) && MRIP_catch == 0) {
    if (is.finite(diff_catch)) abs(diff_catch) / (5 * tol_abs_fish) else Inf
  } else {
    min(
      if (is.finite(diff_catch)) abs(diff_catch) / (5 * tol_abs_fish) else Inf,
      if (is.finite(pct_diff_catch)) abs(pct_diff_catch) / (4 * tol_abs_pct) else Inf
    )
  }

  keep_score + 0.15 * catch_score
}

extract_species_row <- function(dt, sp, md, s, i) {
  out <- as.data.table(dt)[species == sp]
  if (nrow(out) == 0L) {
    out <- data.table(
      mode = md, species = sp,
      MRIP_keep = NA_real_, model_keep = 0, diff_keep = NA_real_, pct_diff_keep = NA_real_,
      MRIP_rel = NA_real_, model_rel = 0, diff_rel = NA_real_, pct_diff_rel = NA_real_,
      MRIP_catch = NA_real_, model_catch = 0, diff_catch = NA_real_, pct_diff_catch = NA_real_,
      rel_to_keep_new = 0, keep_to_rel_new = 0,
      p_rel_to_keep_new = 0, p_keep_to_rel_new = 0,
      original_rel_eligible = 0,
      draw = i, region = s
    )
  }
  out[1]
}

make_species_state <- function(base_row) {
  mrip_keep  <- as.numeric(base_row$MRIP_keep)
  model_keep <- as.numeric(base_row$model_keep)
  diff_keep  <- as.numeric(base_row$diff_keep)

  # If both target and model harvest are zero, run catch1 once as a no-op so
  # downstream objects can still be computed consistently.
  if (is.finite(mrip_keep) && mrip_keep == 0 && is.finite(model_keep) && model_keep == 0) {
    return(list(
      direction = "rel_to_keep",
      p = 0,
      lo = 0,
      hi = 0,
      achieved = FALSE,
      convergence = 1L,
      best_score = Inf,
      best_row = NULL,
      frozen = FALSE,
      best_p = 0,
      no_realloc_needed = TRUE
    ))
  }

  if (is.finite(diff_keep) && diff_keep < 0) {
    direction <- "rel_to_keep"
    p0 <- if ("p_rel_to_keep_new" %in% names(base_row)) base_row$p_rel_to_keep_new else base_row$p_rel_to_keep
  } else if (is.finite(diff_keep) && diff_keep > 0) {
    direction <- "keep_to_rel"
    p0 <- if ("p_keep_to_rel_new" %in% names(base_row)) base_row$p_keep_to_rel_new else base_row$p_keep_to_rel
  } else {
    direction <- "none"
    p0 <- 0
  }

  p0 <- suppressWarnings(as.numeric(p0))
  if (!is.finite(p0)) p0 <- 0
  p0 <- max(0, min(1, p0))

  # If there is no finite harvest gap, treat this species/cell as a no-op.
  # This occurs most commonly when MRIP_keep is missing or when MRIP_keep == 0
  # and a percent difference is undefined. Passing NA probabilities into the
  # bracket logic can trigger missing-value errors in ordinary if statements.
  if (!is.finite(diff_keep) || direction == "none") {
    return(list(
      direction = "none",
      p = 0,
      lo = 0,
      hi = 0,
      achieved = TRUE,
      convergence = 1L,
      best_score = 0,
      best_row = NULL,
      frozen = TRUE,
      best_p = 0,
      no_realloc_needed = TRUE
    ))
  }

  list(
    direction = direction,
    p = p0,
    lo = 0,
    hi = if (p0 > 0 && p0 < 1) 1 else NA_real_,
    achieved = FALSE,
    convergence = 1L,
    best_score = Inf,
    best_row = NULL,
    frozen = FALSE,
    best_p = p0,
    no_realloc_needed = FALSE
  )
}

push_globals <- function(states_by_sp, target_env = parent.frame()) {
  for (sp in species_vec) {
    state <- states_by_sp[[sp]]
    assign(paste0("rel_to_keep_", sp),
           as.integer(state$direction == "rel_to_keep"),
           envir = target_env)
    assign(paste0("keep_to_rel_", sp),
           as.integer(state$direction == "keep_to_rel"),
           envir = target_env)
    assign(paste0("p_rel_to_keep_", sp),
           if (state$direction == "rel_to_keep") state$p else 0,
           envir = target_env)
    assign(paste0("p_keep_to_rel_", sp),
           if (state$direction == "keep_to_rel") state$p else 0,
           envir = target_env)
    assign(paste0("all_keep_to_rel_", sp),
           as.integer(state$direction == "keep_to_rel" && state$p >= 1 - p_tol),
           envir = target_env)
  }
}

update_bracket <- function(state, row) {
  if (isTRUE(state$frozen)) return(state)

  if (state$direction == "none") {
    state$achieved <- TRUE
    state$frozen <- TRUE
    state$best_row <- copy(row)
    state$best_p <- 0
    return(state)
  }

  diff_keep <- as.numeric(row$diff_keep)
  pct_diff_keep <- as.numeric(row$pct_diff_keep)
  diff_catch <- as.numeric(row$diff_catch)
  pct_diff_catch <- as.numeric(row$pct_diff_catch)
  MRIP_keep <- as.numeric(row$MRIP_keep)
  MRIP_catch <- as.numeric(row$MRIP_catch)

  state$achieved <- is_achieved(diff_keep, pct_diff_keep, MRIP_keep)

  if (isTRUE(state$no_realloc_needed)) {
    state$best_row <- copy(row)
    state$best_score <- -Inf
    state$best_p <- 0
    state$frozen <- TRUE
    state$achieved <- TRUE
    return(state)
  }

  this_score <- score_species(diff_keep, pct_diff_keep, diff_catch, pct_diff_catch,
                              MRIP_keep, MRIP_catch)

  if (state$achieved) {
    state$best_row <- copy(row)
    state$best_score <- -Inf
    state$best_p <- state$p
    state$frozen <- TRUE
    return(state)
  }

  if (this_score < state$best_score) {
    state$best_score <- this_score
    state$best_row <- copy(row)
    state$best_p <- state$p
  }

  if (state$direction == "rel_to_keep") {
    # Larger p means more harvest.
    if (is.finite(diff_keep) && diff_keep < 0) {
      state$lo <- max(state$lo, state$p)
    } else if (is.finite(diff_keep) && diff_keep > 0) {
      state$hi <- if (is.na(state$hi)) state$p else min(state$hi, state$p)
    }
  } else if (state$direction == "keep_to_rel") {
    # Larger p means less harvest.
    if (is.finite(diff_keep) && diff_keep > 0) {
      state$lo <- max(state$lo, state$p)
    } else if (is.finite(diff_keep) && diff_keep < 0) {
      state$hi <- if (is.na(state$hi)) state$p else min(state$hi, state$p)
    }
  }

  old_p <- state$p
  if (!is.na(state$hi)) {
    state$p <- (state$lo + state$hi) / 2
  } else {
    state$p <- if (old_p == 0) 0.1 else min(1, max(old_p * 1.5, old_p + 0.05))
  }
  state$p <- max(0, min(1, state$p))

  if (abs(state$p - old_p) < p_tol && !state$achieved) state$convergence <- 0L
  if (state$p >= 1 - p_tol && is.na(state$hi) && !state$achieved) state$convergence <- 0L

  state
}

print_calib_progress <- function(row, iter_used, s, md, i) {
  cat("\n--------------------------------------------------\n")
  cat("Striper/bluefish calibration progress\n")
  cat("Region:", s, "| Mode:", md, "| Draw:", i,
      "| Species:", row$species, "| Iteration:", iter_used, "\n")
  print(data.table::data.table(
    MRIP_keep = row$MRIP_keep,
    model_keep = row$model_keep,
    diff_keep = row$diff_keep,
    pct_diff_keep = row$pct_diff_keep
  ))
  cat("--------------------------------------------------\n")
}

# ----------------------------
# Main loop
# ----------------------------
region_draw <- c("NYHUDS", "RIALL", "VACHES", "VAOCN", "VAPOTO")

# region_draw<-c("CTALL")
# mode_draw<-"pr"
 draws<-1:50

calibrated <- vector("list", length(region_draw) * length(mode_draw) * length(draws))
k <- 1L
calibrated_monthly <- list()
km <- 1L

 # s<-"CTALL"
 # md<-"pr"
 # i<-1

for (s in region_draw) {
  for (md in mode_draw) {
    for (i in draws) {

      baseline_targets_current <- baseline_output0[region == s & draw == i & mode == md]
      if (nrow(baseline_targets_current) == 0L) next

      if (all(is.na(baseline_targets_current$MRIP_keep))) {
        out <- copy(baseline_targets_current)
        out[, `:=`(
          keep_to_rel = 0, rel_to_keep = 0,
          p_rel_to_keep = 0, p_keep_to_rel = 0,
          convergence = NA_integer_, iter_used = 0L
        )]
        calibrated[[k]] <- out
        k <- k + 1L
        next
      }

      states_by_sp <- setNames(vector("list", length(species_vec)), species_vec)
      for (sp in species_vec) {
        base_row <- extract_species_row(baseline_targets_current, sp, md, s, i)
        states_by_sp[[sp]] <- make_species_state(base_row)
        states_by_sp[[sp]]$best_row <- copy(base_row)
        states_by_sp[[sp]]$best_score <- score_species(
          base_row$diff_keep,
          base_row$pct_diff_keep,
          base_row$diff_catch,
          base_row$pct_diff_catch,
          base_row$MRIP_keep,
          base_row$MRIP_catch
        )
        states_by_sp[[sp]]$achieved <- is_achieved(
          base_row$diff_keep,
          base_row$pct_diff_keep,
          base_row$MRIP_keep
        )
      }

      iter_used <- 0L
      last_result <- NULL

      repeat {
        push_globals(states_by_sp, target_env = environment())
        source(file.path(code_cd, "calibrate_rec_catch1_striper_bluefish.R"), local = environment())

        last_result <- copy(as.data.table(calib_comparison1))

        last_monthly_result <- if (exists("calib_monthly_stats1")) {
          copy(as.data.table(calib_monthly_stats1))
        } else {
          NULL
        }

        all_done <- TRUE
        for (sp in species_vec) {
          row <- extract_species_row(last_result, sp, md, s, i)
          states_by_sp[[sp]] <- update_bracket(states_by_sp[[sp]], row)
          if (!states_by_sp[[sp]]$achieved && states_by_sp[[sp]]$convergence == 1L) {
            all_done <- FALSE
          }
        }

        for (sp in species_vec) {
          print_calib_progress(states_by_sp[[sp]]$best_row,
                               iter_used = iter_used, s = s, md = md, i = i)
        }

        iter_used <- iter_used + 1L
        if (all_done || iter_used >= max_iter) break
      }

      final_rows <- rbindlist(lapply(species_vec, function(sp) {
        state <- states_by_sp[[sp]]
        row <- if (!is.null(state$best_row)) copy(state$best_row) else extract_species_row(last_result, sp, md, s, i)
        row[, `:=`(
          keep_to_rel = as.integer(state$direction == "keep_to_rel"),
          rel_to_keep = as.integer(state$direction == "rel_to_keep"),
          p_rel_to_keep = if (state$direction == "rel_to_keep") state$best_p else 0,
          p_keep_to_rel = if (state$direction == "keep_to_rel") state$best_p else 0,
          convergence = as.integer(is_achieved(diff_keep, pct_diff_keep, MRIP_keep)),
          iter_used = iter_used
        )]
        row
      }), use.names = TRUE, fill = TRUE)

      final_rows[, `:=`(
        n_sub_kept_striper = n_sub_striper_kept,
        prop_sub_kept_striper = prop_sub_striper_kept,
        n_legal_rel_striper = n_legal_striper_rel,
        prop_legal_rel_striper = prop_legal_striper_rel,
        original_rel_eligible_striper = original_rel_eligible_striper,
        n_sub_kept_bluefish = n_sub_bluefish_kept,
        prop_sub_kept_bluefish = prop_sub_bluefish_kept,
        n_legal_rel_bluefish = n_legal_bluefish_rel,
        prop_legal_rel_bluefish = prop_legal_bluefish_rel,
        original_rel_eligible_bluefish = original_rel_eligible_bluefish
      )]

      # Collapse species-specific diagnostics into a common column for easier downstream use.
      final_rows[species == "striper", `:=`(
        n_sub_kept = n_sub_kept_striper,
        prop_sub_kept = prop_sub_kept_striper,
        n_legal_rel = n_legal_rel_striper,
        prop_legal_rel = prop_legal_rel_striper,
        original_rel_eligible_final = original_rel_eligible_striper
      )]
      final_rows[species == "bluefish", `:=`(
        n_sub_kept = n_sub_kept_bluefish,
        prop_sub_kept = prop_sub_kept_bluefish,
        n_legal_rel = n_legal_rel_bluefish,
        prop_legal_rel = prop_legal_rel_bluefish,
        original_rel_eligible_final = original_rel_eligible_bluefish
      )]

      front_cols <- c("region", "mode", "draw", "species",
                      "MRIP_rel", "model_rel", "diff_rel", "pct_diff_rel",
                      "MRIP_keep", "model_keep", "diff_keep", "pct_diff_keep",
                      "MRIP_catch", "model_catch", "diff_catch", "pct_diff_catch",
                      "keep_to_rel", "rel_to_keep", "p_rel_to_keep", "p_keep_to_rel",
                      "convergence", "iter_used",
                      "n_sub_kept", "prop_sub_kept", "n_legal_rel", "prop_legal_rel")
      data.table::setcolorder(final_rows, c(intersect(front_cols, names(final_rows)),
                                            setdiff(names(final_rows), front_cols)))

      calibrated[[k]] <- final_rows
      k <- k + 1L

      if (!is.null(last_monthly_result) && nrow(last_monthly_result)) {
        calibrated_monthly[[km]] <- last_monthly_result
        km <- km + 1L
      }

    }
  }
}



calibrated_combined <- rbindlist(calibrated, use.names = TRUE, fill = TRUE)
calibrated_combined <- calibrated_combined[!is.na(mode)]
calibrated_combined <- unique(calibrated_combined, by = c("region", "mode", "draw", "species"))

front_cols <- c("region", "mode", "draw", "species",
                "MRIP_rel", "model_rel", "diff_rel", "pct_diff_rel",
                "MRIP_keep", "model_keep", "diff_keep", "pct_diff_keep",
                "MRIP_catch", "model_catch", "diff_catch", "pct_diff_catch",
                "keep_to_rel", "rel_to_keep", "p_rel_to_keep", "p_keep_to_rel",
                "convergence", "iter_used")
data.table::setcolorder(calibrated_combined, c(intersect(front_cols, names(calibrated_combined)),
                                               setdiff(names(calibrated_combined), front_cols)))

# fst::write_fst(calibrated_combined,
#                file.path(final_process_misc_cd, "calibrated_model_stats26to50_7_6_26.fst"))
#
# data.table::fwrite(calibrated_combined,
#                    file.path(final_process_misc_cd, "calibrated_model_stats26to50_7_6_26.csv"))
#
# calibrated_monthly_combined <- data.table::rbindlist(
#   calibrated_monthly,
#   use.names = TRUE,
#   fill = TRUE
# )
#
# calibrated_monthly_combined <- calibrated_monthly_combined[!is.na(mode)]
#
# fst::write_fst(
#   calibrated_monthly_combined,
#   file.path(final_process_misc_cd, "calibrated_model_stats_monthly26to50_7_6_26.fst")
# )
#
# data.table::fwrite(
#   calibrated_monthly_combined,
#   file.path(final_process_misc_cd, "calibrated_model_stats_monthly26to50_7_6_26.csv")
# )


fst::write_fst(calibrated_combined,
               file.path(final_process_misc_cd, "calibrated_model_stats_7_9_26.fst"))

data.table::fwrite(calibrated_combined,
                   file.path(final_process_misc_cd, "calibrated_model_stats_7_9_26.csv"))

calibrated_monthly_combined <- data.table::rbindlist(
  calibrated_monthly,
  use.names = TRUE,
  fill = TRUE
)

calibrated_monthly_combined <- calibrated_monthly_combined[!is.na(mode)]

fst::write_fst(
  calibrated_monthly_combined,
  file.path(final_process_misc_cd, "calibrated_model_stats_monthly_7_9_26.fst")
)

data.table::fwrite(
  calibrated_monthly_combined,
  file.path(final_process_misc_cd, "calibrated_model_stats_monthly_7_9_26.csv")
)






# monthly_1_25 <- fst::read_fst(
#   file.path(final_process_misc_cd, "calibrated_model_stats_monthly5_7_6_26.fst"),
#   as.data.table = TRUE
# )
#
# monthly_26_50 <- fst::read_fst(
#   file.path(final_process_misc_cd, "calibrated_model_stats_monthly26to50_7_6_26.fst"),
#   as.data.table = TRUE
# )
#
# calibrated_monthly_combined_all <- data.table::rbindlist(
#   list(monthly_1_25, monthly_26_50),
#   use.names = TRUE,
#   fill = TRUE
# )
#
# calibrated_monthly_combined_all <- calibrated_monthly_combined_all[!is.na(mode)]
#
# calib_1_25 <- fst::read_fst(
#   file.path(final_process_misc_cd, "calibrated_model_stats25_7_6_26.fst"),
#   as.data.table = TRUE
# )
#
# calib_26_50 <- fst::read_fst(
#   file.path(final_process_misc_cd, "calibrated_model_stats26to50_7_6_26.fst"),
#   as.data.table = TRUE
# )
#
# calibrated_combined_all <- data.table::rbindlist(
#   list(calib_1_25, calib_26_50),
#   use.names = TRUE,
#   fill = TRUE
# )
#
# fst::write_fst(
#   calibrated_monthly_combined_all,
#   file.path(final_process_misc_cd, "calibrated_model_stats_monthly1to50_7_6_26.fst")
# )
#
# data.table::fwrite(
#   calibrated_monthly_combined_all,
#   file.path(final_process_misc_cd, "calibrated_model_stats_monthly1to50_7_6_26.csv")
# )
#
# fst::write_fst(
#   calibrated_combined_all,
#   file.path(final_process_misc_cd, "calibrated_model_stats1to50_7_6_26.fst")
# )
#
# data.table::fwrite(
#   calibrated_combined_all,
#   file.path(final_process_misc_cd, "calibrated_model_stats1to50_7_6_26.csv")
# )
