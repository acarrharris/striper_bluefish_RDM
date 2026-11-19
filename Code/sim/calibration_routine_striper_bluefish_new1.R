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

# ------------------------------------------------------------------
# Memory settings
# ------------------------------------------------------------------

# Prevent data.table from making large parallel temporary allocations
# during grouping/sorting operations.
data.table::setDTthreads(1L)

# Disable memory-intensive GForce grouped aggregation optimization.
# This is slower but safer for very large grouped tables.
options(datatable.optimize = 1L)

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
  fst::read_fst(file.path(final_process_misc_cd, "calibration_comparison.fst"))
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

is_achieved <- function(diff_keep,
                        pct_diff_keep,
                        MRIP_keep = NA_real_,
                        model_keep = NA_real_) {

  # When MRIP harvest is zero, require modeled harvest to be effectively zero.
  # Do not use the ordinary 500-fish tolerance.
  if (is.finite(MRIP_keep) && MRIP_keep == 0) {

    if (is.finite(model_keep)) {
      return(abs(model_keep) < 0.5)
    }

    return(is.finite(diff_keep) && abs(diff_keep) < 0.5)
  }

  (is.finite(diff_keep) &&
      abs(diff_keep) < tol_abs_fish) ||
    (is.finite(pct_diff_keep) &&
       abs(pct_diff_keep) < tol_abs_pct)
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
      no_realloc_needed = TRUE,
      force_all_keep_to_rel = FALSE
    ))
  }

  # If MRIP harvest is zero but the simulation has harvest, force every
  # simulated kept fish to release. This is not a probability search.
  if (is.finite(mrip_keep) &&
      mrip_keep == 0 &&
      is.finite(model_keep) &&
      model_keep > 0) {

    return(list(
      direction = "keep_to_rel",
      p = 1,
      lo = 1,
      hi = 1,
      achieved = FALSE,
      convergence = 1L,
      best_score = Inf,
      best_row = NULL,
      frozen = FALSE,
      best_p = 1,
      no_realloc_needed = FALSE,
      force_all_keep_to_rel = TRUE
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
      no_realloc_needed = TRUE,
      force_all_keep_to_rel = FALSE
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
    no_realloc_needed = FALSE,
    force_all_keep_to_rel = FALSE
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

  # Zero-MRIP harvest case: p = 1 is mandatory. Evaluate it once and retain it.
  if (isTRUE(state$force_all_keep_to_rel)) {

    state$p <- 1
    state$best_p <- 1
    state$best_row <- data.table::copy(row)

    state$achieved <- is_achieved(
      diff_keep = diff_keep,
      pct_diff_keep = pct_diff_keep,
      MRIP_keep = MRIP_keep,
      model_keep = as.numeric(row$model_keep)
    )

    state$best_score <- if (state$achieved) -Inf else abs(row$model_keep)
    state$convergence <- as.integer(state$achieved)
    state$frozen <- TRUE

    return(state)
  }
  state$achieved <- is_achieved(
    diff_keep = diff_keep,
    pct_diff_keep = pct_diff_keep,
    MRIP_keep = MRIP_keep,
    model_keep = as.numeric(row$model_keep)
  )
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

    next_p <- (state$lo + state$hi) / 2

  } else {

    next_p <- if (old_p == 0) {
      0.1
    } else {
      min(1, max(old_p * 1.5, old_p + 0.05))
    }
  }

  next_p <- max(0, min(1, next_p))

  # Only declare failure when the probability that was just evaluated was
  # already at the endpoint and still did not achieve the target.
  evaluated_upper_endpoint <- (
    is.na(state$hi) &&
      old_p >= 1 - p_tol
  )

  evaluated_lower_endpoint <- (
    !is.na(state$hi) &&
      abs(state$hi - state$lo) < p_tol
  )

  if (!state$achieved &&
      (evaluated_upper_endpoint || evaluated_lower_endpoint)) {

    state$convergence <- 0L
    state$frozen <- TRUE

  } else {

    state$p <- next_p
  }

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

make_cell_seed <- function(region, mode, draw) {

  region_num <- sum(
    utf8ToInt(as.character(region)) *
      seq_along(utf8ToInt(as.character(region)))
  )

  mode_num <- sum(
    utf8ToInt(as.character(mode)) *
      seq_along(utf8ToInt(as.character(mode)))
  )

  as.integer(
    100000L +
      region_num * 100L +
      mode_num * 10L +
      as.integer(draw)
  )
}

# ----------------------------
# Checkpoint and combined-output settings
# ----------------------------

# Each completed region x mode x draw cell is saved here. The cell-level
# calibration file is written last and serves as the completion marker.
calibration_piece_dir <- file.path(
  final_process_misc_cd,
  "calibrated_model_stats_piecewise"
)

calibration_monthly_piece_dir <- file.path(
  final_process_misc_cd,
  "calibrated_model_stats_monthly_piecewise"
)

dir.create(calibration_piece_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(calibration_monthly_piece_dir, recursive = TRUE, showWarnings = FALSE)

# Set TRUE only when you intentionally want to rerun and overwrite cells that
# already have a completed checkpoint.
overwrite_completed_cells <- FALSE

# Change this single tag when you want a separately named combined output.
output_tag <- "8_10_26"

sanitize_file_component <- function(x) {
  gsub("[^A-Za-z0-9_-]", "_", as.character(x))
}

make_piece_path <- function(region, mode, draw, monthly = FALSE) {
  output_dir <- if (monthly) {
    calibration_monthly_piece_dir
  } else {
    calibration_piece_dir
  }

  file.path(
    output_dir,
    sprintf(
      "calibrated_model_stats%s_%s_%s_draw_%03d.fst",
      if (monthly) "_monthly" else "",
      sanitize_file_component(region),
      sanitize_file_component(mode),
      as.integer(draw)
    )
  )
}

# Write to a temporary file and rename it only after fst::write_fst succeeds.
# This prevents an interrupted write from leaving a partial file that would be
# mistaken for a completed calibration cell on restart.
write_fst_atomic <- function(x, path) {
  tmp_path <- paste0(path, ".tmp_", Sys.getpid())
  on.exit(unlink(tmp_path), add = TRUE)

  fst::write_fst(as.data.frame(x), tmp_path)

  if (file.exists(path)) {
    unlink(path)
  }

  if (!file.rename(tmp_path, path)) {
    stop("Could not rename temporary checkpoint file to: ", path, call. = FALSE)
  }

  invisible(path)
}

read_piece_files <- function(directory) {
  files <- list.files(
    directory,
    pattern = "\\.fst$",
    full.names = TRUE
  )

  if (length(files) == 0L) {
    return(data.table())
  }

  data.table::rbindlist(
    lapply(files, fst::read_fst, as.data.table = TRUE),
    use.names = TRUE,
    fill = TRUE
  )
}

run_catch1 <- function(parent_env) {

  # Objects required by catch1 are found through the parent environment, but
  # all large objects created by catch1 remain inside this disposable environment.
  run_env <- new.env(
    parent = parent_env,
    hash = TRUE
  )

  control_names <- unlist(
    lapply(
      c("striper", "bluefish"),
      function(sp) {
        c(
          paste0("keep_to_rel_", sp),
          paste0("rel_to_keep_", sp),
          paste0("p_rel_to_keep_", sp),
          paste0("p_keep_to_rel_", sp),
          paste0("all_keep_to_rel_", sp)
        )
      }
    ),
    use.names = FALSE
  )

  for (nm in control_names) {
    assign(
      nm,
      get(nm, envir = parent_env),
      envir = run_env
    )
  }

  toggle_names <- c(
    "save_calibration_outputs",
    "compute_monthly_output",
    "compute_weight_output"
  )

  for (nm in toggle_names) {
    if (exists(nm, envir = parent_env, inherits = TRUE)) {
      assign(
        nm,
        get(nm, envir = parent_env, inherits = TRUE),
        envir = run_env
      )
    }
  }

  preloaded_names <- c(
    "dtripz_current",
    "catch_draw_current",
    "angler_draw_current"
  )

  for (nm in preloaded_names) {
    if (exists(nm, envir = parent_env, inherits = TRUE)) {
      assign(
        nm,
        get(nm, envir = parent_env, inherits = TRUE),
        envir = run_env
      )
    }
  }

  source(
    file.path(
      get("code_cd", envir = parent_env),
      "calibrate_rec_catch1_striper_bluefish_new.R"
    ),
    local = run_env
  )

  # Return only the objects needed by the outer calibration routine.
  out <- list(
    calib_comparison1 = data.table::copy(
      get("calib_comparison1", envir = run_env)
    ),

    calib_monthly_stats1 = if (
      exists("calib_monthly_stats1", envir = run_env, inherits = FALSE)
    ) {
      data.table::copy(
        get("calib_monthly_stats1", envir = run_env)
      )
    } else {
      NULL
    }
  )

  # Retain the small diagnostic scalars needed after the final run.
  diagnostic_names <- c(
    "n_sub_striper_kept",
    "prop_sub_striper_kept",
    "n_legal_striper_rel",
    "prop_legal_striper_rel",
    "original_rel_eligible_striper",
    "n_sub_bluefish_kept",
    "prop_sub_bluefish_kept",
    "n_legal_bluefish_rel",
    "prop_legal_bluefish_rel",
    "original_rel_eligible_bluefish"
  )

  out$diagnostics <- stats::setNames(
    lapply(diagnostic_names, function(nm) {
      if (exists(nm, envir = run_env, inherits = FALSE)) {
        get(nm, envir = run_env)
      } else {
        NA_real_
      }
    }),
    diagnostic_names
  )

  # Removing the environment makes all large intermediate objects eligible
  # for garbage collection as soon as this function returns.
  rm(run_env)

  out
}

# ----------------------------
# Main loop
# ----------------------------

# Current run configuration. Edit these vectors to run a different subset.
#region_draw <- c("CTALL", "DEDERIV", "DENANT", "DEOCN", "MAALL", "MDCHES", "MDOCN", "MDPOT", "MEALL",
#                 "NCALB", "NCCNTRL", "NCOCN", "NHALL", "NJALL", "NYHUDN", "NYHUDS", "RIALL", "VACHES",
#                 "VAOCN", "VAPOTO")
#mode_draw <- if (exists("mode_draw")) mode_draw else c("pr", "fh", "sh")
#region_draw <- c("CTALL")
#draws <- 1:101
draws <- 76:101

for (s in region_draw) {

  # ================================================================
  # REGION-LEVEL INPUT
  # Read directed-trip/regulation data once per region.
  # ================================================================

  dtrip_region_path <- file.path(
    iterative_input_data_cd,
    paste0("directed_trip_draws_", s, ".fst")
  )

  if (!file.exists(dtrip_region_path)) {
    warning(
      "Missing directed-trip file; skipping region: ",
      s
    )
    next
  }

  dtrip_region_all <- data.table::as.data.table(
    fst::read_fst(dtrip_region_path)
  )

  if (!"date_parsed" %in% names(dtrip_region_all)) {
    dtrip_region_all[
      ,
      date_parsed := parse_date_any(date)
    ]
  }

  # Retain only draws and modes that will be processed.
  dtrip_region_all <- dtrip_region_all[
    draw %in% draws & mode %in% mode_draw
  ]

  for (i in draws) {

    # ==============================================================
    # Determine which modes still need to be run for this region/draw
    # ==============================================================

    pending_modes <- mode_draw[
      !vapply(
        mode_draw,
        function(md_check) {
          file.exists(
            make_piece_path(
              region = s,
              mode = md_check,
              draw = i,
              monthly = FALSE
            )
          )
        },
        logical(1)
      ) |
        isTRUE(overwrite_completed_cells)
    ]

    if (length(pending_modes) == 0L) {

      cat(
        "\nAll modes already completed:",
        "region =", s,
        "| draw =", i,
        "\n"
      )

      next
    }

    # ==============================================================
    # REGION x DRAW INPUTS
    # Read catch and angler files once for this region/draw.
    # ==============================================================

    catch_draw_path <- file.path(
      iterative_input_data_cd,
      paste0(
        "calib_catch_draws_processed_",
        s,
        "_",
        i,
        ".fst"
      )
    )

    angler_draw_path <- file.path(
      iterative_input_data_cd,
      paste0(
        "pref_params_and_dems_",
        s,
        "_",
        i,
        ".fst"
      )
    )

    if (!file.exists(catch_draw_path)) {

      warning(
        "Missing catch-draw file; skipping region/draw: ",
        s,
        " / ",
        i
      )

      next
    }

    if (!file.exists(angler_draw_path)) {

      warning(
        "Missing angler file; skipping region/draw: ",
        s,
        " / ",
        i
      )

      next
    }

    catch_draw_all <- data.table::as.data.table(
      fst::read_fst(catch_draw_path)
    )

    if (!"date_parsed" %in% names(catch_draw_all)) {
      catch_draw_all[
        ,
        date_parsed := parse_date_any(date)
      ]
    }

    angler_draw_all <- data.table::as.data.table(
      fst::read_fst(angler_draw_path)
    )

    if (!"date_parsed" %in% names(angler_draw_all)) {
      angler_draw_all[
        ,
        date_parsed := parse_date_any(date)
      ]
    }

    # ==============================================================
    # Retain only columns used by the catch simulation
    # ==============================================================

    required_catch_cols <- c(
      "date_parsed",
      "mode",
      "tripid",
      "catch_draw",
      "str_cat",
      "blu_cat"
    )

    missing_cols <- setdiff(
      required_catch_cols,
      names(catch_draw_all)
    )

    if (length(missing_cols)) {
      stop(
        "catch_draw_all is missing columns: ",
        paste(missing_cols, collapse = ", ")
      )
    }

    catch_draw_all <- catch_draw_all[
      mode %in% pending_modes,
      .(date_parsed,
        mode,
        tripid,
        catch_draw,
        str_cat,
        blu_cat)
    ]

    required_angler_cols <- c(
      "date_parsed",
      "mode",
      "tripid",
      "cost_sim",

      "beta_str_kp",
      "beta_str_kp_L",
      "beta_str_kp_L2",
      "beta_str_rel",
      "beta_str_rel_L",
      "beta_blu_keep",
      "beta_blu_rel",
      "beta_cost",

      "beta_optout",
      "beta_optout_male",

      "beta_optoth",
      "beta_optoth_days_non_str",
      "beta_optoth_likely",

      "male",
      "days_non_str_blue12",
      "likely_to_fish"
    )

    missing_cols <- setdiff(
      required_angler_cols,
      names(angler_draw_all)
    )

    if (length(missing_cols)) {
      stop(
        "angler_draw_all is missing columns: ",
        paste(missing_cols, collapse = ", ")
      )
    }

    angler_draw_all <- angler_draw_all[
      mode %in% pending_modes,
      .(date_parsed,
        mode,
        tripid,
        cost_sim,

        beta_str_kp,
        beta_str_kp_L,
        beta_str_kp_L2,
        beta_str_rel,
        beta_str_rel_L,
        beta_blu_keep,
        beta_blu_rel,
        beta_cost,

        beta_optout,
        beta_optout_male,

        beta_optoth,
        beta_optoth_days_non_str,
        beta_optoth_likely,

        male,
        days_non_str_blue12,
        likely_to_fish)
    ]

    # ==============================================================
    # Split each region/draw input once by mode
    # ==============================================================

    catch_by_mode <- split(
      catch_draw_all,
      by = "mode",
      keep.by = TRUE,
      drop = FALSE
    )

    angler_by_mode <- split(
      angler_draw_all,
      by = "mode",
      keep.by = TRUE,
      drop = FALSE
    )

    dtrip_draw_all <- dtrip_region_all[
      draw == i & mode %in% pending_modes,
      .(
        mode,
        date_parsed,
        striper_bag,
        striper_min,
        striper_max,
        bluefish_bag,
        bluefish_min,
        dtrip
      )
    ]

    dtrip_by_mode <- split(
      dtrip_draw_all,
      by = "mode",
      keep.by = TRUE,
      drop = FALSE
    )

    # ==============================================================
    # MODE LOOP
    # ==============================================================

    for (md in pending_modes) {

      piece_file <- make_piece_path(
        region = s,
        mode = md,
        draw = i,
        monthly = FALSE
      )

      monthly_piece_file <- make_piece_path(
        region = s,
        mode = md,
        draw = i,
        monthly = TRUE
      )

      # This check is retained defensively, although pending_modes should
      # already exclude completed cells.
      if (!overwrite_completed_cells &&
          file.exists(piece_file)) {

        cat(
          "\nSkipping completed calibration:",
          "region =", s,
          "| mode =", md,
          "| draw =", i,
          "\n"
        )

        next
      }

      baseline_targets_current <- baseline_output0[
        region == s &
          draw == i &
          mode == md
      ]

      # ------------------------------------------------------------
      # Create mode-specific input objects expected by catch1
      # ------------------------------------------------------------

      catch_draw_current <- catch_by_mode[[md]]
      angler_draw_current <- angler_by_mode[[md]]
      dtripz_current <- dtrip_by_mode[[md]]

      if (is.null(catch_draw_current)) {
        catch_draw_current <- data.table::data.table()
      }

      if (is.null(angler_draw_current)) {
        angler_draw_current <- data.table::data.table()
      }

      if (is.null(dtripz_current)) {
        dtripz_current <- data.table::data.table()
      }

      # ------------------------------------------------------------
      # Validate this mode's inputs
      # ------------------------------------------------------------

      if (!nrow(catch_draw_current)) {

        cat(
          "\nNo catch-draw rows; skipping:",
          "region =", s,
          "| mode =", md,
          "| draw =", i,
          "\n"
        )

        rm(
          catch_draw_current,
          angler_draw_current,
          dtripz_current
        )

        next
      }

      if (!nrow(angler_draw_current)) {

        cat(
          "\nNo angler rows; skipping:",
          "region =", s,
          "| mode =", md,
          "| draw =", i,
          "\n"
        )

        rm(
          catch_draw_current,
          angler_draw_current,
          dtripz_current
        )

        next
      }

      if (!nrow(dtripz_current) ||
          sum(dtripz_current$dtrip, na.rm = TRUE) == 0) {

        cat(
          "\nNo directed trips; skipping:",
          "region =", s,
          "| mode =", md,
          "| draw =", i,
          "\n"
        )

        rm(
          catch_draw_current,
          angler_draw_current,
          dtripz_current
        )

        next
      }
      if (nrow(baseline_targets_current) == 0L) {

        cat(
          "\nNo baseline target rows found; skipping:",
          "region =", s,
          "| mode =", md,
          "| draw =", i,
          "\n"
        )

        rm(
          catch_draw_current,
          angler_draw_current,
          dtripz_current
        )

        gc(verbose = FALSE)

        next
      }

      # Preserve missing-target cells as completed no-calibration outputs.
      if (all(is.na(baseline_targets_current$MRIP_keep))) {

        final_rows <- data.table::copy(
          baseline_targets_current
        )

        final_rows[, `:=`(
          keep_to_rel = 0L,
          rel_to_keep = 0L,
          p_rel_to_keep = 0,
          p_keep_to_rel = 0,
          convergence = NA_integer_,
          iter_used = 0L
        )]

        write_fst_atomic(
          final_rows,
          piece_file
        )

        cat(
          "\nSaved missing-target calibration checkpoint:",
          "region =", s,
          "| mode =", md,
          "| draw =", i,
          "\n"
        )

        rm(
          final_rows,
          catch_draw_current,
          angler_draw_current,
          dtripz_current
        )

        gc(verbose = FALSE)

        next
      }

      states_by_sp <- setNames(vector("list", length(species_vec)), species_vec)

      cell_seed <- make_cell_seed(
        region = s,
        mode = md,
        draw = i)


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
          diff_keep = base_row$diff_keep,
          pct_diff_keep = base_row$pct_diff_keep,
          MRIP_keep = base_row$MRIP_keep,
          model_keep = base_row$model_keep
        )
      }

      iter_used <- 0L
      last_result <- NULL

      save_calibration_outputs <- FALSE
      compute_monthly_output <- FALSE
      compute_weight_output <- FALSE

      repeat {
        push_globals(
          states_by_sp,
          target_env = environment()
        )

        # Use identical catch-at-length draws and reallocation rankings for every
        # probability evaluated in this region-mode-draw cell.
        set.seed(cell_seed)

        catch1_out <- run_catch1(
          parent_env = environment()
        )

        last_result <- catch1_out$calib_comparison1

        rm(catch1_out)
        gc(verbose = FALSE)

        all_done <- TRUE

        current_rows <- setNames(
          vector("list", length(species_vec)),
          species_vec
        )

        for (sp in species_vec) {

          current_rows[[sp]] <- extract_species_row(
            last_result,
            sp,
            md,
            s,
            i
          )

          states_by_sp[[sp]] <- update_bracket(
            states_by_sp[[sp]],
            current_rows[[sp]]
          )

          if (!states_by_sp[[sp]]$achieved &&
              states_by_sp[[sp]]$convergence == 1L) {
            all_done <- FALSE
          }
        }

        for (sp in species_vec) {

          row <- current_rows[[sp]]
          state <- states_by_sp[[sp]]

          cat("\n--------------------------------------------------\n")
          cat(
            "Region:", s,
            "| Mode:", md,
            "| Draw:", i,
            "| Species:", sp,
            "| Iteration:", iter_used,
            "\n"
          )

          print(
            data.table::data.table(
              evaluated_p = if (state$direction == "rel_to_keep") {
                get(paste0("p_rel_to_keep_", sp), envir = environment())
              } else if (state$direction == "keep_to_rel") {
                get(paste0("p_keep_to_rel_", sp), envir = environment())
              } else {
                0
              },
              next_p = state$p,
              MRIP_keep = row$MRIP_keep,
              model_keep = row$model_keep,
              diff_keep = row$diff_keep,
              pct_diff_keep = row$pct_diff_keep,
              achieved = state$achieved,
              convergence = state$convergence
            )
          )

          cat("--------------------------------------------------\n")
        }

        iter_used <- iter_used + 1L

        if (all_done || iter_used >= max_iter) {
          break
        }
      }

      # Run once more using the selected probability for each species. This
      # ensures that the saved aggregate, monthly output, diagnostics, baseline
      # outcomes, and choice-occurrence files all correspond to the exact pair
      # of probabilities recorded in the final calibration rows.


      for (sp in species_vec) {
        states_by_sp[[sp]]$p <- states_by_sp[[sp]]$best_p
      }

      save_calibration_outputs <- TRUE
      compute_monthly_output <- TRUE
      compute_weight_output <- TRUE

      push_globals(
        states_by_sp,
        target_env = environment()
      )

      # Use identical catch-at-length draws and reallocation rankings for every
      # probability evaluated in this region-mode-draw cell.
      set.seed(cell_seed)

      final_catch1_out <- run_catch1(
        parent_env = environment()
      )

      final_result <- final_catch1_out$calib_comparison1
      final_monthly_result <- final_catch1_out$calib_monthly_stats1
      final_diagnostics <- final_catch1_out$diagnostics

      final_rows <- rbindlist(
        lapply(species_vec, function(sp) {
          state <- states_by_sp[[sp]]
          row <- extract_species_row(final_result, sp, md, s, i)

          row[, `:=`(
            keep_to_rel = as.integer(state$direction == "keep_to_rel"),
            rel_to_keep = as.integer(state$direction == "rel_to_keep"),
            p_rel_to_keep = if (state$direction == "rel_to_keep") state$best_p else 0,
            p_keep_to_rel = if (state$direction == "keep_to_rel") state$best_p else 0,
            convergence = as.integer(
              is_achieved(
                diff_keep = diff_keep,
                pct_diff_keep = pct_diff_keep,
                MRIP_keep = MRIP_keep,
                model_keep = model_keep
              )
            ),
            iter_used = iter_used
          )]

          row
        }),
        use.names = TRUE,
        fill = TRUE
      )

      # These objects are generated by the final catch1 run above, so they now
      # correspond to the selected joint striper/bluefish probabilities.
      final_rows[, `:=`(
        n_sub_kept_striper =
          final_diagnostics$n_sub_striper_kept,

        prop_sub_kept_striper =
          final_diagnostics$prop_sub_striper_kept,

        n_legal_rel_striper =
          final_diagnostics$n_legal_striper_rel,

        prop_legal_rel_striper =
          final_diagnostics$prop_legal_striper_rel,

        original_rel_eligible_striper =
          final_diagnostics$original_rel_eligible_striper,

        n_sub_kept_bluefish =
          final_diagnostics$n_sub_bluefish_kept,

        prop_sub_kept_bluefish =
          final_diagnostics$prop_sub_bluefish_kept,

        n_legal_rel_bluefish =
          final_diagnostics$n_legal_bluefish_rel,

        prop_legal_rel_bluefish =
          final_diagnostics$prop_legal_bluefish_rel,

        original_rel_eligible_bluefish =
          final_diagnostics$original_rel_eligible_bluefish
      )]

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

      front_cols <- c(
        "region", "mode", "draw", "species",
        "MRIP_rel", "model_rel", "diff_rel", "pct_diff_rel",
        "MRIP_keep", "model_keep", "diff_keep", "pct_diff_keep",
        "MRIP_catch", "model_catch", "diff_catch", "pct_diff_catch",
        "keep_to_rel", "rel_to_keep", "p_rel_to_keep", "p_keep_to_rel",
        "convergence", "iter_used",
        "n_sub_kept", "prop_sub_kept", "n_legal_rel", "prop_legal_rel"
      )

      data.table::setcolorder(
        final_rows,
        c(
          intersect(front_cols, names(final_rows)),
          setdiff(names(final_rows), front_cols)
        )
      )

      # Write monthly first. The main piece is the completion marker and must be
      # written only after all other outputs for the cell are safely on disk.
      if (!is.null(final_monthly_result) && nrow(final_monthly_result) > 0L) {
        write_fst_atomic(final_monthly_result, monthly_piece_file)
      } else if (file.exists(monthly_piece_file)) {
        unlink(monthly_piece_file)
      }

      write_fst_atomic(final_rows, piece_file)

      cat(
        "\nSaved completed calibration checkpoint:",
        "region =", s,
        "| mode =", md,
        "| draw =", i,
        "\n"
      )

      # --------------------------------------------------
      # Clean up large objects before moving to next
      # region x mode x draw cell
      # --------------------------------------------------

      # --------------------------------------------------
      # Clean up objects for this region x mode x draw
      # --------------------------------------------------

      rm(
        final_catch1_out,
        final_result,
        final_monthly_result,
        final_diagnostics,
        final_rows,
        states_by_sp,
        last_result,
        current_rows,
        dtripz_current,
        catch_draw_current,
        angler_draw_current
      )

      gc(verbose = FALSE)

    } # end mode loop

    # ==============================================================
    # Finished all modes for this region x draw
    # ==============================================================

    rm(
      catch_draw_all,
      angler_draw_all,
      catch_by_mode,
      angler_by_mode,
      dtrip_draw_all,
      dtrip_by_mode
    )

    gc(verbose = FALSE)

  } # end draw loop

  # ================================================================
  # Finished all draws for this region
  # ================================================================

  rm(dtrip_region_all)

  gc(verbose = FALSE)

} # end region loop

# ----------------------------
# Combine all saved checkpoints
# ----------------------------

calibrated_combined <- read_piece_files(calibration_piece_dir)

if (nrow(calibrated_combined) == 0L) {
  stop(
    "No completed calibration checkpoint files were found in: ",
    calibration_piece_dir,
    call. = FALSE
  )
}

calibrated_combined <- calibrated_combined[!is.na(mode)]

# Each checkpoint path is unique to a region x mode x draw cell and is
# overwritten atomically. Therefore, deduplicating the complete table is not
# necessary and would force data.table to allocate a potentially large sort
# workspace. If duplicate checkpoint files are ever introduced manually,
# diagnose them from the filenames rather than sorting the combined table.

front_cols <- c(
  "region", "mode", "draw", "species",
  "MRIP_rel", "model_rel", "diff_rel", "pct_diff_rel",
  "MRIP_keep", "model_keep", "diff_keep", "pct_diff_keep",
  "MRIP_catch", "model_catch", "diff_catch", "pct_diff_catch",
  "keep_to_rel", "rel_to_keep", "p_rel_to_keep", "p_keep_to_rel",
  "convergence", "iter_used"
)

data.table::setcolorder(
  calibrated_combined,
  c(
    intersect(front_cols, names(calibrated_combined)),
    setdiff(names(calibrated_combined), front_cols)
  )
)

# Sorting is cosmetic and can require substantial temporary memory. Leave the
# rows in checkpoint-file order; downstream code should not depend on row order.

combined_fst <- file.path(
  final_process_misc_cd,
  paste0("calibrated_model_stats_", output_tag, ".fst")
)

write_fst_atomic(calibrated_combined, combined_fst)

calibrated_monthly_combined <- read_piece_files(
  calibration_monthly_piece_dir
)

if (nrow(calibrated_monthly_combined) > 0L) {
  calibrated_monthly_combined <- calibrated_monthly_combined[!is.na(mode)]

  # Do not call unique(..., by=...) or setorderv() here. Both operations invoke
  # data.table's parallel ordering machinery and can require a large temporary
  # allocation. There is one atomically overwritten monthly checkpoint per
  # region x mode x draw cell, so combining the files with rbindlist() is
  # sufficient and preserves every monthly/statistic row within each cell.

  monthly_combined_fst <- file.path(
    final_process_misc_cd,
    paste0("calibrated_model_stats_monthly_", output_tag, ".fst")
  )


  write_fst_atomic(calibrated_monthly_combined, monthly_combined_fst)
} else {
  warning(
    "No monthly calibration checkpoint files were found in: ",
    calibration_monthly_piece_dir
  )
}

cat(
  "\nCalibration complete.\n",
  "Piecewise outputs: ", calibration_piece_dir, "\n",
  "Monthly piecewise outputs: ", calibration_monthly_piece_dir, "\n",
  "Combined output: ", combined_fst, "\n",
  sep = ""
)
