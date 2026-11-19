
# ---- packages ----
required_pkgs <- c(
  "survey", "copula", "MASS", "fitdistrplus", "readxl",
  "weights", "wCorr", "patchwork", "Hmisc", "tidyr",
  "dplyr", "ggplot2", "writexl", "plyr", "conflicted"
)

missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Install these packages before running the script: ",
       paste(missing_pkgs, collapse = ", "))
}

library(patchwork)
library(survey)
library(copula)
library(MASS)
library(fitdistrplus)
library(readxl)
library(Hmisc)
library(weights)
library(wCorr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(writexl)
library(plyr)
library(conflicted)
library(haven)

conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::summarise)

set.seed(3211990)

# ---- controls ----
n_sim   <- 20000
n_draws <- 101
n_reps  <- 200

statez<-c("ME", "NH", "MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")

input_file <- "E:/Lou_projects/striper_bluefish_RDM/input_data/baseline_mrip_catch_processed.xlsx"

full_df <- readxl::read_xlsx(input_file)

output_dir <- "E:/Lou_projects/striper_bluefish_RDM/process_data/"

# ---- helper functions ----

get_rep_stat <- function(formula, rep_design) {
  out <- survey::svymean(formula, rep_design, return.replicates = TRUE, na.rm = TRUE)

  reps <- attr(out, "replicates")

  if (is.null(reps)) {
    reps <- numeric(0)
  } else {
    reps <- as.numeric(reps)
  }

  reps <- reps[is.finite(reps)]

  list(
    mean = as.numeric(coef(out)),
    reps = reps
  )
}

get_rep_var <- function(df, varname, rep_design) {
  rep_wgts <- stats::weights(rep_design, type = "analysis")

  if (is.null(rep_wgts)) {
    return(numeric(0))
  }

  if (is.vector(rep_wgts)) {
    rep_wgts <- matrix(rep_wgts, ncol = 1)
  }

  out <- sapply(seq_len(ncol(rep_wgts)), function(i) {
    df_rep <- df
    df_rep$rep_wgt_tmp <- rep_wgts[, i]

    if (all(is.na(df_rep$rep_wgt_tmp)) || sum(df_rep$rep_wgt_tmp, na.rm = TRUE) <= 0) {
      return(NA_real_)
    }

    svy_rep <- survey::svydesign(
      ids = ~psu_id,
      strata = ~strat_id,
      weights = ~rep_wgt_tmp,
      nest = TRUE,
      data = df_rep
    )

    val <- try(
      as.numeric(coef(
        survey::svyvar(
          stats::as.formula(paste0("~", varname)),
          svy_rep,
          na.rm = TRUE
        )
      )),
      silent = TRUE
    )

    if (inherits(val, "try-error")) {
      return(NA_real_)
    }

    val
  })

  out <- as.numeric(out)
  out <- out[is.finite(out)]

  out
}

safe_theta <- function(mu, var, theta_cap = 1000) {
  mu  <- as.numeric(mu)
  var <- as.numeric(var)

  mu  <- mu[is.finite(mu)]
  var <- var[is.finite(var)]

  if (length(mu) == 0 || length(var) == 0) {
    return(theta_cap)
  }

  len <- min(length(mu), length(var))
  mu  <- pmax(mu[seq_len(len)],  1e-8)
  var <- pmax(var[seq_len(len)], 1e-8)

  theta <- mu^2 / pmax(var - mu, 1e-6)
  theta <- pmax(theta, 1e-6)
  theta <- pmin(theta, theta_cap)
  theta[!is.finite(theta)] <- theta_cap

  if (length(theta) == 0) theta <- theta_cap

  theta
}

weighted_sample_for_copula <- function(df, value_cols, n_sim, weight_col = "wp_int", jitter_ties = FALSE) {
  if (is.null(df) || nrow(df) == 0) {
    stop("weighted_sample_for_copula: df has zero rows.")
  }

  w <- df[[weight_col]]

  if (is.null(w)) {
    stop("weighted_sample_for_copula: weight column not found: ", weight_col)
  }

  w[is.na(w) | !is.finite(w) | w < 0] <- 0

  if (length(w) != nrow(df)) {
    stop("weighted_sample_for_copula: weight vector length does not match nrow(df).")
  }

  if (sum(w) <= 0) {
    stop("weighted_sample_for_copula: all sampling weights are zero or missing.")
  }

  idx <- sample.int(
    n = nrow(df),
    size = n_sim,
    replace = TRUE,
    prob = w
  )

  out <- df[idx, , drop = FALSE]

  if (jitter_ties) {
    for (v in value_cols) {
      out[[v]] <- out[[v]] + stats::runif(nrow(out), -1e-8, 1e-8)
    }
  }

  out
}

cap_with_resample <- function(x, max_x) {
  x <- round(x)
  x[x < 0] <- 0

  if (all(x <= max_x)) return(x)

  x_cap <- pmin(x, max_x)
  excess <- sum(x - x_cap)

  while (excess > 0) {
    candidates <- which(x_cap < max_x)
    if (length(candidates) == 0) break

    room <- max_x - x_cap[candidates]
    probs <- room / sum(room)

    chosen_pos <- sample.int(
      n = length(candidates),
      size = 1,
      replace = TRUE,
      prob = probs
    )

    chosen <- candidates[chosen_pos]
    x_cap[chosen] <- x_cap[chosen] + 1
    excess <- excess - 1
  }

  x_cap
}

n_psu <- function(df) {
  dplyr::n_distinct(df$psu_id)
}

sample_positive_mean <- function(mu, var_mu, min_mu = 1e-8) {

  mu     <- as.numeric(mu)[1]
  var_mu <- as.numeric(var_mu)[1]

  if (!is.finite(mu) || mu <= 0) {
    return(min_mu)
  }

  if (!is.finite(var_mu) || var_mu <= 0) {
    return(mu)
  }

  sigma2_log <- log1p(var_mu / mu^2)
  meanlog     <- log(mu) - 0.5 * sigma2_log
  sdlog       <- sqrt(sigma2_log)

  sampled_mu <- stats::rlnorm(
    n = 1,
    meanlog = meanlog,
    sdlog = sdlog
  )

  max(sampled_mu, min_mu)
}

get_svy_mean_var <- function(varname, svy_design) {
  f <- stats::as.formula(paste0("~", varname))
  mean_obj <- survey::svymean(f, svy_design, na.rm = TRUE)
  var_obj <- survey::svyvar(f, svy_design, na.rm = TRUE)
  list(mu = as.numeric(stats::coef(mean_obj))[1], var_y = as.numeric(stats::coef(var_obj))[1], var_mu = as.numeric(stats::vcov(mean_obj)[1, 1]))
}


run_species_copula_sim <- function(full_df,
                                   species_prefix,
                                   statez,
                                   output_dir,
                                   n_sim = n_sim,
                                   n_draws = n_draws,
                                   n_reps = 200,
                                   keep_cap_buffer = 0,
                                   rel_cap_buffer = 0,
                                   verbose = TRUE) {

  keep_var   <- paste0(species_prefix, "_keep")
  rel_var    <- paste0(species_prefix, "_rel")
  se_keep    <- paste0("se", species_prefix, "_keep")
  se_rel     <- paste0("se", species_prefix, "_rel")

  flag_only_keep        <- paste0(species_prefix, "_only_keep")
  flag_only_rel         <- paste0(species_prefix, "_only_rel")
  flag_keep_and_rel     <- paste0(species_prefix, "_keep_and_rel")
  flag_no_catch         <- paste0(species_prefix, "_no_catch")
  flag_keep_and_rel_ind <- paste0(species_prefix, "_keep_and_rel_ind")

  needed_cols <- c(
    "state", "my_dom_id_string", "psu_id", "strat_id", "wp_int",
    keep_var, rel_var, se_keep, se_rel,
    flag_only_keep, flag_only_rel, flag_keep_and_rel,
    flag_no_catch, flag_keep_and_rel_ind
  )

  missing_cols <- setdiff(needed_cols, names(full_df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns for species ", species_prefix, ": ",
         paste(missing_cols, collapse = ", "))
  }

  simulate_one_margin <- function(df, varname, se_varname, dom, out_prefix, fixed_zero_side = c("rel", "keep")) {
    fixed_zero_side <- match.arg(fixed_zero_side)

    svy_design <- survey::svydesign(
      ids = ~psu_id,
      strata = ~strat_id,
      weights = ~wp_int,
      nest = TRUE,
      data = df
    )
    options(survey.lonely.psu = "certainty")

    mom <- get_svy_mean_var(varname, svy_design)
    mu <- mom$mu
    var_y <- mom$var_y
    var_mu <- mom$var_mu

    if (!is.finite(var_mu) || var_mu <= 0) var_mu <- mean(df[[se_varname]], na.rm = TRUE)^2

    max_val <- round(max(df[[varname]], na.rm = TRUE)) + ifelse(grepl("_keep$", varname), keep_cap_buffer, rel_cap_buffer)

    rep_design <- survey::as.svrepdesign(
      svy_design,
      type = "bootstrap",
      replicates = n_reps
    )

    rep_obj <- get_rep_stat(stats::as.formula(paste0("~", varname)), rep_design)
    rep_means <- rep_obj$reps
    rep_vars  <- get_rep_var(df, varname, rep_design)
    rep_theta <- safe_theta(rep_means, rep_vars)
    theta_hat_single <- safe_theta(mu, var_y)

    rep_theta_use <- rep_theta[is.finite(rep_theta)]
    if (length(rep_theta_use) == 0) rep_theta_use <- theta_hat_single

    sim_datasets <- vector("list", n_draws)
    i <- 1

    if (verbose) {
      message(
        "Species=", species_prefix,
        " | Domain=", dom,
        " | Case=", out_prefix,
        " | nrow(df)=", nrow(df),
        " | n_psu=", n_psu(df),
        " | length(rep_theta)=", length(rep_theta)
      )
    }

    while (i <= n_draws) {

      sampled_mu <- sample_positive_mean(
        mu = mu,
        var_mu = var_mu
      )

      if (n_psu(df) <= 1) {
        sampled_theta <- theta_hat_single
      } else {
        sampled_theta <- sample(rep_theta_use, 1, replace = TRUE)
      }

      sim_x <- qnbinom(runif(n_sim), size = sampled_theta, mu = sampled_mu)

      if (!anyNA(sim_x)) {
        sim_x <- cap_with_resample(sim_x, max_val)

        out <- data.frame(
          sim_id = rep(i, n_sim),
          my_dom_id_string = rep(dom, n_sim),
          stringsAsFactors = FALSE
        )

        if (fixed_zero_side == "rel") {
          out[[keep_var]] <- sim_x
          out[[rel_var]]  <- rep(0, n_sim)
        } else {
          out[[keep_var]] <- rep(0, n_sim)
          out[[rel_var]]  <- sim_x
        }

        sim_datasets[[i]] <- out
        i <- i + 1
      }
    }

    dplyr::bind_rows(sim_datasets)
  }

  simulate_joint_copula <- function(df, dom) {
    svy_design <- survey::svydesign(
      ids = ~psu_id,
      strata = ~strat_id,
      weights = ~wp_int,
      nest = TRUE,
      data = df
    )
    options(survey.lonely.psu = "certainty")

    mom_keep <- get_svy_mean_var(keep_var, svy_design)
    mom_rel <- get_svy_mean_var(rel_var, svy_design)

    mu_keep <- mom_keep$mu
    mu_rel <- mom_rel$mu
    var_y_keep <- mom_keep$var_y
    var_y_rel <- mom_rel$var_y
    var_mu_keep <- mom_keep$var_mu
    var_mu_rel <- mom_rel$var_mu

    if (!is.finite(var_mu_keep) || var_mu_keep <= 0) var_mu_keep <- mean(df[[se_keep]], na.rm = TRUE)^2
    if (!is.finite(var_mu_rel) || var_mu_rel <= 0) var_mu_rel <- mean(df[[se_rel]], na.rm = TRUE)^2

    max_keep <- round(max(df[[keep_var]], na.rm = TRUE)) + keep_cap_buffer
    max_rel  <- round(max(df[[rel_var]],  na.rm = TRUE)) + rel_cap_buffer

    rep_design <- survey::as.svrepdesign(
      svy_design,
      type = "bootstrap",
      replicates = n_reps
    )

    rep_keep_obj <- get_rep_stat(stats::as.formula(paste0("~", keep_var)), rep_design)
    rep_rel_obj  <- get_rep_stat(stats::as.formula(paste0("~", rel_var)),  rep_design)

    rep_theta_keep <- safe_theta(rep_keep_obj$reps, get_rep_var(df, keep_var, rep_design))
    rep_theta_rel  <- safe_theta(rep_rel_obj$reps,  get_rep_var(df, rel_var,  rep_design))

    theta_hat_keep_single <- safe_theta(mu_keep, var_y_keep)
    theta_hat_rel_single <- safe_theta(mu_rel, var_y_rel)

    rep_theta_keep_use <- rep_theta_keep[is.finite(rep_theta_keep)]
    rep_theta_rel_use  <- rep_theta_rel[is.finite(rep_theta_rel)]
    if (length(rep_theta_keep_use) == 0) rep_theta_keep_use <- theta_hat_keep_single
    if (length(rep_theta_rel_use)  == 0) rep_theta_rel_use  <- theta_hat_rel_single

    df_copula <- weighted_sample_for_copula(
      df = df,
      value_cols = c(keep_var, rel_var),
      n_sim = n_sim,
      weight_col = "wp_int"
    )

    df_copula <- df_copula %>%
      dplyr::mutate(
        rank_keep = rank(.data[[keep_var]], ties.method = "average"),
        rank_rel  = rank(.data[[rel_var]],  ties.method = "average"),
        u_keep = rank_keep / (dplyr::n() + 1),
        u_rel  = rank_rel  / (dplyr::n() + 1)
      )

    u_mat <- as.matrix(df_copula[, c("u_keep", "u_rel")])
    tau_hat <- suppressWarnings(cor(df_copula[[keep_var]], df_copula[[rel_var]], method = "kendall"))
    if (!is.finite(tau_hat)) tau_hat <- 0

    copula_fit <- try({
      if (tau_hat >= 0.3) {
        copula::fitCopula(copula::gumbelCopula(dim = 2), u_mat, method = "mpl", start = 1)
      } else if (tau_hat <= -0.3) {
        copula::fitCopula(copula::normalCopula(dim = 2), u_mat, method = "mpl")
      } else {
        copula::fitCopula(copula::frankCopula(dim = 2), u_mat, method = "mpl", start = 1)
      }
    }, silent = TRUE)

    if (inherits(copula_fit, "try-error")) {
      copula_fit <- copula::fitCopula(copula::normalCopula(dim = 2), u_mat, method = "mpl")
    }

    fitted_cop <- copula_fit@copula

    sim_datasets <- vector("list", n_draws)
    i <- 1

    if (verbose) {
      message(
        "Species=", species_prefix,
        " | Domain=", dom,
        " | Case=keep_and_rel",
        " | nrow(df)=", nrow(df),
        " | n_psu=", n_psu(df),
        " | length(rep_theta_keep)=", length(rep_theta_keep),
        " | length(rep_theta_rel)=", length(rep_theta_rel)
      )
    }

    while (i <= n_draws) {
      sampled_mu_keep <- sample_positive_mean(mu = mu_keep, var_mu = var_mu_keep)
      sampled_mu_rel <- sample_positive_mean(mu = mu_rel, var_mu = var_mu_rel)

      if (n_psu(df) <= 1) {
        sampled_theta_keep <- theta_hat_keep_single
        sampled_theta_rel  <- theta_hat_rel_single
      } else {
        sampled_theta_keep <- sample(rep_theta_keep_use, 1, replace = TRUE)
        sampled_theta_rel  <- sample(rep_theta_rel_use,  1, replace = TRUE)
      }

      uv <- copula::rCopula(n_sim, fitted_cop)
      sim_keep <- qnbinom(uv[, 1], size = sampled_theta_keep, mu = sampled_mu_keep)
      sim_rel  <- qnbinom(uv[, 2], size = sampled_theta_rel,  mu = sampled_mu_rel)

      if (!anyNA(sim_keep) && !anyNA(sim_rel)) {
        sim_keep <- cap_with_resample(sim_keep, max_keep)
        sim_rel  <- cap_with_resample(sim_rel,  max_rel)

        out <- data.frame(
          sim_id = rep(i, n_sim),
          my_dom_id_string = rep(dom, n_sim),
          stringsAsFactors = FALSE
        )
        out[[keep_var]] <- sim_keep
        out[[rel_var]]  <- sim_rel

        sim_datasets[[i]] <- out
        i <- i + 1
      }
    }

    dplyr::bind_rows(sim_datasets)
  }

  simulate_joint_independent <- function(df, dom) {
    svy_design <- survey::svydesign(
      ids = ~psu_id,
      strata = ~strat_id,
      weights = ~wp_int,
      nest = TRUE,
      data = df
    )
    options(survey.lonely.psu = "certainty")

    mom_keep <- get_svy_mean_var(keep_var, svy_design)
    mom_rel <- get_svy_mean_var(rel_var, svy_design)

    mu_keep <- mom_keep$mu
    mu_rel <- mom_rel$mu
    var_y_keep <- mom_keep$var_y
    var_y_rel <- mom_rel$var_y
    var_mu_keep <- mom_keep$var_mu
    var_mu_rel <- mom_rel$var_mu

    if (!is.finite(var_mu_keep) || var_mu_keep <= 0) var_mu_keep <- mean(df[[se_keep]], na.rm = TRUE)^2
    if (!is.finite(var_mu_rel) || var_mu_rel <= 0) var_mu_rel <- mean(df[[se_rel]], na.rm = TRUE)^2

    max_keep <- round(max(df[[keep_var]], na.rm = TRUE)) + keep_cap_buffer
    max_rel  <- round(max(df[[rel_var]],  na.rm = TRUE)) + rel_cap_buffer

    rep_design <- survey::as.svrepdesign(
      svy_design,
      type = "bootstrap",
      replicates = n_reps
    )

    rep_keep_obj <- get_rep_stat(stats::as.formula(paste0("~", keep_var)), rep_design)
    rep_rel_obj  <- get_rep_stat(stats::as.formula(paste0("~", rel_var)),  rep_design)

    rep_theta_keep <- safe_theta(rep_keep_obj$reps, get_rep_var(df, keep_var, rep_design))
    rep_theta_rel  <- safe_theta(rep_rel_obj$reps,  get_rep_var(df, rel_var,  rep_design))

    theta_hat_keep_single <- safe_theta(mu_keep, var_y_keep)
    theta_hat_rel_single <- safe_theta(mu_rel, var_y_rel)

    rep_theta_keep_use <- rep_theta_keep[is.finite(rep_theta_keep)]
    rep_theta_rel_use  <- rep_theta_rel[is.finite(rep_theta_rel)]
    if (length(rep_theta_keep_use) == 0) rep_theta_keep_use <- theta_hat_keep_single
    if (length(rep_theta_rel_use)  == 0) rep_theta_rel_use  <- theta_hat_rel_single

    sim_datasets <- vector("list", n_draws)
    i <- 1

    if (verbose) {
      message(
        "Species=", species_prefix,
        " | Domain=", dom,
        " | Case=keep_and_rel_ind",
        " | nrow(df)=", nrow(df),
        " | n_psu=", n_psu(df),
        " | length(rep_theta_keep)=", length(rep_theta_keep),
        " | length(rep_theta_rel)=", length(rep_theta_rel)
      )
    }

    while (i <= n_draws) {
      sampled_mu_keep <- sample_positive_mean(mu = mu_keep, var_mu = var_mu_keep)
      sampled_mu_rel <- sample_positive_mean(mu = mu_rel, var_mu = var_mu_rel)

      if (n_psu(df) <= 1) {
        sampled_theta_keep <- theta_hat_keep_single
        sampled_theta_rel  <- theta_hat_rel_single
      } else {
        sampled_theta_keep <- sample(rep_theta_keep_use, 1, replace = TRUE)
        sampled_theta_rel  <- sample(rep_theta_rel_use,  1, replace = TRUE)
      }

      sim_keep <- qnbinom(runif(n_sim), size = sampled_theta_keep, mu = sampled_mu_keep)
      sim_rel  <- qnbinom(runif(n_sim), size = sampled_theta_rel,  mu = sampled_mu_rel)

      if (!anyNA(sim_keep) && !anyNA(sim_rel)) {
        sim_keep <- cap_with_resample(sim_keep, max_keep)
        sim_rel  <- cap_with_resample(sim_rel,  max_rel)

        out <- data.frame(
          sim_id = rep(i, n_sim),
          my_dom_id_string = rep(dom, n_sim),
          stringsAsFactors = FALSE
        )
        out[[keep_var]] <- sim_keep
        out[[rel_var]]  <- sim_rel

        sim_datasets[[i]] <- out
        i <- i + 1
      }
    }

    dplyr::bind_rows(sim_datasets)
  }

  for (s in statez) {

    if (verbose) message("Running ", species_prefix, " for state: ", s)

    state_file <- file.path(output_dir, paste0(species_prefix, "_sim_", s, ".rds"))

    # Skip states already completed
    if (file.exists(state_file)) {
      if (verbose) message("Already completed: ", state_file)
      next
    }

    df_state <- full_df %>% dplyr::filter(state == s)
    if (nrow(df_state) == 0) next

    df_only_keep <- df_state %>% dplyr::filter(.data[[flag_only_keep]] == 1)
    df_only_rel  <- df_state %>% dplyr::filter(.data[[flag_only_rel]] == 1)
    df_joint     <- df_state %>% dplyr::filter(.data[[flag_keep_and_rel]] == 1)
    df_none      <- df_state %>% dplyr::filter(.data[[flag_no_catch]] == 1)
    df_ind       <- df_state %>% dplyr::filter(.data[[flag_keep_and_rel_ind]] == 1)

    state_results <- list()

    if (nrow(df_only_keep) > 0) {
      for (dom in unique(df_only_keep$my_dom_id_string)) {
        df_dom <- df_only_keep %>% dplyr::filter(my_dom_id_string == dom)

        state_results[[paste0(species_prefix, "_only_keep_", dom)]] <-
          simulate_one_margin(df_dom, keep_var, se_keep, dom, "only_keep", fixed_zero_side = "rel")

        rm(df_dom)
        gc()
      }
    }

    if (nrow(df_only_rel) > 0) {
      for (dom in unique(df_only_rel$my_dom_id_string)) {
        df_dom <- df_only_rel %>% dplyr::filter(my_dom_id_string == dom)

        state_results[[paste0(species_prefix, "_only_rel_", dom)]] <-
          simulate_one_margin(df_dom, rel_var, se_rel, dom, "only_rel", fixed_zero_side = "keep")

        rm(df_dom)
        gc()
      }
    }

    if (nrow(df_joint) > 0) {
      for (dom in unique(df_joint$my_dom_id_string)) {
        df_dom <- df_joint %>% dplyr::filter(my_dom_id_string == dom)

        state_results[[paste0(species_prefix, "_keep_and_rel_", dom)]] <-
          simulate_joint_copula(df_dom, dom)

        rm(df_dom)
        gc()
      }
    }

    if (nrow(df_none) > 0) {
      for (dom in unique(df_none$my_dom_id_string)) {

        sim_datasets <- vector("list", n_draws)

        for (i in seq_len(n_draws)) {
          out <- data.frame(
            sim_id = rep(i, n_sim),
            my_dom_id_string = rep(dom, n_sim),
            stringsAsFactors = FALSE
          )

          out[[keep_var]] <- rep(0, n_sim)
          out[[rel_var]]  <- rep(0, n_sim)

          sim_datasets[[i]] <- out
        }

        state_results[[paste0(species_prefix, "_no_catch_", dom)]] <- dplyr::bind_rows(sim_datasets)

        rm(sim_datasets, out)
        gc()
      }
    }

    if (nrow(df_ind) > 0) {
      for (dom in unique(df_ind$my_dom_id_string)) {
        df_dom <- df_ind %>% dplyr::filter(my_dom_id_string == dom)

        state_results[[paste0(species_prefix, "_keep_and_rel_ind_", dom)]] <-
          simulate_joint_independent(df_dom, dom)

        rm(df_dom)
        gc()
      }
    }

    if (length(state_results) > 0) {

      state_out <- dplyr::bind_rows(state_results) %>%
        dplyr::mutate(state = s, .before = 1) %>%
        dplyr::group_by(state, my_dom_id_string, sim_id) %>%
        dplyr::mutate(draw_row = dplyr::row_number()) %>%
        dplyr::ungroup()

      saveRDS(state_out, state_file, compress = FALSE)

      if (verbose) message("Saved: ", state_file)

      rm(state_out)
    }

    rm(
      state_results, df_state,
      df_only_keep, df_only_rel, df_joint, df_none, df_ind
    )

    gc()
  }

  invisible(NULL)
}


run_species_copula_sim(
  full_df = full_df,
  species_prefix = "str",
  statez = statez,
  output_dir = output_dir,
  n_sim = n_sim,
  n_draws = n_draws,
  n_reps = n_reps
)

gc()

run_species_copula_sim(
  full_df = full_df,
  species_prefix = "blu",
  statez = statez,
  output_dir = output_dir,
  n_sim = n_sim,
  n_draws = n_draws,
  n_reps = n_reps
)

gc()



join_cols <- c("state", "my_dom_id_string", "sim_id", "draw_row")

setkeyv(str_dt, join_cols)
setkeyv(blu_dt, join_cols)

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}


if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

for (s in statez) {

  message("Combining and writing state: ", s)

  str_file <- file.path(output_dir, paste0("str_sim_", s, ".rds"))
  blu_file <- file.path(output_dir, paste0("blu_sim_", s, ".rds"))

  if (!file.exists(str_file) || !file.exists(blu_file)) {
    warning("Missing species simulation file for state: ", s)
    next
  }

  str_s <- data.table::as.data.table(readRDS(str_file))
  blu_s <- data.table::as.data.table(readRDS(blu_file))

  join_cols <- c("state", "my_dom_id_string", "sim_id", "draw_row")

  data.table::setkeyv(str_s, join_cols)
  data.table::setkeyv(blu_s, join_cols)

  catch_s <- merge(str_s, blu_s, by = join_cols, all = TRUE, sort = FALSE)

  rm(str_s, blu_s)
  gc()

  for (v in c("str_keep", "str_rel", "blu_keep", "blu_rel")) {
    if (!v %in% names(catch_s)) catch_s[, (v) := 0]
    catch_s[is.na(get(v)), (v) := 0]
  }

  catch_s[, `:=`(
    str_catch = str_keep + str_rel,
    blu_catch = blu_keep + blu_rel
  )]

  data.table::setorder(catch_s, state, my_dom_id_string, sim_id, draw_row)

  for (d in seq_len(n_draws)) {

    out_file <- file.path(output_dir, paste0("calib_catch_draws_", s, "_", d, ".dta"))

    # Makes the output-writing stage restartable too
    if (file.exists(out_file)) next

    out_draw <- catch_s[sim_id == d]

    haven::write_dta(out_draw, path = out_file)

    rm(out_draw)
  }

  rm(catch_s)
  gc()

  message("Finished state: ", s)
}


library(data.table)
library(haven)

output_dir <- "E:/Lou_projects/striper_bluefish_RDM/process_data/"
statez <- c("ME", "NH", "MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")
n_draws <- 101

join_cols <- c("state", "my_dom_id_string", "sim_id", "draw_row")

for (s in statez) {

  message("Creating .dta files for state: ", s)

  str_file <- file.path(output_dir, paste0("str_sim_", s, ".rds"))
  blu_file <- file.path(output_dir, paste0("blu_sim_", s, ".rds"))

  if (!file.exists(str_file)) stop("Missing striped bass file: ", str_file)
  if (!file.exists(blu_file)) stop("Missing bluefish file: ", blu_file)

  str_s <- data.table::as.data.table(readRDS(str_file))
  blu_s <- data.table::as.data.table(readRDS(blu_file))

  data.table::setkeyv(str_s, join_cols)
  data.table::setkeyv(blu_s, join_cols)

  catch_s <- merge(str_s, blu_s, by = join_cols, all = TRUE, sort = FALSE)

  rm(str_s, blu_s)
  gc()

  for (v in c("str_keep", "str_rel", "blu_keep", "blu_rel")) {
    if (!v %in% names(catch_s)) catch_s[, (v) := 0]
    catch_s[is.na(get(v)), (v) := 0]
  }

  catch_s[, `:=`(
    str_catch = str_keep + str_rel,
    blu_catch = blu_keep + blu_rel
  )]

  data.table::setorder(catch_s, state, my_dom_id_string, sim_id, draw_row)

  for (d in seq_len(n_draws)) {

    out_draw <- catch_s[sim_id == d]

    out_file <- file.path(
      output_dir,
      paste0("calib_catch_draws_", s, "_", d, ".dta")
    )

    haven::write_dta(out_draw, path = out_file)

    rm(out_draw)
  }

  rm(catch_s)
  gc()

  message("Finished state: ", s)
}
