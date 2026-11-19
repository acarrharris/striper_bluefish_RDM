
options(scipen = 999)

packages <- c("tidyr",  "magrittr", "tidyverse", "reshape2", "splitstackshape","doBy","WriteXLS","Rcpp",
              "ggplot2","rlist","fitdistrplus","MASS","psych","rgl","copula","VineCopula","scales",
              "univariateML","logspline","readr","data.table","conflicted", "readxl", "writexl", "fs", "fst",
              "purrr", "readr", "here", "furrr", "profvis", "future", "magrittr", "feather", "RStata", "haven", "arrow")

#Install only those not already installed
# installed <- packages %in% rownames(installed.packages())
# if (any(!installed)) {
#   install.packages(packages[!installed])
# }

lapply(packages, library, character.only = TRUE)

library(plyr)
library(dplyr)

conflicts_prefer(here::here)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::rename)
conflicts_prefer(dplyr::summarize)
conflicts_prefer(dplyr::summarise)
conflicts_prefer(dplyr::count)
conflicts_prefer(feather::read_feather)

# Set directories consistent with stata model_wrapper.do file.
code_cd=here("Code", "sim")
input_data_cd="E:/Lou_projects/striper_bluefish_RDM/input_data"
iterative_input_data_cd="E:/Lou_projects/striper_bluefish_RDM/process_data"

final_process_data_cd="E:/Lou_projects/striper_bluefish_RDM/final_process_data"
final_process_outcomes_cd="E:/Lou_projects/striper_bluefish_RDM/final_process_data/base_outcomes"
final_process_choice_occasions_cd="E:/Lou_projects/striper_bluefish_RDM/final_process_data/n_choice_occasions"
final_process_misc_cd="E:/Lou_projects/striper_bluefish_RDM/final_process_data/miscellaneous"

projection_output_cd <- if (exists("projection_output_cd")) projection_output_cd else file.path(iterative_input_data_cd, "projection_outputs")
if (!dir.exists(projection_output_cd)) dir.create(projection_output_cd, recursive = TRUE)


# =============================================================================
# Compare latest projected harvest with MRIP and plot harvest at length
# =============================================================================

# Required packages are called explicitly with package::function().

# -----------------------------------------------------------------------------
# 1. Find the most recently saved projection files
# -----------------------------------------------------------------------------

get_latest_file <- function(directory, include_pattern, exclude_pattern = NULL) {

  files <- list.files(
    directory,
    pattern = "\\.fst$",
    full.names = TRUE
  )

  files <- files[
    grepl(include_pattern, basename(files))
  ]

  if (!is.null(exclude_pattern)) {
    files <- files[
      !grepl(exclude_pattern, basename(files))
    ]
  }

  if (length(files) == 0L) {
    stop(
      "No matching .fst files found in: ",
      directory,
      call. = FALSE
    )
  }

  file_info <- file.info(files)

  files[which.max(file_info$mtime)]
}


# Most recent summary file, excluding raw and at-length files.
projection_summary_file <- get_latest_file(
  directory = projection_output_cd,
  include_pattern = "^str_blu_projection_",
  exclude_pattern = "at_length|raw"
)

# Most recent length-level projection file.
projection_length_file <- get_latest_file(
  directory = projection_output_cd,
  include_pattern = "^str_blu_projection_at_length_"
)

message("Projection summary file: ", projection_summary_file)
message("Projection length file: ", projection_length_file)

# -----------------------------------------------------------------------------
# 2. Read the latest saved outputs
# -----------------------------------------------------------------------------

projected_output <- data.table::as.data.table(
  fst::read_fst(projection_summary_file)
)

projected_output_length <- data.table::as.data.table(
  fst::read_fst(projection_length_file)
)


# Inspect the inputs if useful.
print(unique(projected_output$metric))
print(unique(projected_output_length$species))


# -----------------------------------------------------------------------------
# 3. Coastwide projected harvest by draw
# -----------------------------------------------------------------------------
# The saved summary already includes "all modes" rows. Summing those rows over
# region and month produces coastwide totals without double-counting modes.

projected_coast_draw <- projected_output[
  mode == "all modes" &
    metric %in% c("harvest (#s)", "harvest (lbs.)"),
  .(
    projected_value = sum(projected_value, na.rm = TRUE)
  ),
  by = .(
    scenario,
    iteration,
    species,
    metric
  )
]


# Summarize the distribution across simulation draws.
projected_coast_summary <- projected_coast_draw[
  ,
  .(
    projected_mean = mean(projected_value, na.rm = TRUE),
    projected_median = stats::median(projected_value, na.rm = TRUE),
    projected_sd = stats::sd(projected_value, na.rm = TRUE),
    projected_p05 = stats::quantile(
      projected_value,
      probs = 0.05,
      na.rm = TRUE,
      names = FALSE
    ),
    projected_p25 = stats::quantile(
      projected_value,
      probs = 0.25,
      na.rm = TRUE,
      names = FALSE
    ),
    projected_p75 = stats::quantile(
      projected_value,
      probs = 0.75,
      na.rm = TRUE,
      names = FALSE
    ),
    projected_p95 = stats::quantile(
      projected_value,
      probs = 0.95,
      na.rm = TRUE,
      names = FALSE
    ),
    n_draws = sum(is.finite(projected_value))
  ),
  by = .(
    scenario,
    species,
    metric
  )
]


# -----------------------------------------------------------------------------
# 4. Read coastwide MRIP estimates
# -----------------------------------------------------------------------------
# Expected columns:
#   species
#   metric
#   total_mrip
#
# The metric labels should include:
#   "harvest (#s)"
#   "harvest (lbs.)"

mrip_coast <- data.table::as.data.table(
  haven::read_dta(
    file.path(input_data_cd, "mrip_catch_coast.dta")
  )
)

mrip_coast <- mrip_coast[
  metric %in% c("harvest (#s)", "harvest (lbs.)")
]


# -----------------------------------------------------------------------------
# 5. Compare projected and MRIP coastwide harvest
# -----------------------------------------------------------------------------

compare_coast <- merge(
  projected_coast_summary,
  mrip_coast,
  by = c("species", "metric"),
  all.x = TRUE
)

compare_coast[
  ,
  `:=`(
    difference_mean = projected_mean - total_mrip,
    pct_difference_mean = data.table::fifelse(
      is.finite(total_mrip) & total_mrip != 0,
      100 * (projected_mean - total_mrip) / total_mrip,
      NA_real_
    ),
    difference_median = projected_median - total_mrip,
    pct_difference_median = data.table::fifelse(
      is.finite(total_mrip) & total_mrip != 0,
      100 * (projected_median - total_mrip) / total_mrip,
      NA_real_
    )
  )
]

round_cols <- c(
  "projected_mean",
  "projected_median",
  "projected_sd",
  "projected_p05",
  "projected_p25",
  "projected_p75",
  "projected_p95",
  "total_mrip",
  "difference_mean",
  "pct_difference_mean",
  "difference_median",
  "pct_difference_median"
)

round_cols <- intersect(round_cols, names(compare_coast))

compare_coast[
  ,
  (round_cols) := lapply(.SD, round, digits = 1),
  .SDcols = round_cols
]

data.table::setorder(
  compare_coast,
  species,
  metric
)

print(compare_coast)


# Save the coastwide comparison.
# data.table::fwrite(
#   compare_coast,
#   file.path(
#     projection_output_cd,
#     "latest_projection_vs_mrip_coastwide.csv"
#   )
# )

# fst::write_fst(
#   compare_coast,
#   file.path(
#     projection_output_cd,
#     "latest_projection_vs_mrip_coastwide.fst"
#   )
# )


# -----------------------------------------------------------------------------
# 6. Calculate pounds per harvested fish
# -----------------------------------------------------------------------------

compare_coast_wide <- data.table::dcast(
  compare_coast,
  scenario + species ~ metric,
  value.var = c(
    "projected_mean",
    "projected_median",
    "total_mrip"
  )
)

compare_coast_wide[
  ,
  `:=`(
    projected_mean_lb_per_fish =
      `projected_mean_harvest (lbs.)` /
      `projected_mean_harvest (#s)`,

    projected_median_lb_per_fish =
      `projected_median_harvest (lbs.)` /
      `projected_median_harvest (#s)`,

    mrip_lb_per_fish =
      `total_mrip_harvest (lbs.)` /
      `total_mrip_harvest (#s)`
  )
]

compare_coast_wide[
  ,
  `:=`(
    mean_weight_difference =
      projected_mean_lb_per_fish - mrip_lb_per_fish,

    mean_weight_ratio =
      projected_mean_lb_per_fish / mrip_lb_per_fish
  )
]

weight_cols <- c(
  "projected_mean_lb_per_fish",
  "projected_median_lb_per_fish",
  "mrip_lb_per_fish",
  "mean_weight_difference",
  "mean_weight_ratio"
)

compare_coast_wide[
  ,
  (weight_cols) := lapply(.SD, round, digits = 2),
  .SDcols = weight_cols
]

print(compare_coast_wide)

# data.table::fwrite(
#   compare_coast_wide,
#   file.path(
#     projection_output_cd,
#     "latest_projection_vs_mrip_average_weight.csv"
#   )
# )


# -----------------------------------------------------------------------------
# 7. Optional state-level harvest-weight comparison
# -----------------------------------------------------------------------------

projected_state_draw <- projected_output[
  mode == "all modes" &
    metric %in% c("harvest (#s)", "harvest (lbs.)"),
  .(
    projected_value = sum(projected_value, na.rm = TRUE)
  ),
  by = .(
    scenario,
    iteration,
    state = substr(region, 1, 2),
    species,
    metric
  )
]

projected_state_summary <- projected_state_draw[
  ,
  .(
    projected_mean = mean(projected_value, na.rm = TRUE),
    projected_median = stats::median(projected_value, na.rm = TRUE),
    projected_sd = stats::sd(projected_value, na.rm = TRUE),
    projected_p05 = stats::quantile(
      projected_value,
      0.05,
      na.rm = TRUE,
      names = FALSE
    ),
    projected_p95 = stats::quantile(
      projected_value,
      0.95,
      na.rm = TRUE,
      names = FALSE
    ),
    n_draws = sum(is.finite(projected_value))
  ),
  by = .(
    scenario,
    state,
    species,
    metric
  )
]

mrip_state <- data.table::as.data.table(
  haven::read_dta(
    file.path(input_data_cd, "mrip_catch_state.dta")
  )
)

mrip_state <- mrip_state[
  metric %in% c("harvest (#s)", "harvest (lbs.)")
]

compare_state <- merge(
  projected_state_summary,
  mrip_state,
  by = c("state", "species", "metric"),
  all.x = TRUE
)

compare_state[
  ,
  `:=`(
    difference_mean = projected_mean - total_mrip,
    difference_median = projected_median - total_mrip
  )
]

compare_state[
  ,
  `:=`(
    pct_difference_mean = data.table::fifelse(
      is.finite(total_mrip) & total_mrip != 0,
      100 * difference_mean / total_mrip,
      NA_real_
    ),

    pct_difference_median = data.table::fifelse(
      is.finite(total_mrip) & total_mrip != 0,
      100 * difference_median / total_mrip,
      NA_real_
    )
  )
]

state_round_cols <- intersect(
  c(
    "projected_mean",
    "projected_median",
    "projected_sd",
    "projected_p05",
    "projected_p95",
    "total_mrip",
    "difference_mean",
    "pct_difference_mean",
    "difference_median",
    "pct_difference_median"
  ),
  names(compare_state)
)

compare_state[
  ,
  (state_round_cols) := lapply(.SD, round, digits = 1),
  .SDcols = state_round_cols
]

data.table::setorder(
  compare_state,
  state,
  species,
  metric
)

# data.table::fwrite(
#   compare_state,
#   file.path(
#     projection_output_cd,
#     "latest_projection_vs_mrip_by_state.csv"
#   )
# )

# -----------------------------------------------------------------------------
# Coastwide harvest by two-inch length bin and simulation draw
# -----------------------------------------------------------------------------

harvest_length_draw_2in <- projected_output_length[
  mode == "all modes",
  .(
    harvest_n = sum(harvest_n, na.rm = TRUE),
    harvest_weight_lb = sum(harvest_weight_lb, na.rm = TRUE)
  ),
  by = .(
    scenario,
    iteration,
    species,

    # Combine the existing one-inch bins into two-inch bins.
    length_bin_in = floor(length_bin_in / 2) * 2
  )
]

harvest_length_draw_2in[
  ,
  `:=`(
    length_bin_upper_in = length_bin_in + 2,
    length_bin_mid_in = length_bin_in + 1
  )
]

# -----------------------------------------------------------------------------
# Restore zero-valued bins that are absent from individual draws
# -----------------------------------------------------------------------------

length_ranges <- harvest_length_draw_2in[
  ,
  .(
    min_length_bin = min(length_bin_in, na.rm = TRUE),
    max_length_bin = max(length_bin_in, na.rm = TRUE)
  ),
  by = .(
    scenario,
    species
  )
]

complete_length_grid <- length_ranges[
  ,
  .(
    length_bin_in = seq(
      min_length_bin,
      max_length_bin,
      by = 2
    )
  ),
  by = .(
    scenario,
    species
  )
]

draw_grid <- unique(
  harvest_length_draw_2in[
    ,
    .(
      scenario,
      iteration,
      species
    )
  ]
)

complete_length_grid <- merge(
  draw_grid,
  complete_length_grid,
  by = c(
    "scenario",
    "species"
  ),
  allow.cartesian = TRUE
)

harvest_length_draw_2in <- merge(
  complete_length_grid,
  harvest_length_draw_2in,
  by = c(
    "scenario",
    "iteration",
    "species",
    "length_bin_in"
  ),
  all.x = TRUE
)

harvest_length_draw_2in[
  is.na(harvest_n),
  harvest_n := 0
]

harvest_length_draw_2in[
  is.na(harvest_weight_lb),
  harvest_weight_lb := 0
]

harvest_length_draw_2in[
  ,
  `:=`(
    length_bin_upper_in = length_bin_in + 2,
    length_bin_mid_in = length_bin_in + 1
  )
]

# -----------------------------------------------------------------------------
# Median and simulation intervals across draws
# -----------------------------------------------------------------------------

harvest_length_summary_2in <- harvest_length_draw_2in[
  ,
  .(
    median_harvest_n =
      stats::median(harvest_n, na.rm = TRUE),

    lower_harvest_n =
      stats::quantile(
        harvest_n,
        probs = 0.05,
        na.rm = TRUE,
        names = FALSE
      ),

    upper_harvest_n =
      stats::quantile(
        harvest_n,
        probs = 0.95,
        na.rm = TRUE,
        names = FALSE
      ),

    median_harvest_weight_lb =
      stats::median(harvest_weight_lb, na.rm = TRUE),

    lower_harvest_weight_lb =
      stats::quantile(
        harvest_weight_lb,
        probs = 0.05,
        na.rm = TRUE,
        names = FALSE
      ),

    upper_harvest_weight_lb =
      stats::quantile(
        harvest_weight_lb,
        probs = 0.95,
        na.rm = TRUE,
        names = FALSE
      ),

    n_draws = data.table::uniqueN(iteration)
  ),
  by = .(
    scenario,
    species,
    length_bin_in,
    length_bin_upper_in,
    length_bin_mid_in
  )
]

data.table::setorder(
  harvest_length_summary_2in,
  species,
  length_bin_in
)

harvest_numbers_plot <- ggplot2::ggplot(
  harvest_length_summary_2in,
  ggplot2::aes(
    x = length_bin_mid_in,
    y = median_harvest_n
  )
) +
  ggplot2::geom_col(
    width = 1.8
  ) +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      ymin = lower_harvest_n,
      ymax = upper_harvest_n
    ),
    width = 0.5
  ) +
  ggplot2::facet_wrap(
    ~ species,
    scales = "free_y",
    ncol = 1
  ) +
  ggplot2::scale_x_continuous(
    breaks = scales::breaks_pretty(n = 12)
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::label_number(
      scale_cut = scales::cut_short_scale()
    )
  ) +
  ggplot2::labs(
    x = "Fish length (two-inch bins)",
    y = "Projected harvest (number of fish)",
    title = "Projected recreational harvest at length",
    subtitle = "Bars show medians; intervals show 5th–95th percentiles across simulations"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank()
  )

print(harvest_numbers_plot)

harvest_weight_plot <- ggplot2::ggplot(
  harvest_length_summary_2in,
  ggplot2::aes(
    x = length_bin_mid_in,
    y = median_harvest_weight_lb
  )
) +
  ggplot2::geom_col(
    width = 1.8
  ) +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      ymin = lower_harvest_weight_lb,
      ymax = upper_harvest_weight_lb
    ),
    width = 0.5
  ) +
  ggplot2::facet_wrap(
    ~ species,
    scales = "free_y",
    ncol = 1
  ) +
  ggplot2::scale_x_continuous(
    breaks = scales::breaks_pretty(n = 12)
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::label_number(
      scale_cut = scales::cut_short_scale()
    )
  ) +
  ggplot2::labs(
    x = "Fish length (two-inch bins)",
    y = "Projected harvest weight (lb)",
    title = "Projected recreational harvest weight at length",
    subtitle = "Bars show medians; intervals show 5th–95th percentiles across simulations"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank()
  )

print(harvest_weight_plot)



# Median and uncertainty intervals across the simulation draws.
harvest_length_summary <- harvest_length_draw[
  ,
  .(
    median_harvest_n =
      stats::median(harvest_n, na.rm = TRUE),

    p25_harvest_n =
      stats::quantile(
        harvest_n,
        0.25,
        na.rm = TRUE,
        names = FALSE
      ),

    p75_harvest_n =
      stats::quantile(
        harvest_n,
        0.75,
        na.rm = TRUE,
        names = FALSE
      ),

    p05_harvest_n =
      stats::quantile(
        harvest_n,
        0.05,
        na.rm = TRUE,
        names = FALSE
      ),

    p95_harvest_n =
      stats::quantile(
        harvest_n,
        0.95,
        na.rm = TRUE,
        names = FALSE
      ),

    median_harvest_weight_lb =
      stats::median(harvest_weight_lb, na.rm = TRUE),

    p25_harvest_weight_lb =
      stats::quantile(
        harvest_weight_lb,
        0.25,
        na.rm = TRUE,
        names = FALSE
      ),

    p75_harvest_weight_lb =
      stats::quantile(
        harvest_weight_lb,
        0.75,
        na.rm = TRUE,
        names = FALSE
      ),

    p05_harvest_weight_lb =
      stats::quantile(
        harvest_weight_lb,
        0.05,
        na.rm = TRUE,
        names = FALSE
      ),

    p95_harvest_weight_lb =
      stats::quantile(
        harvest_weight_lb,
        0.95,
        na.rm = TRUE,
        names = FALSE
      ),

    n_draws = data.table::uniqueN(iteration)
  ),
  by = .(
    scenario,
    species,
    length_bin_in,
    length_bin_upper_in,
    length_bin_mid_in
  )
]

data.table::setorder(
  harvest_length_summary,
  species,
  length_bin_in
)

# data.table::fwrite(
#   harvest_length_summary,
#   file.path(
#     projection_output_cd,
#     "latest_projection_harvest_at_length_summary.csv"
#   )
# )
#
# fst::write_fst(
#   harvest_length_summary,
#   file.path(
#     projection_output_cd,
#     "latest_projection_harvest_at_length_summary.fst"
#   )
# )


# -----------------------------------------------------------------------------
# 9. Plot median harvested numbers by length
# -----------------------------------------------------------------------------

harvest_number_plot <- ggplot2::ggplot(
  harvest_length_summary,
  ggplot2::aes(
    x = length_bin_mid_in,
    y = median_harvest_n
  )
) +
  ggplot2::geom_ribbon(
    ggplot2::aes(
      ymin = p25_harvest_n,
      ymax = p75_harvest_n
    ),
    alpha = 0.25
  ) +
  ggplot2::geom_line(
    linewidth = 0.8
  ) +
  ggplot2::geom_point(
    size = 1.4
  ) +
  ggplot2::facet_wrap(
    ~ species,
    scales = "free_y",
    ncol = 1
  ) +
  ggplot2::scale_x_continuous(
    breaks = scales::breaks_pretty(n = 12)
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::label_number(
      scale_cut = scales::cut_short_scale()
    )
  ) +
  ggplot2::labs(
    x = "Length bin midpoint (inches)",
    y = "Median projected harvest (number of fish)",
    title = "Projected recreational harvest by fish length",
    subtitle = "Line shows median across simulation draws; ribbon shows interquartile range"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "none",
    strip.background = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank()
  )

print(harvest_number_plot)

harvest_number_plot
# -----------------------------------------------------------------------------
# 10. Plot median harvest weight by length
# -----------------------------------------------------------------------------

harvest_weight_plot <- ggplot2::ggplot(
  harvest_length_summary,
  ggplot2::aes(
    x = length_bin_mid_in,
    y = median_harvest_weight_lb
  )
) +
  ggplot2::geom_ribbon(
    ggplot2::aes(
      ymin = p25_harvest_weight_lb,
      ymax = p75_harvest_weight_lb
    ),
    alpha = 0.25
  ) +
  ggplot2::geom_line(
    linewidth = 0.8
  ) +
  ggplot2::geom_point(
    size = 1.4
  ) +
  ggplot2::facet_wrap(
    ~ species,
    scales = "free_y",
    ncol = 1
  ) +
  ggplot2::scale_x_continuous(
    breaks = scales::breaks_pretty(n = 12)
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::label_number(
      scale_cut = scales::cut_short_scale()
    )
  ) +
  ggplot2::labs(
    x = "Length bin midpoint (inches)",
    y = "Median projected harvest weight (lb)",
    title = "Projected recreational harvest weight by fish length",
    subtitle = "Line shows median across simulation draws; ribbon shows interquartile range"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "none",
    strip.background = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank()
  )

print(harvest_weight_plot)
harvest_weight_plot

# -----------------------------------------------------------------------------
# 11. Save the plots
# -----------------------------------------------------------------------------

# ggplot2::ggsave(
#   filename = file.path(
#     projection_output_cd,
#     "latest_projection_harvest_numbers_at_length.png"
#   ),
#   plot = harvest_number_plot,
#   width = 8,
#   height = 8,
#   units = "in",
#   dpi = 300
# )
#
# ggplot2::ggsave(
#   filename = file.path(
#     projection_output_cd,
#     "latest_projection_harvest_weight_at_length.png"
#   ),
#   plot = harvest_weight_plot,
#   width = 8,
#   height = 8,
#   units = "in",
#   dpi = 300
# )









# =============================================================================
# State-level MRIP versus simulated projection comparison
# =============================================================================

#Side-by-side MRIP and simulation comparison


# Most recent summary file, excluding raw and at-length files.
projection_summary_file <- get_latest_file(
  directory = projection_output_cd,
  include_pattern = "^str_blu_projection_",
  exclude_pattern = "at_length|raw"
)

# Most recent length-level projection file.
projection_length_file <- get_latest_file(
  directory = projection_output_cd,
  include_pattern = "^str_blu_projection_at_length_"
)

message("Projection summary file: ", projection_summary_file)
message("Projection length file: ", projection_length_file)

# -----------------------------------------------------------------------------
# 2. Read the latest saved outputs
# -----------------------------------------------------------------------------

projected_output <- data.table::as.data.table(
  fst::read_fst(projection_summary_file)
)

projected_output_length <- data.table::as.data.table(
  fst::read_fst(projection_length_file)
)


# Summarize projections within each draw.
projected_state_draw <- projected_output[
  mode == "all modes" &
    metric %in% c("harvest (#s)", "harvest (lbs.)"),
  .(
    projected_value = sum(projected_value, na.rm = TRUE)
  ),
  by = .(
    scenario,
    iteration,
    state = substr(region, 1, 2),
    species,
    metric
  )
]


# Summarize the simulated distribution across draws.
projected_state_summary <- projected_state_draw[
  ,
  .(
    projected_mean = mean(
      projected_value,
      na.rm = TRUE
    ),

    projected_median = stats::median(
      projected_value,
      na.rm = TRUE
    ),

    projected_sd = stats::sd(
      projected_value,
      na.rm = TRUE
    ),

    projected_lower = stats::quantile(
      projected_value,
      probs = 0.05,
      na.rm = TRUE,
      names = FALSE
    ),

    projected_upper = stats::quantile(
      projected_value,
      probs = 0.95,
      na.rm = TRUE,
      names = FALSE
    ),

    n_draws = sum(
      is.finite(projected_value)
    )
  ),
  by = .(
    scenario,
    state,
    species,
    metric
  )
]


# Read MRIP state estimates.
mrip_state <- data.table::as.data.table(
  haven::read_dta(
    file.path(
      input_data_cd,
      "mrip_catch_state.dta"
    )
  )
)

mrip_state <- mrip_state[
  metric %in% c(
    "harvest (#s)",
    "harvest (lbs.)"
  )
]

names(mrip_state)
data.table::setnames(
  mrip_state,
  old = c("ll95_mrip", "ul95_mrip"),
  new = c("lower_mrip", "upper_mrip")
)

# Confirm these variables are available.
required_mrip_cols <- c(
  "state",
  "species",
  "metric",
  "total_mrip",
  "lower_mrip",
  "upper_mrip"
)

missing_mrip_cols <- setdiff(
  required_mrip_cols,
  names(mrip_state)
)

if (length(missing_mrip_cols) > 0L) {
  stop(
    "The MRIP data are missing these required columns: ",
    paste(missing_mrip_cols, collapse = ", "),
    call. = FALSE
  )
}

# Check uniqueness of the merge keys.
mrip_duplicates <- mrip_state[
  ,
  .(
    n_rows = .N,
    n_total_values = data.table::uniqueN(total_mrip),
    total_values = paste(
      sort(unique(total_mrip)),
      collapse = ", "
    )
  ),
  by = .(
    state,
    species,
    metric
  )
][
  n_rows > 1
]

print(mrip_duplicates)
mrip_nc_blue <- mrip_state[
  state == "NC" &
    species == "bluefish" &
    metric == "harvest (#s)"
]

print(mrip_nc_blue)

calibration_nc_blue <- calib[
  species == "bluefish" &
    region %in% c(
      "NCALB",
      "NCCNTRL",
      "NCOCN"
    ),
  .(
    calibration_MRIP_keep = sum(
      MRIP_keep,
      na.rm = TRUE
    )
  ),
  by = draw
][
  ,
  .(
    mean_calibration_MRIP_keep =
      mean(calibration_MRIP_keep),

    median_calibration_MRIP_keep =
      stats::median(calibration_MRIP_keep),

    min_calibration_MRIP_keep =
      min(calibration_MRIP_keep),

    max_calibration_MRIP_keep =
      max(calibration_MRIP_keep)
  )
]

print(calibration_nc_blue)

data.table::data.table(
  source = c(
    "MRIP used in calibration",
    "MRIP state comparison file"
  ),
  harvest_numbers = c(
    calibration_nc_blue$mean_calibration_MRIP_keep,
    mrip_nc_blue$total_mrip[1]
  )
)


nc_calib_decomposition <- calib[
  species == "bluefish" &
    region %in% c(
      "NCALB",
      "NCCNTRL",
      "NCOCN"
    ),
  .(
    mean_MRIP_keep = mean(
      MRIP_keep,
      na.rm = TRUE
    ),

    median_MRIP_keep = stats::median(
      MRIP_keep,
      na.rm = TRUE
    ),

    min_MRIP_keep = min(
      MRIP_keep,
      na.rm = TRUE
    ),

    max_MRIP_keep = max(
      MRIP_keep,
      na.rm = TRUE
    )
  ),
  by = .(
    region,
    mode
  )
]

data.table::setorder(
  nc_calib_decomposition,
  region,
  mode
)

print(nc_calib_decomposition)

nc_region_draw <- calib[
  species == "bluefish" &
    region %in% c(
      "NCALB",
      "NCCNTRL",
      "NCOCN"
    ),
  .(
    MRIP_keep = sum(
      MRIP_keep,
      na.rm = TRUE
    )
  ),
  by = .(
    draw,
    region
  )
]

nc_region_summary <- nc_region_draw[
  ,
  .(
    mean_MRIP_keep = mean(
      MRIP_keep,
      na.rm = TRUE
    ),

    median_MRIP_keep = stats::median(
      MRIP_keep,
      na.rm = TRUE
    )
  ),
  by = region
]

nc_region_summary[
  ,
  share_of_calibration_total :=
    mean_MRIP_keep /
    sum(mean_MRIP_keep)
]

data.table::setorder(
  nc_region_summary,
  -mean_MRIP_keep
)

print(nc_region_summary)
# Merge projection and MRIP results.
compare_state <- merge(
  projected_state_summary,
  mrip_state,
  by = c(
    "state",
    "species",
    "metric"
  ),
  all.x = TRUE
)


# Differences.
compare_state[
  ,
  `:=`(
    difference_mean =
      projected_mean - total_mrip,

    difference_median =
      projected_median - total_mrip
  )
]

compare_state[
  ,
  `:=`(
    pct_difference_mean = data.table::fifelse(
      is.finite(total_mrip) & total_mrip != 0,
      100 * difference_mean / total_mrip,
      NA_real_
    ),

    pct_difference_median = data.table::fifelse(
      is.finite(total_mrip) & total_mrip != 0,
      100 * difference_median / total_mrip,
      NA_real_
    )
  )
]


data.table::setorder(
  compare_state,
  species,
  metric,
  state
)


# data.table::fwrite(
#   compare_state,
#   file.path(
#     projection_output_cd,
#     "latest_projection_vs_mrip_by_state.csv"
#   )
# )

mrip_plot_data <- compare_state[
  ,
  .(
    scenario,
    state,
    species,
    metric,

    source = "MRIP",

    estimate = total_mrip,
    lower = lower_mrip,
    upper = upper_mrip
  )
]


simulation_plot_data <- compare_state[
  ,
  .(
    scenario,
    state,
    species,
    metric,

    source = "Simulation",

    estimate = projected_median,
    lower = projected_lower,
    upper = projected_upper
  )
]


state_plot_data <- data.table::rbindlist(
  list(
    mrip_plot_data,
    simulation_plot_data
  ),
  use.names = TRUE,
  fill = TRUE
)


state_plot_data[
  ,
  source := factor(
    source,
    levels = c(
      "MRIP",
      "Simulation"
    )
  )
]


state_weight_plot <- ggplot2::ggplot(
  state_plot_data[
    metric == "harvest (lbs.)"
  ],
  ggplot2::aes(
    x = state,
    y = estimate,
    group = source,
    shape = source
  )
) +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      ymin = lower,
      ymax = upper
    ),
    position = ggplot2::position_dodge(
      width = 0.55
    ),
    width = 0.18,
    linewidth = 0.6
  ) +
  ggplot2::geom_point(
    position = ggplot2::position_dodge(
      width = 0.55
    ),
    size = 2.5
  ) +
  ggplot2::facet_wrap(
    ~ species,
    scales = "free_y",
    ncol = 1
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::label_number(
      scale_cut = scales::cut_short_scale()
    )
  ) +
  ggplot2::labs(
    x = NULL,
    y = "Harvest weight (lb)",
    shape = NULL,
    title = "MRIP and simulated recreational harvest weight",
    subtitle = paste0(
      "MRIP intervals use reported bounds; ",
      "simulation intervals show the 5th–95th percentiles across draws"
    )
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "bottom",
    panel.grid.minor = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank()
  )

print(state_weight_plot)
state_weight_plot
# ggplot2::ggsave(
#   filename = file.path(
#     projection_output_cd,
#     "mrip_vs_simulated_harvest_weight_by_state.png"
#   ),
#   plot = state_weight_plot,
#   width = 10,
#   height = 8,
#   units = "in",
#   dpi = 300
# )


state_numbers_plot <- ggplot2::ggplot(
  state_plot_data[
    metric == "harvest (#s)"
  ],
  ggplot2::aes(
    x = state,
    y = estimate,
    group = source,
    shape = source
  )
) +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      ymin = lower,
      ymax = upper
    ),
    position = ggplot2::position_dodge(
      width = 0.55
    ),
    width = 0.18,
    linewidth = 0.6
  ) +
  ggplot2::geom_point(
    position = ggplot2::position_dodge(
      width = 0.55
    ),
    size = 2.5
  ) +
  ggplot2::facet_wrap(
    ~ species,
    scales = "free_y",
    ncol = 1
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::label_number(
      scale_cut = scales::cut_short_scale()
    )
  ) +
  ggplot2::labs(
    x = NULL,
    y = "Harvested fish",
    shape = NULL,
    title = "MRIP and simulated recreational harvest numbers",
    subtitle = paste0(
      "MRIP intervals use reported bounds; ",
      "simulation intervals show the 5th–95th percentiles across draws"
    )
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "bottom",
    panel.grid.minor = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank()
  )

print(state_numbers_plot)
state_numbers_plot

# 7. Coastwide version
names(compare_coast)
data.table::setnames(
  compare_coast,
  old = c("ll95_mrip", "ul95_mrip"),
  new = c("lower_mrip", "upper_mrip")
)
mrip_coast_plot <- compare_coast[
  ,
  .(
    scenario,
    species,
    metric,

    source = "MRIP",

    estimate = total_mrip,
    lower = lower_mrip,
    upper = upper_mrip
  )
]


simulation_coast_plot <- compare_coast[
  ,
  .(
    scenario,
    species,
    metric,

    source = "Simulation",

    estimate = projected_median,
    lower = projected_p05,
    upper = projected_p95
  )
]


coast_plot_data <- data.table::rbindlist(
  list(
    mrip_coast_plot,
    simulation_coast_plot
  ),
  use.names = TRUE,
  fill = TRUE
)

coast_weight_plot <- ggplot2::ggplot(
  coast_plot_data[
    metric == "harvest (lbs.)"
  ],
  ggplot2::aes(
    x = species,
    y = estimate,
    group = source,
    shape = source
  )
) +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      ymin = lower,
      ymax = upper
    ),
    position = ggplot2::position_dodge(
      width = 0.5
    ),
    width = 0.15
  ) +
  ggplot2::geom_point(
    position = ggplot2::position_dodge(
      width = 0.5
    ),
    size = 3
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::label_number(
      scale_cut = scales::cut_short_scale()
    )
  ) +
  ggplot2::labs(
    x = NULL,
    y = "Harvest weight)",
    shape = NULL,
    title = "Coastwide MRIP and simulated harvest weight",
    subtitle = paste0(
      "Simulation point is the median; interval is the ",
      "5th–95th percentile range across draws"
    )
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "bottom",
    panel.grid.minor = ggplot2::element_blank()
  )

print(coast_weight_plot)
