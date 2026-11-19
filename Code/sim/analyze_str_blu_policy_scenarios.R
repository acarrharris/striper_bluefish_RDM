options(scipen = 999)

# =============================================================================
# Analyze striped bass / bluefish policy-scenario projection outputs
# =============================================================================
# Outputs:
#   1. Coastwide biological outcomes, predicted trips, and aggregate CV
#   2. State- or region-level biological outcomes, predicted trips, and CV
#   3. Coastwide biological outcomes at length in two-inch bins
#   4. Status-quo simulation versus MRIP comparisons
#
# Biological definitions:
#   Harvest         = retained fish
#   Discards        = all released fish
#   Dead discards   = released fish multiplied by species discard mortality
#   Total mortality = harvest + dead discards
#
# The projection summary files contain releases rather than dead discards, so
# dead discards are calculated below using the same rates as the projection.
# The at-length files already contain dead_discard_n and
# dead_discard_weight_lb and those saved values are used directly.
# =============================================================================

# -----------------------------------------------------------------------------
# 1. User controls
# -----------------------------------------------------------------------------

input_data_cd <- "E:/Lou_projects/striper_bluefish_RDM/input_data"
projection_output_cd <- paste0(
  "E:/Lou_projects/striper_bluefish_RDM/process_data/",
  "projection_outputs"
)

analysis_output_cd <- file.path(projection_output_cd, "policy_analysis")
plot_output_cd <- file.path(analysis_output_cd, "plots")
table_output_cd <- file.path(analysis_output_cd, "tables")

dir.create(plot_output_cd, recursive = TRUE, showWarnings = FALSE)
dir.create(table_output_cd, recursive = TRUE, showWarnings = FALSE)

scenario_names <- c(
  "status_quo_2023",
  "moratorium_both_2023",
  "moratorium_striper_2023",
  "moratorium_bluefish_2023",
  "regulations_2024"
)

scenario_labels <- c(
  status_quo_2023 = "2023 status quo",
  moratorium_both_2023 = "Moratorium: both species",
  moratorium_striper_2023 = "Moratorium: striped bass",
  moratorium_bluefish_2023 = "Moratorium: bluefish",
  regulations_2024 = "2024 regulations and conditions"
)

species_labels <- c(
  striper = "Striped bass",
  bluefish = "Bluefish",
  `all species` = "All species"
)

# Change to "region" to preserve the projection management regions.
spatial_level <- "state"

# Simulation intervals displayed in tables and plots.
lower_probability <- 0.05
upper_probability <- 0.95

# These must match the projection code.
discard_mortality_rates <- c(
  striper = 0.09,
  bluefish = 0.094
)

# Set FALSE to create objects without writing every plot and table.
save_outputs <- TRUE

# -----------------------------------------------------------------------------
# 2. General helpers
# -----------------------------------------------------------------------------

check_required_cols <- function(data, required, object_name) {
  missing_cols <- setdiff(required, names(data))

  if (length(missing_cols) > 0L) {
    stop(
      object_name,
      " is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

safe_quantile <- function(x, probability) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) return(NA_real_)

  stats::quantile(
    x,
    probs = probability,
    na.rm = TRUE,
    names = FALSE
  )
}

summarize_draws <- function(data, grouping_cols, value_col = "value") {
  data[
    ,
    .(
      mean = mean(get(value_col), na.rm = TRUE),
      median = stats::median(get(value_col), na.rm = TRUE),
      sd = stats::sd(get(value_col), na.rm = TRUE),
      lower = safe_quantile(get(value_col), lower_probability),
      upper = safe_quantile(get(value_col), upper_probability),
      n_draws = data.table::uniqueN(iteration[is.finite(get(value_col))])
    ),
    by = grouping_cols
  ]
}

add_display_labels <- function(data) {
  out <- data.table::copy(data)

  if ("scenario" %in% names(out)) {
    out[, scenario_label := factor(
      scenario_labels[scenario],
      levels = unname(scenario_labels[scenario_names])
    )]
  }

  if ("species" %in% names(out)) {
    out[, species_label := factor(
      species_labels[species],
      levels = unname(species_labels)
    )]
  }

  out[]
}

short_number_scale <- function() {
  ggplot2::scale_y_continuous(
    labels = scales::label_number(
      scale_cut = scales::cut_short_scale()
    )
  )
}

save_plot <- function(plot, filename, width, height) {
  if (!isTRUE(save_outputs)) return(invisible(NULL))

  ggplot2::ggsave(
    filename = file.path(plot_output_cd, filename),
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = 300
  )

  invisible(NULL)
}

save_table <- function(data, filename) {
  if (!isTRUE(save_outputs)) return(invisible(NULL))

  data.table::fwrite(
    data,
    file.path(table_output_cd, filename)
  )

  invisible(NULL)
}

# -----------------------------------------------------------------------------
# 3. Read all five scenarios
# -----------------------------------------------------------------------------

read_scenario_file <- function(scenario, output_type = c("raw", "length")) {
  output_type <- match.arg(output_type)

  file_prefix <- if (output_type == "raw") {
    "str_blu_projection_raw_"
  } else {
    "str_blu_projection_at_length_"
  }

  file_path <- file.path(
    projection_output_cd,
    paste0(file_prefix, scenario, ".fst")
  )

  if (!file.exists(file_path)) {
    stop("Missing projection output: ", file_path, call. = FALSE)
  }

  out <- data.table::as.data.table(fst::read_fst(file_path))

  if (!"scenario" %in% names(out)) out[, scenario := scenario]
  out[, scenario := scenario]

  out[]
}

projection_raw <- data.table::rbindlist(
  lapply(scenario_names, read_scenario_file, output_type = "raw"),
  use.names = TRUE,
  fill = TRUE
)

projection_length <- data.table::rbindlist(
  lapply(scenario_names, read_scenario_file, output_type = "length"),
  use.names = TRUE,
  fill = TRUE
)

check_required_cols(
  projection_raw,
  c(
    "scenario", "iteration", "region", "mode", "month",
    "metric", "projected_value"
  ),
  "Combined raw projection output"
)

check_required_cols(
  projection_length,
  c(
    "scenario", "iteration", "region", "mode", "species",
    "length_bin_in", "harvest_n", "harvest_weight_lb",
    "released_n", "released_weight_lb", "dead_discard_n",
    "dead_discard_weight_lb"
  ),
  "Combined at-length projection output"
)

projection_raw[, area := if (spatial_level == "state") {
  substr(region, 1, 2)
} else {
  region
}]

projection_length[, area := if (spatial_level == "state") {
  substr(region, 1, 2)
} else {
  region
}]

# -----------------------------------------------------------------------------
# 4. Convert raw output to biological outcomes
# -----------------------------------------------------------------------------

raw_biological <- projection_raw[
  mode == "all modes" & grepl("^tot_(keep|rel)_(str|blu)", metric),
  .(
    scenario,
    iteration,
    region,
    area,
    month,
    species = data.table::fcase(
      grepl("_str_", metric), "striper",
      grepl("_blu_", metric), "bluefish",
      default = NA_character_
    ),
    component = data.table::fcase(
      grepl("^tot_keep_", metric), "Harvest",
      grepl("^tot_rel_", metric), "Discards",
      default = NA_character_
    ),
    unit = data.table::fifelse(
      grepl("weight_lb", metric),
      "Weight (lb)",
      "Numbers"
    ),
    value = projected_value
  )
]

raw_biological <- raw_biological[
  !is.na(species) & !is.na(component)
]

# Sum months and management regions to the requested geographic level within
# each simulation draw.
regional_component_draw <- raw_biological[
  ,
  .(value = sum(value, na.rm = TRUE)),
  by = .(scenario, iteration, area, species, unit, component)
]

coast_component_draw <- raw_biological[
  ,
  .(value = sum(value, na.rm = TRUE)),
  by = .(scenario, iteration, species, unit, component)
]

add_total_mortality <- function(component_data, geography_cols = character()) {
  id_cols <- c("scenario", "iteration", geography_cols, "species", "unit")

  wide <- data.table::dcast(
    component_data,
    stats::as.formula(
      paste(paste(id_cols, collapse = " + "), "~ component")
    ),
    value.var = "value",
    fill = 0
  )

  wide[, dead_discard := Discards * discard_mortality_rates[species]]
  wide[, `Total mortality` := Harvest + dead_discard]

  biological_long <- data.table::melt(
    wide,
    id.vars = id_cols,
    measure.vars = c("Harvest", "Discards", "Total mortality"),
    variable.name = "outcome",
    value.name = "value"
  )

  biological_long[, outcome := factor(
    outcome,
    levels = c("Harvest", "Discards", "Total mortality")
  )]

  biological_long[]
}

regional_biological_draw <- add_total_mortality(
  regional_component_draw,
  geography_cols = "area"
)

coast_biological_draw <- add_total_mortality(coast_component_draw)

regional_biological_summary <- summarize_draws(
  regional_biological_draw,
  c("scenario", "area", "species", "unit", "outcome")
)

coast_biological_summary <- summarize_draws(
  coast_biological_draw,
  c("scenario", "species", "unit", "outcome")
)

regional_biological_summary <- add_display_labels(regional_biological_summary)
coast_biological_summary <- add_display_labels(coast_biological_summary)

# save_table(
#   regional_biological_summary,
#   paste0("projection_biological_summary_by_", spatial_level, ".csv")
# )
#
# save_table(
#   coast_biological_summary,
#   "projection_biological_summary_coastwide.csv"
# )

# -----------------------------------------------------------------------------
# 5. Predicted trips and compensating variation
# -----------------------------------------------------------------------------
# CV in the saved projection is expansion-weighted. Summing it produces
# aggregate compensating variation over the modeled choice occasions.

socioeconomic_raw <- projection_raw[
  mode == "all modes" & metric %in% c("n_trips_alt", "CV"),
  .(
    scenario,
    iteration,
    region,
    area,
    month,
    measure = data.table::fcase(
      metric == "n_trips_alt", "Predicted trips",
      metric == "CV", "Aggregate CV ($)",
      default = NA_character_
    ),
    value = projected_value
  )
]

regional_socioeconomic_draw <- socioeconomic_raw[
  ,
  .(value = sum(value, na.rm = TRUE)),
  by = .(scenario, iteration, area, measure)
]

coast_socioeconomic_draw <- socioeconomic_raw[
  ,
  .(value = sum(value, na.rm = TRUE)),
  by = .(scenario, iteration, measure)
]

regional_socioeconomic_summary <- summarize_draws(
  regional_socioeconomic_draw,
  c("scenario", "area", "measure")
)

coast_socioeconomic_summary <- summarize_draws(
  coast_socioeconomic_draw,
  c("scenario", "measure")
)

regional_socioeconomic_summary <- add_display_labels(
  regional_socioeconomic_summary
)

coast_socioeconomic_summary <- add_display_labels(
  coast_socioeconomic_summary
)

# save_table(
#   regional_socioeconomic_summary,
#   paste0("projection_trips_cv_summary_by_", spatial_level, ".csv")
# )
#
# save_table(
#   coast_socioeconomic_summary,
#   "projection_trips_cv_summary_coastwide.csv"
# )

# -----------------------------------------------------------------------------
# 6. Coastwide plots
# -----------------------------------------------------------------------------

coast_numbers_plot <- ggplot2::ggplot(
  coast_biological_summary[unit == "Numbers"],
  ggplot2::aes(
    x = scenario_label,
    y = median,
    ymin = lower,
    ymax = upper,
    group = scenario_label
  )
) +
  ggplot2::geom_errorbar(width = 0.15) +
  ggplot2::geom_point(size = 2.4) +
  ggplot2::facet_grid(
    species_label ~ outcome,
    scales = "free_y"
  ) +
  short_number_scale() +
  ggplot2::labs(
    x = NULL,
    y = "Number of fish",
    title = "Coastwide recreational fishery outcomes in numbers",
    subtitle = "Points are medians; intervals are 5th–95th percentiles across simulation draws"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
    panel.grid.minor = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank()
  )

coast_weight_plot <- ggplot2::ggplot(
  coast_biological_summary[unit == "Weight (lb)"],
  ggplot2::aes(
    x = scenario_label,
    y = median,
    ymin = lower,
    ymax = upper,
    group = scenario_label
  )
) +
  ggplot2::geom_errorbar(width = 0.15) +
  ggplot2::geom_point(size = 2.4) +
  ggplot2::facet_grid(
    species_label ~ outcome,
    scales = "free_y"
  ) +
  short_number_scale() +
  ggplot2::labs(
    x = NULL,
    y = "Weight (lb)",
    title = "Coastwide recreational fishery outcomes in weight",
    subtitle = "Points are medians; intervals are 5th–95th percentiles across simulation draws"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
    panel.grid.minor = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank()
  )

coast_trips_cv_plot <- ggplot2::ggplot(
  coast_socioeconomic_summary,
  ggplot2::aes(
    x = scenario_label,
    y = median,
    ymin = lower,
    ymax = upper,
    group = scenario_label
  )
) +
  ggplot2::geom_errorbar(width = 0.15) +
  ggplot2::geom_point(size = 2.4) +
  ggplot2::facet_wrap(~ measure, scales = "free_y", ncol = 1) +
  short_number_scale() +
  ggplot2::labs(
    x = NULL,
    y = NULL,
    title = "Coastwide predicted trips and compensating variation",
    subtitle = "CV is aggregate across modeled choice occasions"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
    panel.grid.minor = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank()
  )

# save_plot(coast_numbers_plot, "coastwide_biological_numbers.png", 13, 7)
# save_plot(coast_weight_plot, "coastwide_biological_weight.png", 13, 7)
# save_plot(coast_trips_cv_plot, "coastwide_trips_and_cv.png", 10, 8)

# -----------------------------------------------------------------------------
# 7. State- or region-level plots
# -----------------------------------------------------------------------------

regional_numbers_plot <- ggplot2::ggplot(
  regional_biological_summary[unit == "Numbers"],
  ggplot2::aes(
    x = area,
    y = median,
    ymin = lower,
    ymax = upper,
    group = scenario_label,
    shape = scenario_label
  )
) +
  ggplot2::geom_errorbar(
    position = ggplot2::position_dodge(width = 0.65),
    width = 0.15
  ) +
  ggplot2::geom_point(
    position = ggplot2::position_dodge(width = 0.65),
    size = 2
  ) +
  ggplot2::facet_grid(
    species_label ~ outcome,
    scales = "free_y"
  ) +
  short_number_scale() +
  ggplot2::labs(
    x = if (spatial_level == "state") "State" else "Management region",
    y = "Number of fish",
    shape = NULL,
    title = paste0(
      "Recreational fishery outcomes in numbers by ",
      spatial_level
    ),
    subtitle = "Points are medians; intervals are 5th–95th percentiles across simulation draws"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.grid.minor = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank()
  ) +
  ggplot2::guides(shape = ggplot2::guide_legend(nrow = 2, byrow = TRUE))

regional_weight_plot <- ggplot2::ggplot(
  regional_biological_summary[unit == "Weight (lb)"],
  ggplot2::aes(
    x = area,
    y = median,
    ymin = lower,
    ymax = upper,
    group = scenario_label,
    shape = scenario_label
  )
) +
  ggplot2::geom_errorbar(
    position = ggplot2::position_dodge(width = 0.65),
    width = 0.15
  ) +
  ggplot2::geom_point(
    position = ggplot2::position_dodge(width = 0.65),
    size = 2
  ) +
  ggplot2::facet_grid(
    species_label ~ outcome,
    scales = "free_y"
  ) +
  short_number_scale() +
  ggplot2::labs(
    x = if (spatial_level == "state") "State" else "Management region",
    y = "Weight (lb)",
    shape = NULL,
    title = paste0(
      "Recreational fishery outcomes in weight by ",
      spatial_level
    ),
    subtitle = "Points are medians; intervals are 5th–95th percentiles across simulation draws"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.grid.minor = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank()
  ) +
  ggplot2::guides(shape = ggplot2::guide_legend(nrow = 2, byrow = TRUE))

regional_trips_cv_plot <- ggplot2::ggplot(
  regional_socioeconomic_summary,
  ggplot2::aes(
    x = area,
    y = median,
    ymin = lower,
    ymax = upper,
    group = scenario_label,
    shape = scenario_label
  )
) +
  ggplot2::geom_errorbar(
    position = ggplot2::position_dodge(width = 0.65),
    width = 0.15
  ) +
  ggplot2::geom_point(
    position = ggplot2::position_dodge(width = 0.65),
    size = 2
  ) +
  ggplot2::facet_wrap(~ measure, scales = "free_y", ncol = 1) +
  short_number_scale() +
  ggplot2::labs(
    x = if (spatial_level == "state") "State" else "Management region",
    y = NULL,
    shape = NULL,
    title = paste0("Predicted trips and CV by ", spatial_level),
    subtitle = "CV is aggregate across modeled choice occasions"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.grid.minor = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank()
  ) +
  ggplot2::guides(shape = ggplot2::guide_legend(nrow = 2, byrow = TRUE))

# save_plot(
#   regional_numbers_plot,
#   paste0(spatial_level, "_biological_numbers.png"),
#   16,
#   8
# )
#
# save_plot(
#   regional_weight_plot,
#   paste0(spatial_level, "_biological_weight.png"),
#   16,
#   8
# )
#
# save_plot(
#   regional_trips_cv_plot,
#   paste0(spatial_level, "_trips_and_cv.png"),
#   15,
#   9
# )

# -----------------------------------------------------------------------------
# 8. Coastwide outcomes at length in two-inch bins
# -----------------------------------------------------------------------------
# Use only all-modes rows to avoid double-counting the individual mode rows.

length_draw_2in <- projection_length[
  mode == "all modes",
  .(
    harvest_n = sum(harvest_n, na.rm = TRUE),
    harvest_weight_lb = sum(harvest_weight_lb, na.rm = TRUE),
    released_n = sum(released_n, na.rm = TRUE),
    released_weight_lb = sum(released_weight_lb, na.rm = TRUE),
    dead_discard_n = sum(dead_discard_n, na.rm = TRUE),
    dead_discard_weight_lb = sum(dead_discard_weight_lb, na.rm = TRUE)
  ),
  by = .(
    scenario,
    iteration,
    species,
    length_bin_in = floor(length_bin_in / 2) * 2
  )
]

# Complete missing bins within each scenario/species/draw as structural zeros.
length_limits <- length_draw_2in[
  ,
  .(
    minimum_bin = min(length_bin_in, na.rm = TRUE),
    maximum_bin = max(length_bin_in, na.rm = TRUE)
  ),
  by = .(scenario, species)
]

length_bin_grid <- length_limits[
  ,
  .(length_bin_in = seq(minimum_bin, maximum_bin, by = 2)),
  by = .(scenario, species)
]

length_draw_grid <- unique(
  length_draw_2in[, .(scenario, iteration, species)]
)

complete_length_grid <- merge(
  length_draw_grid,
  length_bin_grid,
  by = c("scenario", "species"),
  allow.cartesian = TRUE
)

length_draw_2in <- merge(
  complete_length_grid,
  length_draw_2in,
  by = c("scenario", "iteration", "species", "length_bin_in"),
  all.x = TRUE
)

length_value_cols <- c(
  "harvest_n", "harvest_weight_lb",
  "released_n", "released_weight_lb",
  "dead_discard_n", "dead_discard_weight_lb"
)

length_draw_2in[
  ,
  (length_value_cols) := lapply(.SD, function(x) {
    data.table::fifelse(is.na(x), 0, x)
  }),
  .SDcols = length_value_cols
]

length_draw_2in[, `:=`(
  length_bin_upper_in = length_bin_in + 2,
  length_bin_mid_in = length_bin_in + 1,
  total_mortality_n = harvest_n + dead_discard_n,
  total_mortality_weight_lb =
    harvest_weight_lb + dead_discard_weight_lb
)]

length_outcomes_draw <- data.table::rbindlist(
  list(
    length_draw_2in[, .(
      scenario, iteration, species,
      length_bin_in, length_bin_upper_in, length_bin_mid_in,
      outcome = "Harvest",
      unit = "Numbers",
      value = harvest_n
    )],
    length_draw_2in[, .(
      scenario, iteration, species,
      length_bin_in, length_bin_upper_in, length_bin_mid_in,
      outcome = "Discards",
      unit = "Numbers",
      value = released_n
    )],
    length_draw_2in[, .(
      scenario, iteration, species,
      length_bin_in, length_bin_upper_in, length_bin_mid_in,
      outcome = "Total mortality",
      unit = "Numbers",
      value = total_mortality_n
    )],
    length_draw_2in[, .(
      scenario, iteration, species,
      length_bin_in, length_bin_upper_in, length_bin_mid_in,
      outcome = "Harvest",
      unit = "Weight (lb)",
      value = harvest_weight_lb
    )],
    length_draw_2in[, .(
      scenario, iteration, species,
      length_bin_in, length_bin_upper_in, length_bin_mid_in,
      outcome = "Discards",
      unit = "Weight (lb)",
      value = released_weight_lb
    )],
    length_draw_2in[, .(
      scenario, iteration, species,
      length_bin_in, length_bin_upper_in, length_bin_mid_in,
      outcome = "Total mortality",
      unit = "Weight (lb)",
      value = total_mortality_weight_lb
    )]
  ),
  use.names = TRUE
)

length_outcomes_draw[, outcome := factor(
  outcome,
  levels = c("Harvest", "Discards", "Total mortality")
)]

length_outcomes_summary <- summarize_draws(
  length_outcomes_draw,
  c(
    "scenario", "species", "length_bin_in", "length_bin_upper_in",
    "length_bin_mid_in", "outcome", "unit"
  )
)

length_outcomes_summary <- add_display_labels(length_outcomes_summary)

# save_table(
#   length_outcomes_summary,
#   "projection_at_length_2inch_summary_coastwide.csv"
# )

length_numbers_plot <- ggplot2::ggplot(
  length_outcomes_summary[unit == "Numbers"],
  ggplot2::aes(
    x = length_bin_mid_in,
    y = median,
    ymin = lower,
    ymax = upper,
    group = scenario_label,
    linetype = scenario_label,
    shape = scenario_label
  )
) +
  ggplot2::geom_errorbar(width = 0.35, linewidth = 0.35) +
  ggplot2::geom_line(linewidth = 0.65) +
  ggplot2::geom_point(size = 1.5) +
  ggplot2::facet_grid(
    species_label ~ outcome,
    scales = "free"
  ) +
  ggplot2::scale_x_continuous(
    breaks = scales::breaks_pretty(n = 10)
  ) +
  short_number_scale() +
  ggplot2::labs(
    x = "Fish length (two-inch bin midpoint, inches)",
    y = "Number of fish",
    linetype = NULL,
    shape = NULL,
    title = "Coastwide recreational fishery outcomes at length in numbers",
    subtitle = "Points are medians; intervals are 5th–95th percentiles across simulation draws"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "bottom",
    panel.grid.minor = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank()
  ) +
  ggplot2::guides(
    linetype = ggplot2::guide_legend(nrow = 2, byrow = TRUE),
    shape = "none"
  )

length_weight_plot <- ggplot2::ggplot(
  length_outcomes_summary[unit == "Weight (lb)"],
  ggplot2::aes(
    x = length_bin_mid_in,
    y = median,
    ymin = lower,
    ymax = upper,
    group = scenario_label,
    linetype = scenario_label,
    shape = scenario_label
  )
) +
  ggplot2::geom_errorbar(width = 0.35, linewidth = 0.35) +
  ggplot2::geom_line(linewidth = 0.65) +
  ggplot2::geom_point(size = 1.5) +
  ggplot2::facet_grid(
    species_label ~ outcome,
    scales = "free"
  ) +
  ggplot2::scale_x_continuous(
    breaks = scales::breaks_pretty(n = 10)
  ) +
  short_number_scale() +
  ggplot2::labs(
    x = "Fish length (two-inch bin midpoint, inches)",
    y = "Weight (lb)",
    linetype = NULL,
    shape = NULL,
    title = "Coastwide recreational fishery outcomes at length in weight",
    subtitle = "Points are medians; intervals are 5th–95th percentiles across simulation draws"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "bottom",
    panel.grid.minor = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank()
  ) +
  ggplot2::guides(
    linetype = ggplot2::guide_legend(nrow = 2, byrow = TRUE),
    shape = "none"
  )

# save_plot(length_numbers_plot, "coastwide_at_length_numbers_2inch.png", 15, 8)
# save_plot(length_weight_plot, "coastwide_at_length_weight_2inch.png", 15, 8)

# -----------------------------------------------------------------------------
# 9. Prepare MRIP data for status-quo comparisons
# -----------------------------------------------------------------------------

standardize_mrip_bounds <- function(data) {
  out <- data.table::copy(data)

  lower_candidates <- c(
    "lower_mrip", "ll95_mrip", "l95_mrip", "lower95", "lower"
  )

  upper_candidates <- c(
    "upper_mrip", "ul95_mrip", "u95_mrip", "upper95", "upper"
  )

  lower_col <- lower_candidates[lower_candidates %in% names(out)][1]
  upper_col <- upper_candidates[upper_candidates %in% names(out)][1]

  if (is.na(lower_col)) out[, lower_mrip := NA_real_]
  if (is.na(upper_col)) out[, upper_mrip := NA_real_]

  if (!is.na(lower_col) && lower_col != "lower_mrip") {
    data.table::setnames(out, lower_col, "lower_mrip")
  }

  if (!is.na(upper_col) && upper_col != "upper_mrip") {
    data.table::setnames(out, upper_col, "upper_mrip")
  }

  out[]
}

normalize_mrip_metric <- function(metric) {
  data.table::fcase(
    grepl("harvest", metric, ignore.case = TRUE) &
      grepl("lb|weight", metric, ignore.case = TRUE),
    "harvest_weight",

    grepl("harvest", metric, ignore.case = TRUE) &
      grepl("#|number", metric, ignore.case = TRUE),
    "harvest_n",

    grepl("total mortality|dead remov", metric, ignore.case = TRUE) &
      grepl("#|number", metric, ignore.case = TRUE),
    "total_mortality_n",

    grepl("discard|release", metric, ignore.case = TRUE) &
      grepl("#|number", metric, ignore.case = TRUE),
    "discard_n",

    default = NA_character_
  )
}

prepare_mrip_comparison <- function(file_path, geographic_level) {
  if (!file.exists(file_path)) {
    warning("Skipping MRIP comparison; file not found: ", file_path)
    return(data.table::data.table())
  }

  mrip <- data.table::as.data.table(haven::read_dta(file_path))
  mrip <- standardize_mrip_bounds(mrip)

  check_required_cols(
    mrip,
    c("species", "metric", "total_mrip"),
    basename(file_path)
  )

  mrip[, metric_standard := normalize_mrip_metric(metric)]
  mrip <- mrip[!is.na(metric_standard)]

  if (geographic_level == "state") {
    check_required_cols(mrip, "state", basename(file_path))
    geography_cols <- "state"
  } else {
    geography_cols <- character()
  }

  mrip_aggregated <- mrip[
    ,
    .(
      estimate = sum(total_mrip, na.rm = TRUE),
      lower = if (all(is.na(lower_mrip))) {
        NA_real_
      } else {
        sum(lower_mrip, na.rm = TRUE)
      },
      upper = if (all(is.na(upper_mrip))) {
        NA_real_
      } else {
        sum(upper_mrip, na.rm = TRUE)
      }
    ),
    by = c(geography_cols, "species", "metric_standard")
  ]

  id_cols <- c(geography_cols, "species")

  mrip_wide <- data.table::dcast(
    mrip_aggregated,
    stats::as.formula(
      paste(paste(id_cols, collapse = " + "), "~ metric_standard")
    ),
    value.var = c("estimate", "lower", "upper")
  )

  needed_cols <- c(
    "estimate_harvest_n", "estimate_harvest_weight",
    "estimate_discard_n", "estimate_total_mortality_n",
    "lower_harvest_n", "lower_harvest_weight",
    "lower_discard_n", "lower_total_mortality_n",
    "upper_harvest_n", "upper_harvest_weight",
    "upper_discard_n", "upper_total_mortality_n"
  )

  for (column in setdiff(needed_cols, names(mrip_wide))) {
    mrip_wide[, (column) := NA_real_]
  }

  # Derive total mortality when the input supplies harvest and all discards but
  # not a direct total-mortality estimate. Bounds derived this way are only
  # approximate because covariance between MRIP estimates is unavailable.
  mrip_wide[
    is.na(estimate_total_mortality_n),
    estimate_total_mortality_n :=
      estimate_harvest_n +
      estimate_discard_n * discard_mortality_rates[species]
  ]

  mrip_wide[
    is.na(lower_total_mortality_n),
    lower_total_mortality_n :=
      lower_harvest_n + lower_discard_n * discard_mortality_rates[species]
  ]

  mrip_wide[
    is.na(upper_total_mortality_n),
    upper_total_mortality_n :=
      upper_harvest_n + upper_discard_n * discard_mortality_rates[species]
  ]

  make_mrip_outcome <- function(outcome_name, unit_name,
                                estimate_col, lower_col, upper_col) {
    ans <- mrip_wide[, ..id_cols]
    ans[, `:=`(
      outcome = outcome_name,
      unit = unit_name,
      estimate = mrip_wide[[estimate_col]],
      lower = mrip_wide[[lower_col]],
      upper = mrip_wide[[upper_col]]
    )]
    ans[]
  }

  output_list <- list(
    make_mrip_outcome(
      "Harvest", "Numbers",
      "estimate_harvest_n", "lower_harvest_n", "upper_harvest_n"
    ),
    make_mrip_outcome(
      "Harvest", "Weight (lb)",
      "estimate_harvest_weight", "lower_harvest_weight",
      "upper_harvest_weight"
    ),
    make_mrip_outcome(
      "Total mortality", "Numbers",
      "estimate_total_mortality_n", "lower_total_mortality_n",
      "upper_total_mortality_n"
    )
  )

  out <- data.table::rbindlist(output_list, use.names = TRUE, fill = TRUE)
  out[, source := "MRIP"]
  out[]
}

mrip_state_comparison <- prepare_mrip_comparison(
  file.path(input_data_cd, "mrip_catch_state.dta"),
  geographic_level = "state"
)

mrip_coast_comparison <- prepare_mrip_comparison(
  file.path(input_data_cd, "mrip_catch_coast.dta"),
  geographic_level = "coast"
)

# -----------------------------------------------------------------------------
# 10. Status-quo simulation versus MRIP plots
# -----------------------------------------------------------------------------

status_quo_state_sim <- regional_biological_summary[
  scenario == "status_quo_2023" &
    spatial_level == "state" &
    (
      (outcome == "Harvest" & unit %in% c("Numbers", "Weight (lb)")) |
      (outcome == "Total mortality" & unit == "Numbers")
    ),
  .(
    state = area,
    species,
    outcome = as.character(outcome),
    unit,
    source = "Simulation",
    estimate = median,
    lower,
    upper
  )
]

status_quo_coast_sim <- coast_biological_summary[
  scenario == "status_quo_2023" &
    (
      (outcome == "Harvest" & unit %in% c("Numbers", "Weight (lb)")) |
      (outcome == "Total mortality" & unit == "Numbers")
    ),
  .(
    species,
    outcome = as.character(outcome),
    unit,
    source = "Simulation",
    estimate = median,
    lower,
    upper
  )
]

if (spatial_level == "state" && nrow(mrip_state_comparison) > 0L) {
  state_mrip_plot_data <- data.table::rbindlist(
    list(status_quo_state_sim, mrip_state_comparison),
    use.names = TRUE,
    fill = TRUE
  )

  state_mrip_plot_data[, `:=`(
    source = factor(source, levels = c("MRIP", "Simulation")),
    species_label = factor(
      species_labels[species],
      levels = unname(species_labels[c("striper", "bluefish")])
    ),
    panel = factor(
      paste(outcome, unit, sep = " — "),
      levels = c(
        "Harvest — Numbers",
        "Harvest — Weight (lb)",
        "Total mortality — Numbers"
      )
    )
  )]

  state_mrip_plot <- ggplot2::ggplot(
    state_mrip_plot_data,
    ggplot2::aes(
      x = state,
      y = estimate,
      ymin = lower,
      ymax = upper,
      group = source,
      shape = source
    )
  ) +
    ggplot2::geom_errorbar(
      position = ggplot2::position_dodge(width = 0.55),
      width = 0.16
    ) +
    ggplot2::geom_point(
      position = ggplot2::position_dodge(width = 0.55),
      size = 2.3
    ) +
    ggplot2::facet_grid(
      species_label ~ panel,
      scales = "free_y"
    ) +
    short_number_scale() +
    ggplot2::labs(
      x = "State",
      y = NULL,
      shape = NULL,
      title = "Status-quo simulation compared with MRIP by state",
      subtitle = paste0(
        "Simulation intervals are 5th–95th percentiles across draws; ",
        "MRIP intervals use reported bounds"
      )
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank()
    )

  # save_plot(
  #   state_mrip_plot,
  #   "status_quo_simulation_vs_mrip_by_state.png",
  #   15,
  #   8
  # )

  # save_table(
  #   state_mrip_plot_data,
  #   "status_quo_simulation_vs_mrip_by_state.csv"
  # )
}

if (nrow(mrip_coast_comparison) > 0L) {
  coast_mrip_plot_data <- data.table::rbindlist(
    list(status_quo_coast_sim, mrip_coast_comparison),
    use.names = TRUE,
    fill = TRUE
  )

  coast_mrip_plot_data[, `:=`(
    source = factor(source, levels = c("MRIP", "Simulation")),
    species_label = factor(
      species_labels[species],
      levels = unname(species_labels[c("striper", "bluefish")])
    ),
    panel = factor(
      paste(outcome, unit, sep = " — "),
      levels = c(
        "Harvest — Numbers",
        "Harvest — Weight (lb)",
        "Total mortality — Numbers"
      )
    )
  )]

  coast_mrip_plot <- ggplot2::ggplot(
    coast_mrip_plot_data,
    ggplot2::aes(
      x = species_label,
      y = estimate,
      ymin = lower,
      ymax = upper,
      group = source,
      shape = source
    )
  ) +
    ggplot2::geom_errorbar(
      position = ggplot2::position_dodge(width = 0.5),
      width = 0.15
    ) +
    ggplot2::geom_point(
      position = ggplot2::position_dodge(width = 0.5),
      size = 2.8
    ) +
    ggplot2::facet_wrap(~ panel, scales = "free_y", nrow = 1) +
    short_number_scale() +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      shape = NULL,
      title = "Coastwide status-quo simulation compared with MRIP",
      subtitle = paste0(
        "Simulation intervals are 5th–95th percentiles across draws; ",
        "MRIP intervals use reported bounds"
      )
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank()
    )

  # save_plot(
  #   coast_mrip_plot,
  #   "status_quo_simulation_vs_mrip_coastwide.png",
  #   13,
  #   5.5
  # )

  # save_table(
  #   coast_mrip_plot_data,
  #   "status_quo_simulation_vs_mrip_coastwide.csv"
  # )
}

# -----------------------------------------------------------------------------
# 11. Print the main plot objects
# -----------------------------------------------------------------------------

print(coast_numbers_plot)
print(coast_weight_plot)
print(coast_trips_cv_plot)
print(regional_numbers_plot)
print(regional_weight_plot)
print(regional_trips_cv_plot)
print(length_numbers_plot)
print(length_weight_plot)

if (exists("state_mrip_plot")) print(state_mrip_plot)
if (exists("coast_mrip_plot")) print(coast_mrip_plot)

message(
  "Analysis complete. Tables saved in: ", table_output_cd,
  "\nPlots saved in: ", plot_output_cd
)
