options(scipen = 999)

# =============================================================================
# Striped bass / bluefish policy-scenario figures
#
# Main-report figures:
#   Figure 1. Effects of harvest moratoria on projected trips and aggregate CV
#   Figure 2. Coastwide harvest and total mortality in numbers
#   Figure 3. Coastwide total mortality at length (optional main-text figure)
#   Figure 4. Coastwide simulation versus MRIP, 2023 and 2024
#
# Appendix figures:
#   A1. Coastwide biological outcomes in weight
#   A2. State-level biological outcomes in numbers
#   A3. State-level projected trips and aggregate CV
#   A4. Full coastwide at-length outcomes in numbers
#   A5. Full coastwide at-length outcomes in weight
#   A6. State-level simulation versus MRIP, 2023 and 2024
#
# Notes:
#   * Simulation summaries use medians and 2.5th-97.5th percentiles across draws.
#   * Policy changes are calculated within matched simulation iterations before
#     being summarized across draws.
#   * Set flip_cv_sign below to TRUE only if the saved projection CV has the
#     opposite sign from the convention used in the report.
# =============================================================================

# -----------------------------------------------------------------------------
# 1. Packages
# -----------------------------------------------------------------------------

required_pkgs <- c("data.table", "fst", "haven", "ggplot2", "scales", "patchwork")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0L) stop("Install these packages before running the script: ", paste(missing_pkgs, collapse = ", "))

# -----------------------------------------------------------------------------
# 2. User controls
# -----------------------------------------------------------------------------

input_data_cd <- "E:/Lou_projects/striper_bluefish_RDM/input_data"
projection_output_cd <- file.path("E:/Lou_projects/striper_bluefish_RDM/process_data", "projection_outputs")
analysis_output_cd <- file.path(projection_output_cd, "policy_analysis")
plot_output_cd <- file.path(analysis_output_cd, "plots_revised")
table_output_cd <- file.path(analysis_output_cd, "tables_revised")
dir.create(plot_output_cd, recursive = TRUE, showWarnings = FALSE)
dir.create(table_output_cd, recursive = TRUE, showWarnings = FALSE)

scenario_names <- c("status_quo_2023", "moratorium_striper_2023", "moratorium_bluefish_2023", "moratorium_both_2023", "regulations_2024")
policy_scenario_order <- c("status_quo_2023", "moratorium_striper_2023", "moratorium_bluefish_2023", "moratorium_both_2023")
counterfactual_scenarios <- c("moratorium_striper_2023", "moratorium_bluefish_2023", "moratorium_both_2023")
scenario_labels <- c(status_quo_2023 = "Status quo", moratorium_striper_2023 = "Striped bass moratorium", moratorium_bluefish_2023 = "Bluefish moratorium", moratorium_both_2023 = "Both-species moratorium", regulations_2024 = "2024")
species_labels <- c(striper = "Striped bass", bluefish = "Bluefish")
spatial_level <- "state"
lower_probability <- 0.025
upper_probability <- 0.975
discard_mortality_rates <- c(striper = 0.09, bluefish = 0.094)
flip_cv_sign <- TRUE
save_outputs <- TRUE

# -----------------------------------------------------------------------------
# 3. Helpers
# -----------------------------------------------------------------------------

check_required_cols <- function(data, required, object_name) {
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0L) stop(object_name, " is missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}

safe_quantile <- function(x, probability) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) return(NA_real_)
  stats::quantile(x, probs = probability, na.rm = TRUE, names = FALSE)
}

summarize_draws <- function(data, grouping_cols, value_col = "value") {
  data[, .(mean = mean(get(value_col), na.rm = TRUE), median = stats::median(get(value_col), na.rm = TRUE), sd = stats::sd(get(value_col), na.rm = TRUE), lower = safe_quantile(get(value_col), lower_probability), upper = safe_quantile(get(value_col), upper_probability), n_draws = data.table::uniqueN(iteration[is.finite(get(value_col))])), by = grouping_cols]
}

add_display_labels <- function(data) {
  out <- data.table::copy(data)
  if ("scenario" %in% names(out)) out[, scenario_label := factor(scenario_labels[scenario], levels = unname(scenario_labels[scenario_names]))]
  if ("species" %in% names(out)) out[, species_label := factor(species_labels[species], levels = unname(species_labels[c("striper", "bluefish")]))]
  out[]
}

short_number_scale <- function(n_breaks = 10) {
  ggplot2::scale_y_continuous(
    breaks = scales::breaks_pretty(n = n_breaks),
    labels = scales::label_number(
      scale_cut = scales::cut_short_scale()
    )
  )
}

save_plot <- function(plot, filename, width, height) {
  if (!isTRUE(save_outputs)) return(invisible(NULL))
  ggplot2::ggsave(filename = file.path(plot_output_cd, filename), plot = plot, width = width, height = height, units = "in", dpi = 300)
  invisible(NULL)
}

save_table <- function(data, filename) {
  if (!isTRUE(save_outputs)) return(invisible(NULL))
  data.table::fwrite(data, file.path(table_output_cd, filename))
  invisible(NULL)
}

theme_report <- function(base_size = 12) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 13, face = "bold"),
                   plot.subtitle = ggplot2::element_text(size = 12),
                   axis.title = ggplot2::element_text(size = 12),
                   axis.text = ggplot2::element_text(size = 11),
                   strip.text = ggplot2::element_text(size = 12, face = "bold"),
                   strip.background = ggplot2::element_blank(),
                   legend.position = "bottom",
                   panel.grid.minor = ggplot2::element_blank())
}

# -----------------------------------------------------------------------------
# 4. Read scenario outputs
# -----------------------------------------------------------------------------

read_scenario_file <- function(scenario, output_type = c("raw", "length")) {
  output_type <- match.arg(output_type)
  file_prefix <- if (output_type == "raw") "str_blu_projection_raw_" else "str_blu_projection_at_length_"
  file_path <- file.path(projection_output_cd, paste0(file_prefix, scenario, ".fst"))
  if (!file.exists(file_path)) stop("Missing projection output: ", file_path, call. = FALSE)
  out <- data.table::as.data.table(fst::read_fst(file_path))
  out[, scenario := scenario]
  out[]
}

projection_raw <- data.table::rbindlist(lapply(scenario_names, read_scenario_file, output_type = "raw"), use.names = TRUE, fill = TRUE)
projection_length <- data.table::rbindlist(lapply(scenario_names, read_scenario_file, output_type = "length"), use.names = TRUE, fill = TRUE)
check_required_cols(projection_raw, c("scenario", "iteration", "region", "mode", "month", "metric", "projected_value"), "Combined raw projection output")
check_required_cols(projection_length, c("scenario", "iteration", "region", "mode", "species", "length_bin_in", "harvest_n", "harvest_weight_lb", "released_n", "released_weight_lb", "dead_discard_n", "dead_discard_weight_lb"), "Combined at-length projection output")
projection_raw[, area := if (spatial_level == "state") substr(region, 1, 2) else region]
projection_length[, area := if (spatial_level == "state") substr(region, 1, 2) else region]

# -----------------------------------------------------------------------------
# 5. Biological outcomes
# -----------------------------------------------------------------------------

raw_biological <- projection_raw[mode == "all modes" & grepl("^tot_(keep|rel)_(str|blu)", metric), .(scenario, iteration, region, area, month, species = data.table::fcase(grepl("_str_", metric), "striper", grepl("_blu_", metric), "bluefish", default = NA_character_), component = data.table::fcase(grepl("^tot_keep_", metric), "Harvest", grepl("^tot_rel_", metric), "Discards", default = NA_character_), unit = data.table::fifelse(grepl("weight_lb", metric), "Weight (lb)", "Numbers"), value = projected_value)][!is.na(species) & !is.na(component)]

regional_component_draw <- raw_biological[, .(value = sum(value, na.rm = TRUE)), by = .(scenario, iteration, area, species, unit, component)]
coast_component_draw <- raw_biological[, .(value = sum(value, na.rm = TRUE)), by = .(scenario, iteration, species, unit, component)]

add_biological_outcomes <- function(component_data, geography_cols = character()) {
  id_cols <- c("scenario", "iteration", geography_cols, "species", "unit")
  wide <- data.table::dcast(component_data, stats::as.formula(paste(paste(id_cols, collapse = " + "), "~ component")), value.var = "value", fill = 0)
  if (!"Harvest" %in% names(wide)) wide[, Harvest := 0]
  if (!"Discards" %in% names(wide)) wide[, Discards := 0]
  wide[, `:=`(Catch = Harvest + Discards, dead_discard = Discards * discard_mortality_rates[species])]
  wide[, `Total mortality` := Harvest + dead_discard]
  long <- data.table::melt(wide, id.vars = id_cols, measure.vars = c("Harvest", "Discards", "Catch", "Total mortality"), variable.name = "outcome", value.name = "value")
  long[, outcome := factor(outcome, levels = c("Harvest", "Discards", "Catch", "Total mortality"))]
  long[]
}

regional_biological_draw <- add_biological_outcomes(regional_component_draw, geography_cols = "area")
coast_biological_draw <- add_biological_outcomes(coast_component_draw)
regional_biological_summary <- add_display_labels(summarize_draws(regional_biological_draw, c("scenario", "area", "species", "unit", "outcome")))
coast_biological_summary <- add_display_labels(summarize_draws(coast_biological_draw, c("scenario", "species", "unit", "outcome")))

# -----------------------------------------------------------------------------
# 6. Projected trips and aggregate compensating variation
# -----------------------------------------------------------------------------

socioeconomic_raw <- projection_raw[mode == "all modes" & metric %in% c("n_trips_alt", "CV"), .(scenario, iteration, region, area, month,
                                                                                                measure = data.table::fcase(metric == "n_trips_alt", "Projected trips",
                                                                                                                            metric == "CV", "Aggregate CV ($)", default = NA_character_),
                                                                                                value = data.table::fifelse(metric == "CV" & isTRUE(flip_cv_sign), -projected_value, projected_value))]
regional_socioeconomic_draw <- socioeconomic_raw[, .(value = sum(value, na.rm = TRUE)), by = .(scenario, iteration, area, measure)]
coast_socioeconomic_draw <- socioeconomic_raw[, .(value = sum(value, na.rm = TRUE)), by = .(scenario, iteration, measure)]
regional_socioeconomic_summary <- add_display_labels(summarize_draws(regional_socioeconomic_draw, c("scenario", "area", "measure")))
coast_socioeconomic_summary <- add_display_labels(summarize_draws(coast_socioeconomic_draw, c("scenario", "measure")))

# -----------------------------------------------------------------------------
# 7. Paired changes from the 2023 status quo
# -----------------------------------------------------------------------------

bio_change_draw <- coast_biological_draw[scenario %in% policy_scenario_order & unit == "Numbers" & outcome %in% c("Harvest", "Total mortality")]
bio_base <- bio_change_draw[scenario == "status_quo_2023", .(iteration, species, outcome, baseline = value)]
bio_alt <- bio_change_draw[scenario %in% counterfactual_scenarios, .(scenario, iteration, species, outcome, alternative = value)]
bio_change_draw <- merge(bio_alt, bio_base, by = c("iteration", "species", "outcome"), all.x = TRUE)
bio_change_draw[, `:=`(absolute_change = alternative - baseline, percent_change = data.table::fifelse(baseline != 0, 100 * (alternative - baseline) / baseline, NA_real_))]
bio_change_summary <- bio_change_draw[, .(baseline_median = stats::median(baseline, na.rm = TRUE), alternative_median = stats::median(alternative, na.rm = TRUE), change_median = stats::median(absolute_change, na.rm = TRUE), change_lower = safe_quantile(absolute_change, lower_probability), change_upper = safe_quantile(absolute_change, upper_probability), pct_change_median = stats::median(percent_change, na.rm = TRUE), pct_change_lower = safe_quantile(percent_change, lower_probability), pct_change_upper = safe_quantile(percent_change, upper_probability)), by = .(scenario, species, outcome)]
bio_change_summary <- add_display_labels(bio_change_summary)

trip_change_draw <- coast_socioeconomic_draw[scenario %in% policy_scenario_order & measure == "Projected trips"]
trip_base <- trip_change_draw[scenario == "status_quo_2023", .(iteration, baseline = value)]
trip_alt <- trip_change_draw[scenario %in% counterfactual_scenarios, .(scenario, iteration, alternative = value)]
trip_change_draw <- merge(trip_alt, trip_base, by = "iteration", all.x = TRUE)
trip_change_draw[, `:=`(absolute_change = alternative - baseline, percent_change = data.table::fifelse(baseline != 0, 100 * (alternative - baseline) / baseline, NA_real_))]
trip_change_summary <- trip_change_draw[, .(baseline_median = stats::median(baseline, na.rm = TRUE), alternative_median = stats::median(alternative, na.rm = TRUE), change_median = stats::median(absolute_change, na.rm = TRUE), change_lower = safe_quantile(absolute_change, lower_probability), change_upper = safe_quantile(absolute_change, upper_probability), pct_change_median = stats::median(percent_change, na.rm = TRUE), pct_change_lower = safe_quantile(percent_change, lower_probability), pct_change_upper = safe_quantile(percent_change, upper_probability)), by = scenario]
trip_change_summary <- add_display_labels(trip_change_summary)
cv_summary <- coast_socioeconomic_summary[scenario %in% counterfactual_scenarios & measure == "Aggregate CV ($)"]

save_table(bio_change_summary, "policy_change_from_status_quo_biological.csv")
save_table(trip_change_summary, "policy_change_from_status_quo_trips.csv")
save_table(cv_summary, "policy_aggregate_cv.csv")

# -----------------------------------------------------------------------------
# 8. MAIN FIGURE 1: Fishing effort and welfare
# -----------------------------------------------------------------------------

trip_effect_plot <- ggplot2::ggplot(trip_change_summary, ggplot2::aes(x = factor(scenario_labels[scenario], levels = scenario_labels[counterfactual_scenarios]), y = pct_change_median, ymin = pct_change_lower, ymax = pct_change_upper)) +
  ggplot2::geom_hline(yintercept = 0, linewidth = 0.35) +
  ggplot2::geom_errorbar(width = 0.15) +
  ggplot2::geom_point(size = 2.6) +
  ggplot2::scale_y_continuous(labels = scales::label_number(suffix = "%"), breaks = scales::breaks_pretty(n = 8),) +
  ggplot2::labs(x = NULL, y = "Change in projected trips", title = "Projected fishing effort", subtitle = "Change relative to the 2023 status quo") +
  theme_report() +
  ggplot2::theme( legend.position = "none")

cv_effect_plot <- ggplot2::ggplot(cv_summary, ggplot2::aes(x = factor(scenario_labels[scenario], levels = scenario_labels[counterfactual_scenarios]), y = median, ymin = lower, ymax = upper)) +
  ggplot2::geom_hline(yintercept = 0, linewidth = 0.35) +
  ggplot2::geom_errorbar(width = 0.15) +
  ggplot2::geom_point(size = 2.6) +
  ggplot2::scale_y_continuous(labels = scales::label_dollar(scale_cut = scales::cut_short_scale(), accuracy = 1), breaks = scales::breaks_pretty(n = 8),) +
  ggplot2::labs(x = NULL, y = "Compensating variation ($)", title = "Angler welfare", subtitle = "Positive values indicate welfare losses") +
  theme_report() +
  ggplot2::theme( legend.position = "none")

policy_effort_welfare_plot <- trip_effect_plot / cv_effect_plot #+
  #patchwork::plot_annotation(
    #title = "Effects of 2023 harvest moratorium scenarios on fishing effort and angler welfare",
    #subtitle = "Points are medians; intervals are 2.5th-97.5th percentiles across paired simulation draws")
policy_effort_welfare_plot
save_plot(policy_effort_welfare_plot, "main_policy_effort_and_welfare.png", 10, 9)

# -----------------------------------------------------------------------------
# 9. MAIN FIGURE 2: Coastwide harvest and total mortality
# -----------------------------------------------------------------------------

coast_policy_numbers <- coast_biological_summary[scenario %in% policy_scenario_order & unit == "Numbers" & outcome %in% c("Harvest", "Total mortality")]
coast_policy_numbers[, scenario_label := factor(scenario_labels[scenario], levels = scenario_labels[policy_scenario_order])]

theme_report <- function(base_size = 14) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 14, face = "bold"),
                   plot.subtitle = ggplot2::element_text(size = 14),
                   axis.title = ggplot2::element_text(size = 14),
                   axis.text = ggplot2::element_text(size = 14),
                   strip.text = ggplot2::element_text(size = 14, face = "bold"),
                   strip.background = ggplot2::element_blank(),
                   legend.position = "bottom",
                   panel.grid.minor = ggplot2::element_blank(),
  legend.text = ggplot2::element_text(size = 14))

}

coast_harvest_mortality_plot <- ggplot2::ggplot(coast_policy_numbers, ggplot2::aes(x = scenario_label, y = median, ymin = lower, ymax = upper)) +
  ggplot2::geom_errorbar(width = 0.15) +
  ggplot2::geom_point(size = 2.5) +
  ggplot2::facet_grid(species_label ~ outcome, scales = "free_y") +
  short_number_scale() +
  ggplot2::labs(x = NULL, y = "Number of fish")+

#,
#                title = "Coastwide harvest and total recreational fishing mortality",
        #        subtitle = "Points are medians; intervals are 2.5th-97.5th percentiles across simulation draws") +
  theme_report() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      angle = 25,
      hjust =1,
      size=14
    )
  )
coast_harvest_mortality_plot
save_plot(coast_harvest_mortality_plot, "main_coastwide_harvest_total_mortality_numbers.png", 13, 7)

# -----------------------------------------------------------------------------
# 10. At-length summaries
# -----------------------------------------------------------------------------

length_draw_2in <- projection_length[mode == "all modes", .(harvest_n = sum(harvest_n, na.rm = TRUE), harvest_weight_lb = sum(harvest_weight_lb, na.rm = TRUE), released_n = sum(released_n, na.rm = TRUE), released_weight_lb = sum(released_weight_lb, na.rm = TRUE), dead_discard_n = sum(dead_discard_n, na.rm = TRUE), dead_discard_weight_lb = sum(dead_discard_weight_lb, na.rm = TRUE)), by = .(scenario, iteration, species, length_bin_in = floor(length_bin_in / 2) * 2)]
length_limits <- length_draw_2in[, .(minimum_bin = min(length_bin_in, na.rm = TRUE), maximum_bin = max(length_bin_in, na.rm = TRUE)), by = .(scenario, species)]
length_bin_grid <- length_limits[, .(length_bin_in = seq(minimum_bin, maximum_bin, by = 2)), by = .(scenario, species)]
length_draw_grid <- unique(length_draw_2in[, .(scenario, iteration, species)])
complete_length_grid <- merge(length_draw_grid, length_bin_grid, by = c("scenario", "species"), allow.cartesian = TRUE)
length_draw_2in <- merge(complete_length_grid, length_draw_2in, by = c("scenario", "iteration", "species", "length_bin_in"), all.x = TRUE)
length_value_cols <- c("harvest_n", "harvest_weight_lb", "released_n", "released_weight_lb", "dead_discard_n", "dead_discard_weight_lb")
length_draw_2in[, (length_value_cols) := lapply(.SD, function(x) data.table::fifelse(is.na(x), 0, x)), .SDcols = length_value_cols]
length_draw_2in[, `:=`(length_bin_upper_in = length_bin_in + 2, length_bin_mid_in = length_bin_in + 1, total_mortality_n = harvest_n + dead_discard_n, total_mortality_weight_lb = harvest_weight_lb + dead_discard_weight_lb)]

length_outcomes_draw <- data.table::rbindlist(list(
  length_draw_2in[, .(scenario, iteration, species, length_bin_in, length_bin_upper_in, length_bin_mid_in, outcome = "Harvest", unit = "Numbers", value = harvest_n)],
  length_draw_2in[, .(scenario, iteration, species, length_bin_in, length_bin_upper_in, length_bin_mid_in, outcome = "Discards", unit = "Numbers", value = released_n)],
  length_draw_2in[, .(scenario, iteration, species, length_bin_in, length_bin_upper_in, length_bin_mid_in, outcome = "Total mortality", unit = "Numbers", value = total_mortality_n)],
  length_draw_2in[, .(scenario, iteration, species, length_bin_in, length_bin_upper_in, length_bin_mid_in, outcome = "Harvest", unit = "Weight (lb)", value = harvest_weight_lb)],
  length_draw_2in[, .(scenario, iteration, species, length_bin_in, length_bin_upper_in, length_bin_mid_in, outcome = "Discards", unit = "Weight (lb)", value = released_weight_lb)],
  length_draw_2in[, .(scenario, iteration, species, length_bin_in, length_bin_upper_in, length_bin_mid_in, outcome = "Total mortality", unit = "Weight (lb)", value = total_mortality_weight_lb)]
), use.names = TRUE)
length_outcomes_draw[, outcome := factor(outcome, levels = c("Harvest", "Discards", "Total mortality"))]
length_outcomes_summary <- add_display_labels(summarize_draws(length_outcomes_draw, c("scenario", "species", "length_bin_in", "length_bin_upper_in", "length_bin_mid_in", "outcome", "unit")))

# -----------------------------------------------------------------------------
# 11. MAIN FIGURE 3: Total mortality at length
# -----------------------------------------------------------------------------

length_mortality_plot_data <- length_outcomes_summary[unit == "Numbers" & outcome == "Total mortality" & scenario %in% c(status_quo_2023)]
length_mortality_plot_data[, scenario_label := factor(scenario_labels[scenario], levels = scenario_labels[policy_scenario_order])]

length_total_mortality_plot <- ggplot2::ggplot(length_mortality_plot_data, ggplot2::aes(x = length_bin_mid_in, y = median, linetype = scenario_label, group = scenario_label)) +
  ggplot2::geom_line(linewidth = 0.85) +
  ggplot2::facet_wrap(~ species_label, scales = "free_y", ncol = 1) +
  ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n = 10)) +
  short_number_scale() +
  ggplot2::labs(x = "Fish length (two-inch bin midpoint, inches)", y = "Total mortality (number of fish)", linetype = NULL, title = "Coastwide recreational fishing mortality at length", subtitle = "Lines show median outcomes across 101 simulation iterations") +
  theme_report()
length_total_mortality_plot
save_plot(length_total_mortality_plot, "main_total_mortality_at_length.png", 11, 8)

# -----------------------------------------------------------------------------
# 12. APPENDIX FIGURE A1: Coastwide biological outcomes in weight
# -----------------------------------------------------------------------------

coast_policy_weight <- coast_biological_summary[scenario %in% policy_scenario_order & unit == "Weight (lb)"]
coast_policy_weight[, scenario_label := factor(scenario_labels[scenario], levels = scenario_labels[policy_scenario_order])]

appendix_coast_weight_plot <- ggplot2::ggplot(coast_policy_weight, ggplot2::aes(x = scenario_label, y = median, ymin = lower, ymax = upper)) +
  ggplot2::geom_errorbar(width = 0.15) + ggplot2::geom_point(size = 2.2) +
  ggplot2::facet_grid(species_label ~ outcome, scales = "free_y") + short_number_scale() +
  ggplot2::labs(x = NULL, y = "Weight (lb)", title = "Coastwide recreational fishery outcomes in weight",
                subtitle = "Points are medians; intervals are 2.5th-97.5th percentiles across simulation draws") +
  theme_report() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 28, hjust = 1))
appendix_coast_weight_plot

save_plot(appendix_coast_weight_plot, "appendix_coastwide_biological_weight.png", 14, 8)

# -----------------------------------------------------------------------------
# Coastwide MRIP versus simulation: harvest weight, 2023 and 2024
# -----------------------------------------------------------------------------

# Read the original MRIP coastwide files because prepare_mrip_comparison()
# currently retains only number-based metrics
mrip_coast_weight_2023 <- data.table::as.data.table(
  haven::read_dta(file.path(input_data_cd, "mrip_catch_coast.dta"))
)

mrip_coast_weight_2024 <- data.table::as.data.table(
  haven::read_dta(file.path(input_data_cd, "mrip_catch_coast_2024.dta"))
)

mrip_coast_weight_2023 <- standardize_mrip_bounds(mrip_coast_weight_2023)
mrip_coast_weight_2024 <- standardize_mrip_bounds(mrip_coast_weight_2024)

# Keep only MRIP harvest pounds
mrip_coast_weight_2023 <- mrip_coast_weight_2023[
  tolower(trimws(metric)) == "harvest (lbs.)",
  .(
    species,
    comparison_year = 2023L,
    source = "MRIP",
    estimate = total_mrip,
    lower = lower_mrip,
    upper = upper_mrip
  )
]

mrip_coast_weight_2024 <- mrip_coast_weight_2024[
  tolower(trimws(metric)) == "harvest (lbs.)",
  .(
    species,
    comparison_year = 2024L,
    source = "MRIP",
    estimate = total_mrip,
    lower = lower_mrip,
    upper = upper_mrip
  )
]

mrip_coast_harvest_weight <- data.table::rbindlist(
  list(
    mrip_coast_weight_2023,
    mrip_coast_weight_2024
  ),
  use.names = TRUE,
  fill = TRUE
)

# Simulation harvest weight
sim_coast_harvest_weight <- coast_biological_summary[
  scenario %in% c("status_quo_2023", "regulations_2024") &
    outcome == "Harvest" &
    unit == "Weight (lb)",
  .(
    species,
    comparison_year = data.table::fcase(
      scenario == "status_quo_2023", 2023L,
      scenario == "regulations_2024", 2024L
    ),
    source = "Simulation",
    estimate = median,
    lower,
    upper
  )
]

# Combine MRIP and simulation
coast_harvest_weight_plot_data <- data.table::rbindlist(
  list(
    mrip_coast_harvest_weight,
    sim_coast_harvest_weight
  ),
  use.names = TRUE,
  fill = TRUE
)

coast_harvest_weight_plot_data[, `:=`(
  source = factor(
    source,
    levels = c("MRIP", "Simulation")
  ),
  comparison_year = factor(
    comparison_year,
    levels = c(2023, 2024)
  ),
  species_label = factor(
    species_labels[species],
    levels = unname(species_labels[c("striper", "bluefish")])
  )
)]

# Plot
coast_harvest_weight_plot <- ggplot2::ggplot(
  coast_harvest_weight_plot_data,
  ggplot2::aes(
    x = comparison_year,
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
  ggplot2::facet_wrap(
    ~ species_label,
    scales = "free_y",
    nrow = 1
  ) +
  short_number_scale() +
  ggplot2::labs(
    x = NULL,
    y = "Harvest (lb)",
    shape = NULL
  ) +
  theme_report() +
  ggplot2::theme(
    panel.grid.major.y = ggplot2::element_line(
      colour = "grey80",
      linewidth = 0.35
    ),
    panel.grid.major.x = ggplot2::element_blank()
  )

coast_harvest_weight_plot
#
# save_plot(
#   coast_harvest_weight_plot,
#   "main_simulation_vs_mrip_harvest_weight_2023_2024_coastwide.png",
#   11,
#   5.5
# )

# -----------------------------------------------------------------------------
# State-level MRIP versus simulation: harvest weight, 2023 and 2024
# -----------------------------------------------------------------------------

# Read MRIP state files
mrip_state_weight_2023 <- data.table::as.data.table(
  haven::read_dta(file.path(input_data_cd, "mrip_catch_state.dta"))
)

mrip_state_weight_2024 <- data.table::as.data.table(
  haven::read_dta(file.path(input_data_cd, "mrip_catch_state_2024.dta"))
)

mrip_state_weight_2023 <- standardize_mrip_bounds(mrip_state_weight_2023)
mrip_state_weight_2024 <- standardize_mrip_bounds(mrip_state_weight_2024)

# Keep only MRIP harvest pounds
mrip_state_weight_2023 <- mrip_state_weight_2023[
  tolower(trimws(metric)) == "harvest (lbs.)",
  .(
    state,
    species,
    comparison_year = 2023L,
    source = "MRIP",
    estimate = total_mrip,
    lower = lower_mrip,
    upper = upper_mrip
  )
]

mrip_state_weight_2024 <- mrip_state_weight_2024[
  tolower(trimws(metric)) == "harvest (lbs.)",
  .(
    state,
    species,
    comparison_year = 2024L,
    source = "MRIP",
    estimate = total_mrip,
    lower = lower_mrip,
    upper = upper_mrip
  )
]

mrip_state_harvest_weight <- data.table::rbindlist(
  list(
    mrip_state_weight_2023,
    mrip_state_weight_2024
  ),
  use.names = TRUE,
  fill = TRUE
)

# Simulation harvest weight by state
sim_state_harvest_weight <- regional_biological_summary[
  spatial_level == "state" &
    scenario %in% c("status_quo_2023", "regulations_2024") &
    outcome == "Harvest" &
    unit == "Weight (lb)",
  .(
    state = area,
    species,
    comparison_year = data.table::fcase(
      scenario == "status_quo_2023", 2023L,
      scenario == "regulations_2024", 2024L
    ),
    source = "Simulation",
    estimate = median,
    lower,
    upper
  )
]

# Combine MRIP and simulation
state_harvest_weight_plot_data <- data.table::rbindlist(
  list(
    mrip_state_harvest_weight,
    sim_state_harvest_weight
  ),
  use.names = TRUE,
  fill = TRUE
)

state_harvest_weight_plot_data[, `:=`(
  source = factor(
    source,
    levels = c("MRIP", "Simulation")
  ),
  comparison_year = factor(
    comparison_year,
    levels = c(2023, 2024)
  ),
  species_label = factor(
    species_labels[species],
    levels = unname(species_labels[c("striper", "bluefish")])
  )
)]

# Plot
state_harvest_weight_plot <- ggplot2::ggplot(
  state_harvest_weight_plot_data,
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
    size = 2.2
  ) +
  ggplot2::facet_grid(
    rows = ggplot2::vars(comparison_year),
    cols = ggplot2::vars(species_label),
    scales = "free_y"
  ) +
  short_number_scale() +
  ggplot2::labs(
    x = "State",
    y = "Harvest (lb)",
    shape = NULL
  ) +
  theme_report() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      angle = 45,
      hjust = 1
    ),
    panel.grid.major.y = ggplot2::element_line(
      colour = "grey80",
      linewidth = 0.35
    ),
    panel.grid.major.x = ggplot2::element_blank()
  )

state_harvest_weight_plot

# save_plot(
#   state_harvest_weight_plot,
#   "appendix_simulation_vs_mrip_harvest_weight_2023_2024_by_state.png",
#   13,
#   8
# )


# -----------------------------------------------------------------------------
# 13. APPENDIX FIGURE A2: State/region biological outcomes in numbers
# -----------------------------------------------------------------------------

regional_policy_numbers <- regional_biological_summary[scenario %in% policy_scenario_order & unit == "Numbers" & outcome %in% c("Harvest", "Total mortality")]
regional_policy_numbers[, scenario_label := factor(scenario_labels[scenario], levels = scenario_labels[policy_scenario_order])]

appendix_regional_numbers_plot <- ggplot2::ggplot(regional_policy_numbers, ggplot2::aes(x = area, y = median, ymin = lower, ymax = upper, group = scenario_label, shape = scenario_label)) +
  ggplot2::geom_errorbar(position = ggplot2::position_dodge(width = 0.65), width = 0.15) +
  ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.65), size = 1.9) +
  ggplot2::facet_grid(species_label ~ outcome, scales = "free_y") + short_number_scale() +
  ggplot2::labs(x = if (spatial_level == "state") "State" else "Management region", y = "Number of fish", shape = NULL, title = paste0("Recreational harvest and total mortality by ", spatial_level), subtitle = "Points are medians; intervals are 2.5th-97.5th percentiles across simulation draws") +
  theme_report() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
  ggplot2::guides(shape = ggplot2::guide_legend(nrow = 2, byrow = TRUE))

save_plot(appendix_regional_numbers_plot, paste0("appendix_", spatial_level, "_biological_numbers.png"), 16, 8)

# -----------------------------------------------------------------------------
# 14. APPENDIX FIGURE A3: State/region trips and CV
# -----------------------------------------------------------------------------

regional_policy_socioeconomic <- regional_socioeconomic_summary[scenario %in% policy_scenario_order]
regional_policy_socioeconomic[, scenario_label := factor(scenario_labels[scenario], levels = scenario_labels[policy_scenario_order])]

appendix_regional_trips_cv_plot <- ggplot2::ggplot(regional_policy_socioeconomic, ggplot2::aes(x = area, y = median, ymin = lower, ymax = upper, group = scenario_label, shape = scenario_label)) +
  ggplot2::geom_errorbar(position = ggplot2::position_dodge(width = 0.65), width = 0.15) +
  ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.65), size = 1.9) +
  ggplot2::facet_wrap(~ measure, scales = "free_y", ncol = 1) + short_number_scale() +
  ggplot2::labs(x = if (spatial_level == "state") "State" else "Management region", y = NULL, shape = NULL, title = paste0("Projected trips and compensating variation by ", spatial_level), subtitle = "Points are medians; intervals are 2.5th-97.5th percentiles across simulation draws") +
  theme_report() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
  ggplot2::guides(shape = ggplot2::guide_legend(nrow = 2, byrow = TRUE))

save_plot(appendix_regional_trips_cv_plot, paste0("appendix_", spatial_level, "_trips_cv.png"), 15, 9)

# -----------------------------------------------------------------------------
# 15. APPENDIX FIGURES A4-A5: Full at-length outcomes
# -----------------------------------------------------------------------------

length_policy_all <- length_outcomes_summary[scenario %in% policy_scenario_order]
length_policy_all[, scenario_label := factor(scenario_labels[scenario], levels = scenario_labels[policy_scenario_order])]

appendix_length_numbers_plot <- ggplot2::ggplot(length_policy_all[unit == "Numbers"], ggplot2::aes(x = length_bin_mid_in, y = median, ymin = lower, ymax = upper, group = scenario_label, linetype = scenario_label)) +
  ggplot2::geom_errorbar(width = 0.35, linewidth = 0.3) + ggplot2::geom_line(linewidth = 0.65) +
  ggplot2::facet_grid(species_label ~ outcome, scales = "free") + ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n = 10)) + short_number_scale() +
  ggplot2::labs(x = "Fish length (two-inch bin midpoint, inches)", y = "Number of fish", linetype = NULL, title = "Coastwide recreational fishery outcomes at length in numbers", subtitle = "Lines are medians; intervals are 2.5th-97.5th percentiles across simulation draws") +
  theme_report() + ggplot2::guides(linetype = ggplot2::guide_legend(nrow = 2, byrow = TRUE))

appendix_length_weight_plot <- ggplot2::ggplot(length_policy_all[unit == "Weight (lb)"], ggplot2::aes(x = length_bin_mid_in, y = median, ymin = lower, ymax = upper, group = scenario_label, linetype = scenario_label)) +
  ggplot2::geom_errorbar(width = 0.35, linewidth = 0.3) + ggplot2::geom_line(linewidth = 0.65) +
  ggplot2::facet_grid(species_label ~ outcome, scales = "free") + ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n = 10)) + short_number_scale() +
  ggplot2::labs(x = "Fish length (two-inch bin midpoint, inches)", y = "Weight (lb)", linetype = NULL, title = "Coastwide recreational fishery outcomes at length in weight", subtitle = "Lines are medians; intervals are 2.5th-97.5th percentiles across simulation draws") +
  theme_report() + ggplot2::guides(linetype = ggplot2::guide_legend(nrow = 2, byrow = TRUE))

save_plot(appendix_length_numbers_plot, "appendix_at_length_numbers_2inch.png", 15, 8)
save_plot(appendix_length_weight_plot, "appendix_at_length_weight_2inch.png", 15, 8)

# -----------------------------------------------------------------------------
# 16. MRIP comparison helpers
# -----------------------------------------------------------------------------

standardize_mrip_bounds <- function(data) {
  out <- data.table::copy(data)
  lower_candidates <- c("lower_mrip", "ll95_mrip", "l95_mrip", "lower95", "lower")
  upper_candidates <- c("upper_mrip", "ul95_mrip", "u95_mrip", "upper95", "upper")
  lower_col <- lower_candidates[lower_candidates %in% names(out)][1]
  upper_col <- upper_candidates[upper_candidates %in% names(out)][1]
  if (is.na(lower_col)) out[, lower_mrip := NA_real_]
  if (is.na(upper_col)) out[, upper_mrip := NA_real_]
  if (!is.na(lower_col) && lower_col != "lower_mrip") data.table::setnames(out, lower_col, "lower_mrip")
  if (!is.na(upper_col) && upper_col != "upper_mrip") data.table::setnames(out, upper_col, "upper_mrip")
  out[]
}

normalize_mrip_metric <- function(metric) {
  data.table::fcase(
    grepl("harvest", metric, ignore.case = TRUE) & grepl("#|number", metric, ignore.case = TRUE), "harvest_n",
    grepl("total mortality|dead remov", metric, ignore.case = TRUE) & grepl("#|number", metric, ignore.case = TRUE), "total_mortality_n",
    grepl("discard|release", metric, ignore.case = TRUE) & grepl("#|number", metric, ignore.case = TRUE), "discard_n",
    grepl("catch", metric, ignore.case = TRUE) & grepl("#|number", metric, ignore.case = TRUE), "catch_n",
    default = NA_character_
  )
}

prepare_mrip_comparison <- function(file_path, geographic_level) {
  if (!file.exists(file_path)) { warning("Skipping MRIP comparison; file not found: ", file_path); return(data.table::data.table()) }
  mrip <- data.table::as.data.table(haven::read_dta(file_path))
  mrip <- standardize_mrip_bounds(mrip)
  check_required_cols(mrip, c("species", "metric", "total_mrip"), basename(file_path))
  mrip[, metric_standard := normalize_mrip_metric(metric)]
  mrip <- mrip[!is.na(metric_standard)]
  if (geographic_level == "state") { check_required_cols(mrip, "state", basename(file_path)); geography_cols <- "state" } else geography_cols <- character()
  mrip_aggregated <- mrip[, .(estimate = sum(total_mrip, na.rm = TRUE), lower = if (all(is.na(lower_mrip))) NA_real_ else sum(lower_mrip, na.rm = TRUE), upper = if (all(is.na(upper_mrip))) NA_real_ else sum(upper_mrip, na.rm = TRUE)), by = c(geography_cols, "species", "metric_standard")]
  id_cols <- c(geography_cols, "species")
  mrip_wide <- data.table::dcast(mrip_aggregated, stats::as.formula(paste(paste(id_cols, collapse = " + "), "~ metric_standard")), value.var = c("estimate", "lower", "upper"))
  needed_cols <- c("estimate_harvest_n", "estimate_discard_n", "estimate_total_mortality_n", "estimate_catch_n", "lower_harvest_n", "lower_discard_n", "lower_total_mortality_n", "lower_catch_n", "upper_harvest_n", "upper_discard_n", "upper_total_mortality_n", "upper_catch_n")
  for (cc in setdiff(needed_cols, names(mrip_wide))) mrip_wide[, (cc) := NA_real_]
  mrip_wide[is.na(estimate_total_mortality_n), estimate_total_mortality_n := estimate_harvest_n + estimate_discard_n * discard_mortality_rates[species]]
  mrip_wide[is.na(lower_total_mortality_n), lower_total_mortality_n := lower_harvest_n + lower_discard_n * discard_mortality_rates[species]]
  mrip_wide[is.na(upper_total_mortality_n), upper_total_mortality_n := upper_harvest_n + upper_discard_n * discard_mortality_rates[species]]
  make_outcome <- function(outcome_name, estimate_col, lower_col, upper_col) {
    ans <- mrip_wide[, ..id_cols]
    ans[, `:=`(outcome = outcome_name, unit = "Numbers", estimate = mrip_wide[[estimate_col]], lower = mrip_wide[[lower_col]], upper = mrip_wide[[upper_col]], source = "MRIP")]
    ans[]
  }
  data.table::rbindlist(list(
    make_outcome("Harvest", "estimate_harvest_n", "lower_harvest_n", "upper_harvest_n"),
    make_outcome("Total mortality", "estimate_total_mortality_n", "lower_total_mortality_n", "upper_total_mortality_n"),
    make_outcome("Catch", "estimate_catch_n", "lower_catch_n", "upper_catch_n")
  ), use.names = TRUE, fill = TRUE)
}

mrip_state_2023 <- prepare_mrip_comparison(file.path(input_data_cd, "mrip_catch_state.dta"), geographic_level = "state")
mrip_coast_2023 <- prepare_mrip_comparison(file.path(input_data_cd, "mrip_catch_coast.dta"), geographic_level = "coast")
mrip_state_2024 <- prepare_mrip_comparison(file.path(input_data_cd, "mrip_catch_state_2024.dta"), geographic_level = "state")
mrip_coast_2024 <- prepare_mrip_comparison(file.path(input_data_cd, "mrip_catch_coast_2024.dta"), geographic_level = "coast")

mrip_state_comparison <- data.table::rbindlist(list(mrip_state_2023[, `:=`(comparison_year = 2023L, scenario = "status_quo_2023")], mrip_state_2024[, `:=`(comparison_year = 2024L, scenario = "regulations_2024")]), use.names = TRUE, fill = TRUE)
mrip_coast_comparison <- data.table::rbindlist(list(mrip_coast_2023[, `:=`(comparison_year = 2023L, scenario = "status_quo_2023")], mrip_coast_2024[, `:=`(comparison_year = 2024L, scenario = "regulations_2024")]), use.names = TRUE, fill = TRUE)

# -----------------------------------------------------------------------------
# 17. Simulation versus MRIP data
# -----------------------------------------------------------------------------

comparison_scenarios <- c("status_quo_2023", "regulations_2024")
state_sim_comparison <- regional_biological_summary[scenario %in% comparison_scenarios & spatial_level == "state" & unit == "Numbers" & outcome %in% c("Harvest", "Total mortality", "Catch"), .(state = area, scenario, comparison_year = data.table::fcase(scenario == "status_quo_2023", 2023L, scenario == "regulations_2024", 2024L), species, outcome = as.character(outcome), unit, source = "Simulation", estimate = median, lower, upper)]
coast_sim_comparison <- coast_biological_summary[scenario %in% comparison_scenarios & unit == "Numbers" & outcome %in% c("Harvest", "Total mortality", "Catch"), .(scenario, comparison_year = data.table::fcase(scenario == "status_quo_2023", 2023L, scenario == "regulations_2024", 2024L), species, outcome = as.character(outcome), unit, source = "Simulation", estimate = median, lower, upper)]

# -----------------------------------------------------------------------------
# 18. MAIN FIGURE 4: Coastwide simulation versus MRIP
# -----------------------------------------------------------------------------
theme_report <- function(base_size = 14) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 14, face = "bold"),
                   plot.subtitle = ggplot2::element_text(size = 14),
                   axis.title = ggplot2::element_text(size = 14),
                   axis.text = ggplot2::element_text(size = 14),
                   strip.text = ggplot2::element_text(size = 14, face = "bold"),
                   strip.background = ggplot2::element_blank(),
                   legend.position = "bottom",
                   panel.grid.minor = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size = 14))

}
if (nrow(mrip_coast_comparison) > 0L) {

  coast_mrip_plot_data <- data.table::rbindlist(
    list(coast_sim_comparison, mrip_coast_comparison),
    use.names = TRUE,
    fill = TRUE
  )

  coast_mrip_plot_data[, `:=`(
    source = factor(
      source,
      levels = c("MRIP", "Simulation")
    ),
    comparison_year = factor(
      comparison_year,
      levels = c(2023, 2024)
    ),
    species_label = factor(
      species_labels[species],
      levels = unname(species_labels[c("striper", "bluefish")])
    ),
    panel = factor(
      outcome,
      levels = c("Harvest", "Total mortality", "Catch")
    )
  )]

  coast_mrip_plot <- ggplot2::ggplot(
    coast_mrip_plot_data,
    ggplot2::aes(
      x = comparison_year,
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
    ggplot2::facet_grid(
      rows = ggplot2::vars(panel),
      cols = ggplot2::vars(species_label),
      scales = "free_y"
    ) +
    short_number_scale() +
    ggplot2::labs(
      x = NULL,
      y = "Number of fish",
      shape = NULL
    ) +
    theme_report() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_line(
        colour = "grey80",
        linewidth = 0.35
      ),
      panel.grid.major.x = ggplot2::element_blank()
    )

  save_plot(
    coast_mrip_plot,
    "main_simulation_vs_mrip_2023_2024_coastwide.png",
    13,
    8
  )


}

coast_mrip_plot
coast_mrip_plot
# -----------------------------------------------------------------------------
# 19. APPENDIX FIGURE A6: State simulation versus MRIP
# -----------------------------------------------------------------------------

if (spatial_level == "state" && nrow(mrip_state_comparison) > 0L) {
  state_mrip_plot_data <- data.table::rbindlist(list(state_sim_comparison, mrip_state_comparison), use.names = TRUE, fill = TRUE)
  state_mrip_plot_data[, `:=`(source = factor(source, levels = c("MRIP", "Simulation")), comparison_year = factor(comparison_year, levels = c(2023, 2024)), species_label = factor(species_labels[species], levels = unname(species_labels[c("striper", "bluefish")])), panel = factor(outcome, levels = c("Harvest", "Total mortality", "Catch")))]
  state_mrip_plot <- ggplot2::ggplot(state_mrip_plot_data, ggplot2::aes(x = state, y = estimate, ymin = lower, ymax = upper, group = source, shape = source)) +
    ggplot2::geom_errorbar(position = ggplot2::position_dodge(width = 0.55), width = 0.16) +
    ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.55), size = 2.1) +
    ggplot2::facet_grid(rows = ggplot2::vars(comparison_year, species_label), cols = ggplot2::vars(panel), scales = "free_y") + short_number_scale() +
    ggplot2::labs(x = "State", y = NULL, shape = NULL, title = "Simulation compared with MRIP by state", subtitle = "2023 uses the calibrated status-quo simulation and 2024 uses the retrospective projection; simulation intervals are 2.5th-97.5th percentiles across draws") +
    theme_report() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  save_plot(state_mrip_plot, "appendix_simulation_vs_mrip_2023_2024_by_state.png", 15, 11)
  save_table(state_mrip_plot_data, "simulation_vs_mrip_2023_2024_by_state.csv")
}

# -----------------------------------------------------------------------------
# 20. Save underlying summary tables
# -----------------------------------------------------------------------------

save_table(coast_biological_summary, "projection_biological_summary_coastwide.csv")
save_table(regional_biological_summary, paste0("projection_biological_summary_by_", spatial_level, ".csv"))
save_table(coast_socioeconomic_summary, "projection_trips_cv_summary_coastwide.csv")
save_table(regional_socioeconomic_summary, paste0("projection_trips_cv_summary_by_", spatial_level, ".csv"))
save_table(length_outcomes_summary, "projection_at_length_2inch_summary_coastwide.csv")

# -----------------------------------------------------------------------------
# 21. Print main figures
# -----------------------------------------------------------------------------

print(policy_effort_welfare_plot)
print(coast_harvest_mortality_plot)
print(length_total_mortality_plot)
if (exists("coast_mrip_plot")) print(coast_mrip_plot)

message("Figure creation complete.", "\nPlots: ", plot_output_cd, "\nTables: ", table_output_cd)
