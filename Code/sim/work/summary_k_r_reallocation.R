# =============================================================================
# Summarize release-to-keep calibration adjustments across regions and draws
# =============================================================================

calib <- data.table::as.data.table(
  fst::read_fst(
    file.path(
      final_process_misc_cd,
      "calibrated_model_stats_7_9_26.fst"
    )
  )
)

# Standardize species names defensively.
calib[
  species %in% c("str", "striped_bass", "striped bass"),
  species := "striper"
]

calib[
  species %in% c("blu"),
  species := "bluefish"
]

# Check that the relevant fields exist.
release_to_keep_cols <- c(
  "region",
  "mode",
  "draw",
  "species",
  "rel_to_keep",
  "p_rel_to_keep",
  "original_rel_eligible_final",
  "n_sub_kept",
  "prop_sub_kept",
  "MRIP_keep",
  "model_keep"
)

missing_cols <- setdiff(
  release_to_keep_cols,
  names(calib)
)

if (length(missing_cols) > 0L) {
  warning(
    "These requested columns are not in the calibration file: ",
    paste(missing_cols, collapse = ", ")
  )
}

#1. Inspect where release-to-keep occurred
rel_to_keep_cells <- calib[
  rel_to_keep == 1,
  .(
    region,
    mode,
    draw,
    species,
    p_rel_to_keep,
    original_rel_eligible_final,
    n_sub_kept,
    prop_sub_kept,
    MRIP_keep,
    model_keep
  )
]

data.table::setorder(
  rel_to_keep_cells,
  species,
  region,
  mode,
  draw
)

print(rel_to_keep_cells)

rel_to_keep_cells[
  order(-p_rel_to_keep)
][1:50]

rel_to_keep_cells[
  order(-n_sub_kept)
][1:50]

#2. Summary by management unit, mode, and species
rel_to_keep_region_mode <- calib[
  ,
  .(
    n_draws = data.table::uniqueN(draw),

    # Number and share of draws using release-to-keep.
    n_draws_rel_to_keep = sum(rel_to_keep == 1, na.rm = TRUE),

    share_draws_rel_to_keep =
      mean(rel_to_keep == 1, na.rm = TRUE),

    # Average probability across all draws.
    # Draws with no release-to-keep have p = 0.
    mean_p_all_draws =
      mean(p_rel_to_keep, na.rm = TRUE),

    median_p_all_draws =
      stats::median(p_rel_to_keep, na.rm = TRUE),

    # Conditional probability among cells where release-to-keep was used.
    mean_p_when_used =
      if (any(rel_to_keep == 1, na.rm = TRUE)) {
        mean(
          p_rel_to_keep[rel_to_keep == 1],
          na.rm = TRUE
        )
      } else {
        0
      },

    median_p_when_used =
      if (any(rel_to_keep == 1, na.rm = TRUE)) {
        stats::median(
          p_rel_to_keep[rel_to_keep == 1],
          na.rm = TRUE
        )
      } else {
        0
      },

    min_p_when_used =
      if (any(rel_to_keep == 1, na.rm = TRUE)) {
        min(
          p_rel_to_keep[rel_to_keep == 1],
          na.rm = TRUE
        )
      } else {
        0
      },

    max_p_when_used =
      if (any(rel_to_keep == 1, na.rm = TRUE)) {
        max(
          p_rel_to_keep[rel_to_keep == 1],
          na.rm = TRUE
        )
      } else {
        0
      },

    # Fish-level adjustment magnitude.
    mean_eligible_releases =
      mean(original_rel_eligible_final, na.rm = TRUE),

    mean_n_converted =
      mean(n_sub_kept, na.rm = TRUE),

    median_n_converted =
      stats::median(n_sub_kept, na.rm = TRUE),

    total_n_converted_across_draw_rows =
      sum(n_sub_kept, na.rm = TRUE),

    # Directly saved realized proportion.
    mean_realized_prop =
      mean(prop_sub_kept, na.rm = TRUE),

    median_realized_prop =
      stats::median(prop_sub_kept, na.rm = TRUE)
  ),
  by = .(
    region,
    mode,
    species
  )
]

rel_to_keep_region_mode[
  ,
  `:=`(
    share_draws_rel_to_keep =
      round(share_draws_rel_to_keep, 3),

    mean_p_all_draws =
      round(mean_p_all_draws, 3),

    median_p_all_draws =
      round(median_p_all_draws, 3),

    mean_p_when_used =
      round(mean_p_when_used, 3),

    median_p_when_used =
      round(median_p_when_used, 3),

    min_p_when_used =
      round(min_p_when_used, 3),

    max_p_when_used =
      round(max_p_when_used, 3),

    mean_realized_prop =
      round(mean_realized_prop, 3),

    median_realized_prop =
      round(median_realized_prop, 3),

    mean_eligible_releases =
      round(mean_eligible_releases, 0),

    mean_n_converted =
      round(mean_n_converted, 0),

    median_n_converted =
      round(median_n_converted, 0)
  )
]

data.table::setorder(
  rel_to_keep_region_mode,
  species,
  region,
  mode
)

print(rel_to_keep_region_mode)

#3. Summary by management unit across modes
rel_to_keep_region <- calib[
  ,
  .(
    n_draws = data.table::uniqueN(draw),

    n_region_mode_draw_cells = .N,

    n_cells_rel_to_keep =
      sum(rel_to_keep == 1, na.rm = TRUE),

    share_cells_rel_to_keep =
      mean(rel_to_keep == 1, na.rm = TRUE),

    # Simple cell-level summaries.
    mean_p_all_cells =
      mean(p_rel_to_keep, na.rm = TRUE),

    median_p_all_cells =
      stats::median(p_rel_to_keep, na.rm = TRUE),

    mean_p_when_used =
      if (any(rel_to_keep == 1, na.rm = TRUE)) {
        mean(
          p_rel_to_keep[rel_to_keep == 1],
          na.rm = TRUE
        )
      } else {
        0
      },

    median_p_when_used =
      if (any(rel_to_keep == 1, na.rm = TRUE)) {
        stats::median(
          p_rel_to_keep[rel_to_keep == 1],
          na.rm = TRUE
        )
      } else {
        0
      },

    # Average fish counts across draw-mode cells.
    mean_eligible_releases =
      mean(original_rel_eligible_final, na.rm = TRUE),

    mean_n_converted =
      mean(n_sub_kept, na.rm = TRUE),

    # Pool fish over modes and draws before taking the ratio.
    pooled_realized_conversion_rate =
      data.table::fifelse(
        sum(
          original_rel_eligible_final,
          na.rm = TRUE
        ) > 0,

        sum(
          n_sub_kept,
          na.rm = TRUE
        ) /
          sum(
            original_rel_eligible_final,
            na.rm = TRUE
          ),

        0
      ),

    total_eligible_across_rows =
      sum(
        original_rel_eligible_final,
        na.rm = TRUE
      ),

    total_converted_across_rows =
      sum(
        n_sub_kept,
        na.rm = TRUE
      )
  ),
  by = .(
    region,
    species
  )
]

rel_to_keep_region[
  ,
  `:=`(
    share_cells_rel_to_keep =
      round(share_cells_rel_to_keep, 3),

    mean_p_all_cells =
      round(mean_p_all_cells, 3),

    median_p_all_cells =
      round(median_p_all_cells, 3),

    mean_p_when_used =
      round(mean_p_when_used, 3),

    median_p_when_used =
      round(median_p_when_used, 3),

    pooled_realized_conversion_rate =
      round(pooled_realized_conversion_rate, 3),

    mean_eligible_releases =
      round(mean_eligible_releases, 0),

    mean_n_converted =
      round(mean_n_converted, 0)
  )
]

data.table::setorder(
  rel_to_keep_region,
  species,
  -pooled_realized_conversion_rate
)

print(rel_to_keep_region)

data.table::fwrite(
  rel_to_keep_region,
  file.path(
    final_process_misc_cd,
    "release_to_keep_summary_by_region.csv"
  )
)

#4. Draw-level regional totals before averaging
rel_to_keep_region_draw <- calib[
  ,
  .(
    any_rel_to_keep =
      as.integer(any(rel_to_keep == 1, na.rm = TRUE)),

    eligible_releases =
      sum(
        original_rel_eligible_final,
        na.rm = TRUE
      ),

    n_converted =
      sum(
        n_sub_kept,
        na.rm = TRUE
      )
  ),
  by = .(
    region,
    draw,
    species
  )
]

rel_to_keep_region_draw[
  ,
  realized_conversion_rate :=
    data.table::fifelse(
      eligible_releases > 0,
      n_converted / eligible_releases,
      0
    )
]

rel_to_keep_region_draw_summary <- rel_to_keep_region_draw[
  ,
  .(
    n_draws =
      data.table::uniqueN(draw),

    share_draws_with_rel_to_keep =
      mean(any_rel_to_keep, na.rm = TRUE),

    mean_eligible_releases =
      mean(eligible_releases, na.rm = TRUE),

    median_eligible_releases =
      stats::median(
        eligible_releases,
        na.rm = TRUE
      ),

    mean_n_converted =
      mean(n_converted, na.rm = TRUE),

    median_n_converted =
      stats::median(
        n_converted,
        na.rm = TRUE
      ),

    mean_conversion_rate =
      mean(
        realized_conversion_rate,
        na.rm = TRUE
      ),

    median_conversion_rate =
      stats::median(
        realized_conversion_rate,
        na.rm = TRUE
      ),

    p05_conversion_rate =
      stats::quantile(
        realized_conversion_rate,
        0.05,
        na.rm = TRUE,
        names = FALSE
      ),

    p95_conversion_rate =
      stats::quantile(
        realized_conversion_rate,
        0.95,
        na.rm = TRUE,
        names = FALSE
      )
  ),
  by = .(
    region,
    species
  )
]

rate_cols <- c(
  "share_draws_with_rel_to_keep",
  "mean_conversion_rate",
  "median_conversion_rate",
  "p05_conversion_rate",
  "p95_conversion_rate"
)

rel_to_keep_region_draw_summary[
  ,
  (rate_cols) := lapply(
    .SD,
    round,
    digits = 3
  ),
  .SDcols = rate_cols
]

count_cols <- c(
  "mean_eligible_releases",
  "median_eligible_releases",
  "mean_n_converted",
  "median_n_converted"
)

rel_to_keep_region_draw_summary[
  ,
  (count_cols) := lapply(
    .SD,
    round,
    digits = 0
  ),
  .SDcols = count_cols
]

data.table::setorder(
  rel_to_keep_region_draw_summary,
  species,
  -mean_conversion_rate
)

print(rel_to_keep_region_draw_summary)

data.table::fwrite(
  rel_to_keep_region_draw_summary,
  file.path(
    final_process_misc_cd,
    "release_to_keep_summary_by_region_across_draws.csv"
  )
)

#5. Plot average conversion rates by management unit
release_to_keep_plot <- ggplot2::ggplot(
  rel_to_keep_region_draw_summary,
  ggplot2::aes(
    x = stats::reorder(
      region,
      mean_conversion_rate
    ),
    y = mean_conversion_rate
  )
) +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      ymin = p05_conversion_rate,
      ymax = p95_conversion_rate
    ),
    width = 0.2
  ) +
  ggplot2::geom_point(
    size = 2.5
  ) +
  ggplot2::facet_wrap(
    ~ species,
    scales = "free_y",
    ncol = 1
  ) +
  ggplot2::coord_flip() +
  ggplot2::scale_y_continuous(
    labels = scales::label_percent(
      accuracy = 1
    )
  ) +
  ggplot2::labs(
    x = "Management unit",
    y = "Eligible releases converted to harvest",
    title = "Release-to-keep calibration adjustments by management unit",
    subtitle = paste0(
      "Points are means across draws; intervals show the ",
      "5th–95th percentiles across draws"
    )
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank()
  )

print(release_to_keep_plot)






###########
#1. Overall convergence for North Carolina bluefish

calib <- data.table::as.data.table(
  fst::read_fst(
    file.path(
      final_process_misc_cd,
      "calibrated_model_stats_7_9_26.fst"
    )
  )
)

calib[
  species == "bluefish" &
    grepl("^NC", region),
  .(
    n_draws = .N,
    n_converged = sum(convergence == 1, na.rm = TRUE),
    pct_converged = mean(convergence == 1, na.rm = TRUE),

    mean_pct_diff = mean(pct_diff_keep, na.rm = TRUE),
    median_pct_diff = median(pct_diff_keep, na.rm = TRUE),

    max_abs_pct_diff = max(abs(pct_diff_keep), na.rm = TRUE),

    mean_diff = mean(diff_keep, na.rm = TRUE),
    max_abs_diff = max(abs(diff_keep), na.rm = TRUE)
  ),
  by = .(region, mode)
]

#. Look at every draw
nc_blue <- calib[
  species == "bluefish" &
    grepl("^NC", region)
]

data.table::setorder(
  nc_blue,
  region,
  mode,
  draw
)

View(
  nc_blue[
    ,
    .(
      region,
      mode,
      draw,

      MRIP_keep,
      model_keep,

      diff_keep,
      pct_diff_keep,

      convergence,

      rel_to_keep,
      p_rel_to_keep,

      keep_to_rel,
      p_keep_to_rel
    )
  ]
)

nc_blue<-nc_blue[
  order(-abs(diff_keep))
][
  ,
  .(
    region,
    mode,
    draw,

    MRIP_keep,
    model_keep,

    diff_keep,
    pct_diff_keep,

    convergence
  )
]

ggplot2::ggplot(
  nc_blue,
  ggplot2::aes(
    draw,
    pct_diff_keep,
    colour = mode
  )
) +
  ggplot2::geom_hline(
    yintercept = 0,
    linetype = 2
  ) +
  ggplot2::geom_point() +
  ggplot2::facet_wrap(
    ~ region
  ) +
  ggplot2::labs(
    y = "% difference",
    x = "Simulation draw"
  ) +
  ggplot2::theme_bw()

#Compare MRIP and simulated harvest directly
nc_compare <- nc_blue[
  ,
  .(
    MRIP_keep,
    model_keep,
    diff_keep,
    pct_diff_keep
  ),
  by = .(
    region,
    mode,
    draw
  )
]

ggplot2::ggplot(
  nc_compare,
  ggplot2::aes(
    draw,
    pct_diff_keep,
    colour = mode
  )
) +
  ggplot2::geom_hline(
    yintercept = 0,
    linetype = 2
  ) +
  ggplot2::geom_point() +
  ggplot2::facet_wrap(
    ~ region
  ) +
  ggplot2::labs(
    y = "% difference",
    x = "Simulation draw"
  ) +
  ggplot2::theme_bw()

ggplot2::ggplot(
  nc_compare,
  ggplot2::aes(
    MRIP_keep,
    model_keep
  )
) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(
    slope = 1,
    intercept = 0,
    linetype = 2
  ) +
  ggplot2::facet_grid(
    region ~ mode,
    scales = "free"
  ) +
  ggplot2::theme_bw()

##
prediction_check <- prediction_long[
  metric == "tot_keep_blu_new"
]

prediction_check <- make_str_blu_projection_compare(prediction_check)

prediction_check[
  species == "bluefish" &
    metric == "harvest (#s)",
  .(
    projected = sum(projected_value)
  ),
  by = .(
    iteration,
    region
  )
]
##
#5. How often did calibration hit the maximum probability?
nc_blue<-nc_blue[
  ,
  .(
    max_rel_to_keep =
      mean(p_rel_to_keep >= 0.99, na.rm = TRUE),

    max_keep_to_rel =
      mean(p_keep_to_rel >= 0.99, na.rm = TRUE)
  ),
  by = .(
    region,
    mode
  )
]
