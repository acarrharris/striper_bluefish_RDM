install.packages(c("sf", "dplyr", "lubridate", "purrr", "stringr", "lwgeom"))
# library(dplyr) # optional

make_atomic_period_shps <- function(master_shp,
                                    out_dir,
                                    year = 2024,
                                    dissolve_by_status = TRUE,
                                    crs_assume_if_missing = 4326) {

  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  g <- sf::read_sf(master_shp)

  # Polygons only + fix geometry
  g <- g[sf::st_geometry_type(g, by_geometry = TRUE) %in% c("POLYGON", "MULTIPOLYGON"), , drop = FALSE]
  g <- sf::st_make_valid(g)

  if (is.na(sf::st_crs(g))) sf::st_crs(g) <- crs_assume_if_missing

  # Parse dates
  g <- g |>
    dplyr::mutate(
      start_dt = as.Date(lubridate::parse_date_time(as.character(start_date),
                                                    orders = c("ymd", "mdy", "dmy", "Ymd", "mdY"))),
      end_dt   = as.Date(lubridate::parse_date_time(as.character(end_date),
                                                    orders = c("ymd", "mdy", "dmy", "Ymd", "mdY")))
    )

  if (any(is.na(g$start_dt)) || any(is.na(g$end_dt))) {
    stop("Some start_date/end_date values could not be parsed.")
  }

  # Keep only portions relevant to the target year (clip date-wise)
  y0 <- as.Date(sprintf("%d-01-01", year))
  y1 <- as.Date(sprintf("%d-12-31", year))

  g <- g |>
    dplyr::filter(!(end_dt < y0 | start_dt > y1)) |>
    dplyr::mutate(
      start_dt = pmax(start_dt, y0),
      end_dt   = pmin(end_dt, y1)
    )

  # Standardize status (your fields)
  g <- g |>
    dplyr::mutate(
      reg_status = dplyr::case_when(
        ClosedtoFi == "yes" ~ "Closed to fishing",
        CatchandRe == "yes" ~ "Catch & release only",
        OpentoFish == "yes" ~ "Open to fishing",
        TRUE                ~ "Unknown"
      ),
      reg_status = factor(
        reg_status,
        levels = c("Open to fishing", "Catch & release only", "Closed to fishing", "Unknown")
      ),
      duration_days = as.integer(end_dt - start_dt) + 1L,
      status_pri = dplyr::recode(
        as.character(reg_status),
        "Closed to fishing"      = 3L,
        "Catch & release only"   = 2L,
        "Open to fishing"        = 1L,
        "Unknown"                = 0L,
        .default = 0L
      )
    )

  # ---- Build atomic (non-overlapping) time intervals that cover the year ----
  # Boundaries: all starts plus (ends + 1 day), plus year bounds.
  bps <- sort(unique(c(
    y0,
    g$start_dt,
    g$end_dt + 1L,
    y1 + 1L
  )))

  # Atomic intervals are [bps[i], bps[i+1]-1]
  atomic <- dplyr::tibble(
    period_start = bps[-length(bps)],
    period_end   = bps[-1] - 1L
  ) |>
    dplyr::filter(period_start <= period_end) |>
    dplyr::mutate(
      period_label = paste0(format(period_start, "%Y-%m-%d"), "_to_", format(period_end, "%Y-%m-%d"))
    )

  # Helper: iterative "most-specific-first" spatial allocation
  # - Sort features by duration_days ascending (more specific first)
  # - Tie-breaker: status_pri descending (Closed first)
  # - Each feature contributes only geometry not already covered
  allocate_geometry_by_specificity <- function(gp) {
    if (nrow(gp) == 0) return(gp)

    gp <- gp |>
      dplyr::arrange(duration_days, dplyr::desc(status_pri))

    kept <- list()
    covered <- NULL

    for (i in seq_len(nrow(gp))) {
      geom_i <- sf::st_geometry(gp[i, ])

      # subtract already-covered geometry (higher specificity)
      if (!is.null(covered)) {
        geom_i <- sf::st_difference(geom_i, covered)
        }

      # keep if anything remains
      if (!sf::st_is_empty(geom_i)) {
        row_i <- gp[i, , drop = FALSE]
        sf::st_geometry(row_i) <- geom_i
        kept[[length(kept) + 1L]] <- row_i

        covered <- if (is.null(covered)) {
          sf::st_union(geom_i)
        } else {
          sf::st_union(covered, sf::st_union(geom_i))
        }
      }
    }

    if (length(kept) == 0) return(gp[0, ])
    dplyr::bind_rows(kept) |>
      sf::st_make_valid()
  }

  # ---- For each atomic period, build the regulation layer ----
  out_files <- character(0)

  for (k in seq_len(nrow(atomic))) {
    ps <- atomic$period_start[k]
    pe <- atomic$period_end[k]
    lab <- atomic$period_label[k]

    # Features that fully cover the atomic interval
    gp <- g |>
      dplyr::filter(start_dt <= ps, end_dt >= pe)

    # Allocate spatially by specificity so broad ranges only fill gaps
    gp_alloc <- allocate_geometry_by_specificity(gp) |>
      dplyr::mutate(
        period_start = ps,
        period_end   = pe,
        period_label = lab
      )

    # Optional dissolve by reg_status (reduces internal fragmentation)
    if (isTRUE(dissolve_by_status) && nrow(gp_alloc) > 0) {
      gp_alloc <- gp_alloc |>
        dplyr::group_by(period_label, period_start, period_end, reg_status) |>
        dplyr::summarise(do_union = TRUE, .groups = "drop") |>
        sf::st_make_valid()
    }

    safe <- stringr::str_replace_all(lab, "[:/\\\\ ]", "_")
    out_path <- file.path(out_dir, paste0("SB_regs_atomic_", safe, ".shp"))

    suppressWarnings(sf::st_write(gp_alloc, out_path, delete_dsn = TRUE, quiet = TRUE))
    out_files <- c(out_files, out_path)
  }

  invisible(list(atomic_periods = atomic, out_files = out_files))
}

# ---- RUN IT ----
master_shp <- "E:/Lou_projects/striper_bluefish_RDM/input_data/MD_shapefiles/SBRegsDates2024.shp"
out_dir    <- "E:/Lou_projects/striper_bluefish_RDM/input_data/MD_shapefiles/period_shps_atomic_2024"

res_atomic <- make_atomic_period_shps(
  master_shp = master_shp,
  out_dir    = out_dir,
  year       = 2024,
  dissolve_by_status = TRUE
)

# res_atomic$out_files are your non-overlapping-in-time, year-covering shapefiles

# --- Paths ---
atomic_dir <- "E:/Lou_projects/striper_bluefish_RDM/input_data/MD_shapefiles/period_shps_atomic_2024"
sites_csv  <- "E:/Lou_projects/striper_bluefish_RDM/input_data/MD_shapefiles/MD_sample_sites_all_22_23.csv"
sites_csv  <- "E:/Lou_projects/striper_bluefish_RDM/input_data/MD_shapefiles/MD_sample_sites_mar1_31.csv"
sites_csv  <- "E:/Lou_projects/striper_bluefish_RDM/input_data/MD_shapefiles/MD_sample_sites_apr1_may15.csv"
sites_csv  <- "E:/Lou_projects/striper_bluefish_RDM/input_data/MD_shapefiles/MD_sample_sites_may16_may31.csv"
sites_csv  <- "E:/Lou_projects/striper_bluefish_RDM/input_data/MD_shapefiles/MD_sample_sites_jun1_jul15.csv"
sites_csv  <- "E:/Lou_projects/striper_bluefish_RDM/input_data/MD_shapefiles/MD_sample_sites_jun16_jul31.csv"
sites_csv  <- "E:/Lou_projects/striper_bluefish_RDM/input_data/MD_shapefiles/MD_sample_sites_aug1_dec10.csv"
sites_csv  <- "E:/Lou_projects/striper_bluefish_RDM/input_data/MD_shapefiles/MD_sample_sites_dec11_dec31.csv"

library(dplyr)
# --- Pick ONE atomic period shapefile ---
# Example: May 16–May 31 (edit to the one you want)
one_period_files <- list.files(atomic_dir, pattern = "\\.shp$", full.names = TRUE)
# one_period_files <- one_period_files[stringr::str_detect(basename(one_period_files),"2024-01-01_to_2024-02-29")]
# one_period_files <- one_period_files[stringr::str_detect(basename(one_period_files),"2024-03-01_to_2024-03-31")]
# one_period_files <- one_period_files[stringr::str_detect(basename(one_period_files),"2024-04-01_to_2024-05-15")]
# one_period_files <- one_period_files[stringr::str_detect(basename(one_period_files),"2024-05-16_to_2024-05-31")]
# one_period_files <- one_period_files[stringr::str_detect(basename(one_period_files),"2024-06-01_to_2024-07-15")]
# one_period_files <- one_period_files[stringr::str_detect(basename(one_period_files),"2024-07-16_to_2024-07-31")]
# one_period_files <- one_period_files[stringr::str_detect(basename(one_period_files),"2024-08-01_to_2024-12-10")]
 one_period_files <- one_period_files[stringr::str_detect(basename(one_period_files),"2024-12-11_to_2024-12-31")]

stopifnot(length(one_period_files) == 1)
one_shp <- one_period_files[1]

# --- Read sites (lat/lon -> sf points) ---
sites <- readr::read_csv(sites_csv, show_col_types = FALSE)
stopifnot(all(c("SITE_LAT", "SITE_LONG") %in% names(sites)))

sites_sf <- sf::st_as_sf(
  sites,
  coords = c("SITE_LONG", "SITE_LAT"),
  crs = 4326,
  remove = FALSE
)

# --- Helper: pick a reasonable "subregion" field if one exists ---
pick_subregion_field <- function(nm) {
  nm_low <- tolower(nm)
  preferred <- c("subregion", "region", "area", "zone", "name", "label", "mgmt", "management", "river", "comar", "type")
  hit <- preferred[preferred %in% nm_low]
  if (length(hit) > 0) return(nm[match(hit[1], nm_low)])
  return(NULL)
}

# --- Read + standardize one shapefile ---
read_one_shp <- function(shp_path) {
  g <- sf::read_sf(shp_path)

  # Keep only polygonal geometry
  g <- g[sf::st_geometry_type(g, by_geometry = TRUE) %in% c("POLYGON", "MULTIPOLYGON"), , drop = FALSE]

  # Fix invalid geometries
  g <- sf::st_make_valid(g)

  # Ensure CRS exists
  if (is.na(sf::st_crs(g))) sf::st_crs(g) <- 4326

  # Period label: prefer attribute if present, else filename
  period_file <- tools::file_path_sans_ext(basename(shp_path))
  if ("period_label" %in% names(g)) {
    g <- dplyr::mutate(g, period_file = as.character(period_label))
  } else {
    g <- dplyr::mutate(g, period_file = period_file)
  }

  # Choose a subregion / label field if available
  non_geom_cols <- setdiff(names(g), attr(g, "sf_column"))
  sub_field <- pick_subregion_field(non_geom_cols)

  if (!is.null(sub_field)) {
    g <- dplyr::mutate(
      g,
      subregion_name = as.character(.data[[sub_field]])
    )
  } else {
    g <- dplyr::mutate(
      g,
      subregion_name = paste0(period_file, "_", dplyr::row_number())
    )
  }

  # Standardize reg_status from your known fields (if present)
  has_status_fields <- all(c("ClosedtoFi", "CatchandRe", "OpentoFish") %in% names(g))
  if (has_status_fields) {
    g <- dplyr::mutate(
      g,
      reg_status = dplyr::case_when(
        ClosedtoFi == "yes" ~ "Closed to fishing",
        CatchandRe == "yes" ~ "Catch & release only",
        OpentoFish == "yes" ~ "Open to fishing",
        TRUE                ~ "Unknown"
      )
    )
  } else if ("reg_status" %in% names(g)) {
    g <- dplyr::mutate(g, reg_status = as.character(reg_status))
  } else {
    g <- dplyr::mutate(g, reg_status = "Unknown")
  }

  g <- dplyr::mutate(
    g,
    reg_status = factor(
      as.character(reg_status),
      levels = c("Open to fishing", "Catch & release only", "Closed to fishing", "Unknown")
    )
  )

  dplyr::select(g, period_file, subregion_name, reg_status, dplyr::everything())
}

# --- Read the selected shapefile (list -> bind_rows, even though it's 1) ---
mgmt_polys_list <- purrr::map(one_shp, read_one_shp)  # one_shp is length 1
mgmt_polys      <- dplyr::bind_rows(mgmt_polys_list)

# --- Work in a projected CRS for distance calculations ---
target_crs <- 26918  # NAD83 / UTM zone 18N (often good for Chesapeake distance calcs)

mgmt_polys_p <- sf::st_transform(mgmt_polys, target_crs)
sites_p      <- sf::st_transform(sites_sf, target_crs)

# --- Assign polygons to points: within-first, nearest fallback ---
within_list <- sf::st_within(sites_p, mgmt_polys_p)   # list of polygon indices per point
within_n    <- lengths(within_list)

poly_area <- as.numeric(sf::st_area(mgmt_polys_p))

pick_poly_for_point <- function(idx_vec) {
  if (length(idx_vec) == 0) return(NA_integer_)  # in no polygon
  if (length(idx_vec) == 1) return(idx_vec)      # clean match

  # If multiple polygons contain point: pick deterministically
  # priority: Closed > C&R > Open > Unknown; tie-break: smallest area
  pri <- dplyr::recode(
    as.character(mgmt_polys_p$rg_stts[idx_vec]),
    "Closed to fishing"       = 3L,
    "Catch & release only"    = 2L,
    "Open to fishing"         = 1L,
    "Unknown"                 = 0L,
    .default = 0L
  )

  best <- idx_vec[pri == max(pri)]
  best[which.min(poly_area[best])]
}

picked_idx <- vapply(within_list, pick_poly_for_point, integer(1))

# Fallback for points not inside any polygon
fallback <- sf::st_nearest_feature(sites_p, mgmt_polys_p)
picked_idx[is.na(picked_idx)] <- fallback[is.na(picked_idx)]

picked_poly <- mgmt_polys_p[picked_idx, ]

sites_colored <- sites_p |>
  dplyr::mutate(
    nearest_period_file = picked_poly$period_file,
    nearest_subregion   = picked_poly$subregion_name,
    nearest_reg_status  = picked_poly$rg_stts,
    nearest_dist_m      = as.numeric(sf::st_distance(sites_p, sf::st_geometry(picked_poly), by_element = TRUE)),
    within_n            = within_n
  )

# --- Colors + status levels ---
status_levels <- c("Open to fishing", "Catch & release only", "Closed to fishing", "Unknown")
reg_colors <- c(
  "Open to fishing"       = "green3",
  "Catch & release only"  = "gold",
  "Closed to fishing"     = "red3",
  "Unknown"               = "grey70"
)

# --- Transform to lat/lon for leaflet ---
polys_ll <- sf::st_transform(mgmt_polys_p, 4326) |>
  dplyr::mutate(reg_status = factor(as.character(rg_stts), levels = status_levels))

pts_ll <- sf::st_transform(sites_colored, 4326) |>
  dplyr::mutate(nearest_reg_status = factor(as.character(nearest_reg_status), levels = status_levels))

pal  <- leaflet::colorFactor(palette = unname(reg_colors[status_levels]), levels = status_levels)
pal2 <- leaflet::colorFactor(palette = unname(reg_colors[status_levels]), levels = status_levels)


# --- Optional: filter to a subset of sites (comment out if not needed) ---
# pts_ll <- pts_ll |> dplyr::filter(SITE_EXTERNAL_ID %in% c(53, 940, 4149))

# --- Leaflet map ---
leaflet::leaflet() |>
  leaflet::addProviderTiles("CartoDB.Positron") |>
  leaflet::addPolygons(
    data = polys_ll,
    fillColor   = ~pal(reg_status),
    fillOpacity = 0.8,
    color       = "black",
    weight      = 1,
    popup = ~paste0(
      "<b>Period:</b> ", period_file, "<br>",
      "<b>Status:</b> ", as.character(reg_status), "<br>",
      if ("COMAR" %in% names(polys_ll)) paste0("<b>COMAR:</b> ", as.character(COMAR), "<br>") else "",
      if ("Type"  %in% names(polys_ll)) paste0("<b>Type:</b> ",  as.character(Type),  "<br>") else "",
      if ("River" %in% names(polys_ll)) paste0("<b>River:</b> ", as.character(River), "<br>") else ""
    )
  ) |>
  leaflet::addCircleMarkers(
    data = pts_ll,
    fillColor   = ~pal2(nearest_reg_status),
    radius      = 4,
    stroke      = TRUE,
    color       = "black",
    weight      = 1,
    fillOpacity = 1,
    popup = ~paste0(
      "<b>SITE_EXTERNAL_ID:</b> ", SITE_EXTERNAL_ID, "<br>",
      "<b>Assigned status:</b> ", as.character(nearest_reg_status), "<br>",
      "<b>Distance (m):</b> ", round(nearest_dist_m, 1), "<br>",
      "<b>Within # polys:</b> ", within_n
    )
  ) |>
  leaflet::addLegend(
    position = "bottomright",
    pal      = pal,
    values   = status_levels,
    title    = "Regulation status"
  )


potomac_ids <- c(788, 1969, 1865, 0107, 3076, 3077, 3774, 0233)

sites_colored <- sites_colored %>%
  dplyr::mutate(
    is_potomac = SITE_EXTERNAL_ID %in% potomac_ids
  )


#Assume boat trips in some sites can travel from closed/C&R to open/open
sites_colored <- sites_colored %>%
  dplyr::mutate(
    nearest_reg_status_boat = dplyr::case_when(
      SITE_EXTERNAL_ID %in% c( 940, 854) ~
        "Open to fishing",
      TRUE ~ as.character(nearest_reg_status)
    ),
    nearest_reg_status = factor(nearest_reg_status, levels = status_levels)
  )

# 1) CSV (no geometry)
sites_colored<-sites_colored %>%
  dplyr::select(SITE_INTERNAL_ID, SITE_EXTERNAL_ID, nearest_reg_status, is_potomac) %>%  #, nearest_reg_status_boat) %>%
  readr::write_csv(file.path("E:/Lou_projects/striper_bluefish_RDM/input_data/MD_shapefiles/md_sites_alloc_Jan1_Feb28.csv"))

sites_colored<-sites_colored %>%
  dplyr::select(SITE_INTERNAL_ID, SITE_EXTERNAL_ID, nearest_reg_status, nearest_reg_status_boat, is_potomac) %>%  #, nearest_reg_status_boat) %>%
  readr::write_csv(file.path("E:/Lou_projects/striper_bluefish_RDM/input_data/MD_shapefiles/md_sites_alloc_Mar1_Mar31.csv"))

sites_colored<-sites_colored %>%
  dplyr::select(SITE_INTERNAL_ID, SITE_EXTERNAL_ID, nearest_reg_status, is_potomac) %>%  #, nearest_reg_status_boat) %>%
  readr::write_csv(file.path("E:/Lou_projects/striper_bluefish_RDM/input_data/MD_shapefiles/md_sites_alloc_Apr1_May15.csv"))

sites_colored<-sites_colored %>%
  dplyr::select(SITE_INTERNAL_ID, SITE_EXTERNAL_ID, nearest_reg_status, nearest_reg_status_boat, is_potomac) %>%  #, nearest_reg_status_boat) %>%
  readr::write_csv(file.path("E:/Lou_projects/striper_bluefish_RDM/input_data/MD_shapefiles/md_sites_alloc_May16_May31.csv"))

sites_colored<-sites_colored %>%
  dplyr::select(SITE_INTERNAL_ID, SITE_EXTERNAL_ID, nearest_reg_status, is_potomac) %>%  #, nearest_reg_status_boat) %>%
  readr::write_csv(file.path("E:/Lou_projects/striper_bluefish_RDM/input_data/MD_shapefiles/md_sites_alloc_Jun1_Jul15.csv"))

sites_colored<-sites_colored %>%
  dplyr::select(SITE_INTERNAL_ID, SITE_EXTERNAL_ID, nearest_reg_status, is_potomac) %>%  #, nearest_reg_status_boat) %>%
  readr::write_csv(file.path("E:/Lou_projects/striper_bluefish_RDM/input_data/MD_shapefiles/md_sites_alloc_Jul16_Jul31.csv"))

sites_colored<-sites_colored %>%
  dplyr::select(SITE_INTERNAL_ID, SITE_EXTERNAL_ID, nearest_reg_status, is_potomac) %>%  #, nearest_reg_status_boat) %>%
  readr::write_csv(file.path("E:/Lou_projects/striper_bluefish_RDM/input_data/MD_shapefiles/md_sites_alloc_Aug1_Dec10.csv"))

sites_colored<-sites_colored %>%
  dplyr::select(SITE_INTERNAL_ID, SITE_EXTERNAL_ID, nearest_reg_status, is_potomac) %>%  #, nearest_reg_status_boat) %>%
  readr::write_csv(file.path("E:/Lou_projects/striper_bluefish_RDM/input_data/MD_shapefiles/md_sites_alloc_Dec11_Dec31.csv"))
