
# --- Packages (install if needed) ---
 install.packages(c("sf", "dplyr", "readr", "stringr", "units", "purrr", "lwgeom"))
library(dplyr)

# --- Paths ---
data_dir  <- "E:/Lou_projects/striper_bluefish_RDM/input_data/MD_shapefiles"
#sites_csv <- file.path(data_dir, "MD_sample_sites.csv")  # (this matches what was mounted)
#sites_csv <- file.path(data_dir, "MD_sample_sites_may16.csv")  # (this matches what was mounted)
sites_csv <- file.path(data_dir, "MD_sample_sites_all_22_23.csv")  # (this matches what was mounted)

# --- Read sites (lat/lon -> sf points) ---
sites <- readr::read_csv(sites_csv, show_col_types = FALSE)


stopifnot(all(c("SITE_LAT", "SITE_LONG") %in% names(sites)))

sites_sf <- sf::st_as_sf(
  sites,
  coords = c("SITE_LONG", "SITE_LAT"),
  crs = 4326,   # assuming lat/lon WGS84
  remove = FALSE
)

# --- Helper: pick a reasonable "subregion" field if one exists ---
pick_subregion_field <- function(nm) {
  nm_low <- tolower(nm)
  # common candidates in shapefiles
  preferred <- c("subregion", "region", "area", "zone", "name", "label", "mgmt", "management")
  hit <- preferred[preferred %in% nm_low]
  if (length(hit) > 0) return(nm[match(hit[1], nm_low)])
  return(NULL)
}

# --- Read + standardize one shapefile ---
read_one_shp <- function(shp_path) {
  g <- sf::read_sf(shp_path)

  # Keep only polygonal geometry (drop points/lines if any)
  g <- g[sf::st_geometry_type(g, by_geometry = TRUE) %in% c("POLYGON", "MULTIPOLYGON"), , drop = FALSE]

  # Fix invalid geometries (common in regulatory layers)
  g <- sf::st_make_valid(g)

  # Ensure it has a CRS; if missing, assume WGS84 (you can change this if needed)
  if (is.na(sf::st_crs(g))) {
    sf::st_crs(g) <- 4326
  }

  # Try to extract a subregion name field; otherwise create row-based id
  non_geom_cols <- setdiff(names(g), attr(g, "sf_column"))
  sub_field <- pick_subregion_field(non_geom_cols)

  period_file <- tools::file_path_sans_ext(basename(shp_path))

  if (!is.null(sub_field)) {
    g <- dplyr::mutate(
      g,
      period_file   = period_file,
      subregion_name = as.character(.data[[sub_field]])
    )
  } else {
    g <- dplyr::mutate(
      g,
      period_file   = period_file,
      subregion_name = paste0(period_file, "_", dplyr::row_number())
    )
  }

  # Keep only what we need + preserve original attrs if you want them later
  g <- dplyr::select(g, period_file, subregion_name, dplyr::everything())

  g
}

# --- Read ALL shapefiles in the folder and stack them ---
shp_files <- list.files(data_dir, pattern = "\\.shp$", full.names = TRUE)

# If you only want the specific ones you mentioned, uncomment this filter:
shp_files <- shp_files[basename(shp_files) %in% c(
  #"Catch_releaseOpenJan1_Feb28.shp"
  #"closed2fishingCatchRlOpenMarch1_31.shp"
  #"Closed_OpenApril1_May15.shp"
  "ClosedOpen_CRMay16_31.shp"
  #"Open2fishingJune1_July15th.shp"
  #"Open_ClosedJuly16_31st.shp"
  #"FishingOpenAugust_Dec10.shp"
  #"ClosedOpenDec11_31st.shp"
)]

mgmt_polys_list <- purrr::map(shp_files, read_one_shp)
mgmt_polys      <- dplyr::bind_rows(mgmt_polys_list)

# --- Work in a projected CRS for distance calculations (Maryland / Chesapeake: EPSG:26918 is common) ---
target_crs <- 26918  # NAD83 / UTM zone 18N

mgmt_polys_p <- sf::st_transform(mgmt_polys, target_crs)
sites_p      <- sf::st_transform(sites_sf, target_crs)


# Add polygon attributes to points
mgmt_polys_p <- mgmt_polys_p |>
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
    )
  )



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
    as.character(mgmt_polys_p$reg_status[idx_vec]),
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
    nearest_reg_status  = picked_poly$reg_status,
    nearest_dist_m      = as.numeric(sf::st_distance(sites_p, sf::st_geometry(picked_poly), by_element = TRUE)),
    within_n            = within_n
  )

reg_colors <- c(
  "Open to fishing"       = "green3",
  "Catch & release only"  = "gold",
  "Closed to fishing"     = "red3",
  "Unknown"               = "grey70"
)


#Interactive leaflet map
status_levels <- c(
  "Open to fishing",
  "Catch & release only",
  "Closed to fishing",
  "Unknown"
)

# Ensure both polygon and point status vars are factors with same levels
polys_ll <- sf::st_transform(mgmt_polys_p, 4326) |>
  dplyr::mutate(reg_status = factor(as.character(reg_status), levels = status_levels))

pts_ll <- sf::st_transform(sites_colored, 4326) |>
  dplyr::mutate(nearest_reg_status = factor(as.character(nearest_reg_status), levels = status_levels))  %>%
  dplyr::filter(SITE_EXTERNAL_ID %in% c(53, 940, 4149))

# IMPORTANT: provide palette in the same order as status_levels
pal  <- leaflet::colorFactor(palette = unname(reg_colors[status_levels]), levels = status_levels)
pal2 <- leaflet::colorFactor(palette = unname(reg_colors[status_levels]), levels = status_levels)


leaflet::leaflet() |>
  leaflet::addProviderTiles("CartoDB.Positron") |>
  leaflet::addPolygons(
    data = polys_ll,
    fillColor   = ~pal(reg_status),
    fillOpacity = 1,
    color       = "black",
    weight      = 1,
    popup = ~paste0(
      "<b>Period:</b> ", period_file, "<br>",
      "<b>Status:</b> ", reg_status
    )
  ) |>
  leaflet::addCircleMarkers(
    data = pts_ll,
    fillColor   = ~pal2(nearest_reg_status),
    radius      = 3,
    stroke      = TRUE,
    color       = "black",
    weight      = 1,
    fillOpacity = 1,
    popup = ~paste0("<b>SITE_EXTERNAL_ID:</b> ", SITE_EXTERNAL_ID)
  ) |>
  leaflet::addLegend(
    position = "bottomright",
    pal      = pal,
    values   = status_levels,
    title    = "Regulation status"
  )

#Manually identify sites along the Potomac River
#If there are boat trips out of them, the will assume the Potmomoc River regs, while
#shore trips will assume the regs identified here

potomac_ids <- c(788, 1969, 1865, 0107, 3076, 3077, 3774, 0233)

sites_colored <- sites_colored %>%
  dplyr::mutate(
    is_potomac = SITE_EXTERNAL_ID %in% potomac_ids
  )



# manual changes - March shapefile
# sites_colored <- sites_colored %>%
#   dplyr::mutate(
#     nearest_reg_status = dplyr::case_when(
#       SITE_EXTERNAL_ID %in% c(3840, 3695, 3694, 0976,0904,
#                               0930, 0901, 0905, 0964, 0913) ~
#         "Closed to fishing",
#       TRUE ~ as.character(nearest_reg_status)
#     ),
#     nearest_reg_status = factor(nearest_reg_status, levels = status_levels)
#   )


#Assume boat trips in some sites can travel from closed to catch and release area - March shapefile
# sites_colored <- sites_colored %>%
#   dplyr::mutate(
#     nearest_reg_status_boat = dplyr::case_when(
#       SITE_EXTERNAL_ID %in% c(1968, 44, 3438) ~
#         "Catch & release only",
#       TRUE ~ as.character(nearest_reg_status)
#     ),
#     nearest_reg_status = factor(nearest_reg_status, levels = status_levels)
#   )

#Assume boat trips in some sites can travel from closed/C&R to open - May 16 shapefile
sites_colored <- sites_colored %>%
  dplyr::mutate(
    nearest_reg_status_boat = dplyr::case_when(
      SITE_EXTERNAL_ID %in% c(1801, 854, 940) ~
        "Open to fishing",
      TRUE ~ as.character(nearest_reg_status)
    ),
    nearest_reg_status = factor(nearest_reg_status, levels = status_levels)
  )


# Ensure both polygon and point status vars are factors with same levels
polys_ll <- sf::st_transform(mgmt_polys_p, 4326) |>
  dplyr::mutate(reg_status = factor(as.character(reg_status), levels = status_levels))

pts_ll <- sf::st_transform(sites_colored, 4326) |>
  dplyr::mutate(nearest_reg_status = factor(as.character(nearest_reg_status), levels = status_levels)) #%>%
  #dplyr::filter(is_potomac==TRUE)

# IMPORTANT: provide palette in the same order as status_levels
pal  <- leaflet::colorFactor(palette = unname(reg_colors[status_levels]), levels = status_levels)
pal2 <- leaflet::colorFactor(palette = unname(reg_colors[status_levels]), levels = status_levels)


leaflet::leaflet() |>
  leaflet::addProviderTiles("CartoDB.Positron") |>
  leaflet::addPolygons(
    data = polys_ll,
    fillColor   = ~pal(reg_status),
    fillOpacity = 1,
    color       = "black",
    weight      = 1,
    popup = ~paste0(
      "<b>Period:</b> ", period_file, "<br>",
      "<b>Status:</b> ", reg_status
    )
  ) |>
  leaflet::addCircleMarkers(
    data = pts_ll,
    fillColor   = ~pal2(nearest_reg_status),
    radius      = 3,
    stroke      = TRUE,
    color       = "black",
    weight      = 1,
    fillOpacity = 1,
    popup = ~paste0("<b>SITE_EXTERNAL_ID:</b> ", SITE_EXTERNAL_ID)
  ) |>
  leaflet::addLegend(
    position = "bottomright",
    pal      = pal,
    values   = status_levels,
    title    = "Regulation status"
  )




# --- Save outputs ---
# 1) CSV (no geometry)
sites_colored<-sites_colored %>%
  dplyr::select(SITE_INTERNAL_ID, SITE_EXTERNAL_ID, nearest_reg_status, is_potomac) %>%  #, nearest_reg_status_boat) %>%
  readr::write_csv(file.path(data_dir, "md_sites_Jan1_Feb28.csv"))

sites_colored<-sites_colored %>%
  dplyr::select(SITE_INTERNAL_ID, SITE_EXTERNAL_ID, nearest_reg_status, nearest_reg_status_boat, is_potomac) %>%  #, nearest_reg_status_boat) %>%
  readr::write_csv(file.path(data_dir, "md_sites_March1_31.csv"))

sites_colored<-sites_colored %>%
  dplyr::select(SITE_INTERNAL_ID, SITE_EXTERNAL_ID, nearest_reg_status, is_potomac) %>%  #, nearest_reg_status_boat) %>%
  readr::write_csv(file.path(data_dir, "md_sites_April1_May15.csv"))

sites_colored<-sites_colored %>%
  dplyr::select(SITE_INTERNAL_ID, SITE_EXTERNAL_ID, nearest_reg_status, nearest_reg_status_boat, is_potomac) %>%  #, nearest_reg_status_boat) %>%
  readr::write_csv(file.path(data_dir, "md_sites_May16_31.csv"))

sites_colored<-sites_colored %>%
  dplyr::select(SITE_INTERNAL_ID, SITE_EXTERNAL_ID, nearest_reg_status, is_potomac) %>%  #, nearest_reg_status_boat) %>%
  readr::write_csv(file.path(data_dir, "md_sites_June1_July15th.csv"))

sites_colored<-sites_colored %>%
  dplyr::select(SITE_INTERNAL_ID, SITE_EXTERNAL_ID, nearest_reg_status, is_potomac) %>%  #, nearest_reg_status_boat) %>%
  readr::write_csv(file.path(data_dir, "md_sites_July16_31st.csv"))

sites_colored<-sites_colored %>%
  dplyr::select(SITE_INTERNAL_ID, SITE_EXTERNAL_ID, nearest_reg_status, is_potomac) %>%  #, nearest_reg_status_boat) %>%
  readr::write_csv(file.path(data_dir, "md_sites_August_Dec10.csv"))

sites_colored<-sites_colored %>%
  dplyr::select(SITE_INTERNAL_ID, SITE_EXTERNAL_ID, nearest_reg_status, is_potomac) %>%  #, nearest_reg_status_boat) %>%
  readr::write_csv(file.path(data_dir, "md_sites_Dec11_31st.csv"))
