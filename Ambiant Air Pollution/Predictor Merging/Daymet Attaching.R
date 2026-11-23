library(terra)
library(data.table)

# =================================================
# Daymet monthly precipitation -> CONUS 5 km grid
# Years: 2010-2019
# Region: NA (use for CONUS)
# Variable: prcp monthly total
# =================================================

# -------------------------------------------------
# Input files
# -------------------------------------------------
grid_file <- "Y:/Ryan Armstrong/Pollution Modeling/Ambiant Air Pollution Data/Input Data/Predictors/CONUS_5km_Grid/conus_5km_grid_polygons_5070.gpkg"

daymet_folder <- "Y:/Ryan Armstrong/Pollution Modeling/Ambiant Air Pollution Data/Input Data/Predictors/Daymet/Daymet_Monthly_V4R1_2131_4.1-20261112_203842"

centroid_csv <- "Y:/Ryan Armstrong/Pollution Modeling/Ambiant Air Pollution Data/Input Data/Predictors/CONUS_5km_Grid/conus_5km_grid_centroids_5070_attributes.csv"

output_folder <- "Y:/Ryan Armstrong/Pollution Modeling/Ambiant Air Pollution Data/Input Data/Predictors/Daymet_Prcp_Monthly_2010_2019_5km_Summaries"
dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

out_csv          <- file.path(output_folder, "daymet_prcp_monthly_2010_2019_5km_summary.csv")
out_parquet      <- file.path(output_folder, "daymet_prcp_monthly_2010_2019_5km_summary.parquet")
out_missing_csv  <- file.path(output_folder, "daymet_prcp_monthly_2010_2019_missingness_summary.csv")
out_diag_csv     <- file.path(output_folder, "daymet_prcp_monthly_2010_2019_missingness_diagnostic.csv")
out_diag_parquet <- file.path(output_folder, "daymet_prcp_monthly_2010_2019_missingness_diagnostic.parquet")
out_map_gpkg     <- file.path(output_folder, "daymet_prcp_monthly_2010_2019_missingness_map.gpkg")

years <- 2010:2019

# -------------------------------------------------
# terra temp settings
# -------------------------------------------------
dir.create("C:/temp/terra", recursive = TRUE, showWarnings = FALSE)

terraOptions(
  tempdir = "C:/temp/terra",
  progress = 1
)

# -------------------------------------------------
# Read grid
# -------------------------------------------------
message("[", Sys.time(), "] Reading 5 km polygon grid...")
grid <- vect(grid_file)

if (!("grid_id" %in% names(grid))) {
  stop("grid_id field not found in grid file.")
}

message("[", Sys.time(), "] Grid cell count: ", format(nrow(grid), big.mark = ","))

# -------------------------------------------------
# Helper: centroid table
# -------------------------------------------------
message("[", Sys.time(), "] Preparing centroid attributes...")

if (file.exists(centroid_csv)) {
  cent_dt <- fread(centroid_csv)
  
  needed_cent_fields <- c("grid_id", "x_5070", "y_5070", "lon", "lat")
  missing_cent_fields <- setdiff(needed_cent_fields, names(cent_dt))
  
  if (length(missing_cent_fields) > 0) {
    stop(
      "Centroid CSV is missing required field(s): ",
      paste(missing_cent_fields, collapse = ", ")
    )
  }
  
  cent_dt <- cent_dt[, ..needed_cent_fields]
  
} else {
  message("[", Sys.time(), "] Centroid CSV not found; deriving centroids from grid...")
  
  cent <- centroids(grid)
  xy_5070 <- crds(cent)
  
  cent_ll <- project(cent, "EPSG:4326")
  xy_ll <- crds(cent_ll)
  
  cent_dt <- data.table(
    grid_id = grid$grid_id,
    x_5070 = xy_5070[, 1],
    y_5070 = xy_5070[, 2],
    lon    = xy_ll[, 1],
    lat    = xy_ll[, 2]
  )
}

# -------------------------------------------------
# Initialize output table
# -------------------------------------------------
summary_dt <- data.table(grid_id = grid$grid_id)

# -------------------------------------------------
# Process each year
# -------------------------------------------------
for (yr in years) {
  
  message("=================================================")
  message("[", Sys.time(), "] Processing year: ", yr)
  
  tif_file <- file.path(daymet_folder, paste0("daymet_v4_prcp_monttl_na_", yr, ".tif"))
  
  if (!file.exists(tif_file)) {
    warning("Missing file, skipping year: ", tif_file)
    next
  }
  
  message("[", Sys.time(), "] Reading raster: ", basename(tif_file))
  r <- rast(tif_file)
  
  message("[", Sys.time(), "] Raster layer count: ", nlyr(r))
  if (nlyr(r) != 12) {
    warning("Expected 12 monthly layers for year ", yr, ", found ", nlyr(r))
  }
  
  message("[", Sys.time(), "] CRS check:")
  message("  Grid   : ", crs(grid))
  message("  Daymet : ", crs(r))
  
  # -----------------------------------------------
  # Crop in raster CRS
  # -----------------------------------------------
  message("[", Sys.time(), "] Projecting grid footprint to raster CRS for crop...")
  grid_in_r_crs <- project(grid, crs(r))
  
  message("[", Sys.time(), "] Cropping raster to transformed grid extent...")
  r_crop <- crop(r, ext(grid_in_r_crs))
  
  # -----------------------------------------------
  # Project raster to grid CRS
  # -----------------------------------------------
  message("[", Sys.time(), "] Projecting raster to grid CRS...")
  r_5070 <- project(r_crop, crs(grid), method = "bilinear")
  
  # -----------------------------------------------
  # Extract monthly polygon means
  # -----------------------------------------------
  message("[", Sys.time(), "] Extracting monthly polygon means...")
  ext_dt <- as.data.table(extract(r_5070, grid, fun = mean, na.rm = TRUE))
  
  # First column is ID from extract; remaining columns are monthly layers
  month_cols <- names(ext_dt)[-1]
  
  # Rename monthly columns
  new_month_names <- sprintf("daymet_prcp_%d_%02d", yr, seq_along(month_cols))
  setnames(ext_dt, old = month_cols, new = new_month_names)
  
  # Keep only extracted value columns in same row order as grid
  ext_dt <- ext_dt[, -1]
  
  # Bind to summary table
  summary_dt <- cbind(summary_dt, ext_dt)
  
  # -----------------------------------------------
  # Annual summaries from monthly extracted values
  # -----------------------------------------------
  year_month_cols <- sprintf("daymet_prcp_%d_%02d", yr, 1:ncol(ext_dt))
  
  summary_dt[, paste0("daymet_prcp_annual_mean_", yr) := rowMeans(.SD, na.rm = TRUE), .SDcols = year_month_cols]
  
  summary_dt[, paste0("daymet_prcp_annual_min_", yr) := do.call(pmin, c(.SD, na.rm = TRUE)), .SDcols = year_month_cols]
  
  summary_dt[, paste0("daymet_prcp_annual_max_", yr) := do.call(pmax, c(.SD, na.rm = TRUE)), .SDcols = year_month_cols]
  
  # Missingness flag for all 12 months missing
  summary_dt[, paste0("daymet_prcp_allmonths_missing_", yr) := rowSums(is.na(.SD)) == length(year_month_cols), .SDcols = year_month_cols]
  
  # Percent missing months per cell-year
  summary_dt[, paste0("daymet_prcp_pct_months_missing_", yr) := 100 * rowSums(is.na(.SD)) / length(year_month_cols), .SDcols = year_month_cols]
  
  message("[", Sys.time(), "] Finished year: ", yr)
}

# -------------------------------------------------
# Build missingness summary table
# -------------------------------------------------
message("[", Sys.time(), "] Building missingness summary table...")

missing_list <- lapply(years, function(yr) {
  
  miss_col <- paste0("daymet_prcp_allmonths_missing_", yr)
  pctmiss_col <- paste0("daymet_prcp_pct_months_missing_", yr)
  
  if (!(miss_col %in% names(summary_dt))) {
    return(NULL)
  }
  
  n_total   <- nrow(summary_dt)
  n_missing <- summary_dt[, sum(get(miss_col), na.rm = TRUE)]
  n_nonmiss <- n_total - n_missing
  
  data.table(
    variable = "daymet_prcp",
    year = yr,
    total_grid_cells = n_total,
    non_missing_cells = n_nonmiss,
    missing_cells_all_12_months = n_missing,
    pct_non_missing = round(100 * n_nonmiss / n_total, 4),
    pct_missing_all_12_months = round(100 * n_missing / n_total, 4),
    mean_pct_months_missing_across_cells = round(summary_dt[, mean(get(pctmiss_col), na.rm = TRUE)], 4),
    max_pct_months_missing_across_cells = round(summary_dt[, max(get(pctmiss_col), na.rm = TRUE)], 4)
  )
})

missing_summary_dt <- rbindlist(missing_list, fill = TRUE)

# -------------------------------------------------
# Build diagnostic table
# -------------------------------------------------
message("[", Sys.time(), "] Building diagnostic table...")

diag_dt <- merge(
  cent_dt,
  summary_dt,
  by = "grid_id",
  all.x = TRUE,
  sort = FALSE
)

# -------------------------------------------------
# Build optional map layer
# -------------------------------------------------
message("[", Sys.time(), "] Building map layer...")

grid_map <- grid
grid_map <- merge(grid_map, diag_dt, by = "grid_id", all.x = TRUE)

# -------------------------------------------------
# Write outputs
# -------------------------------------------------
message("[", Sys.time(), "] Writing main summary CSV...")
fwrite(summary_dt, out_csv)

if (requireNamespace("arrow", quietly = TRUE)) {
  message("[", Sys.time(), "] Writing main summary Parquet...")
  arrow::write_parquet(summary_dt, out_parquet)
} else {
  message("[", Sys.time(), "] Package 'arrow' not installed; skipping main summary Parquet.")
}

message("[", Sys.time(), "] Writing missingness summary CSV...")
fwrite(missing_summary_dt, out_missing_csv)

message("[", Sys.time(), "] Writing diagnostic CSV...")
fwrite(diag_dt, out_diag_csv)

if (requireNamespace("arrow", quietly = TRUE)) {
  message("[", Sys.time(), "] Writing diagnostic Parquet...")
  arrow::write_parquet(diag_dt, out_diag_parquet)
} else {
  message("[", Sys.time(), "] Package 'arrow' not installed; skipping diagnostic Parquet.")
}

message("[", Sys.time(), "] Writing GeoPackage map layer...")
writeVector(grid_map, out_map_gpkg, overwrite = TRUE)

# -------------------------------------------------
# Console summary
# -------------------------------------------------
message("[", Sys.time(), "] Missingness summary:")
print(missing_summary_dt)

message("[", Sys.time(), "] Done.")