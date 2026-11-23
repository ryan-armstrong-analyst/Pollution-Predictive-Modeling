library(terra)
library(data.table)

# =================================================
# LandScan 2010 -> CONUS 5 km grid summary
# =================================================

# -------------------------------------------------
# Input files
# -------------------------------------------------
grid_file <- "Y:/Ryan Armstrong/Pollution Modeling/Ambiant Air Pollution Data/Input Data/Predictors/CONUS_5km_Grid/conus_5km_grid_polygons_5070.gpkg"

landscan_file <- "Y:/Ryan Armstrong/Pollution Modeling/Ambiant Air Pollution Data/Input Data/Predictors/landscan-global-2010-assets/landscan-global-2010.tif"

# Optional centroid attribute table if already created
# Expected fields: grid_id, x_5070, y_5070, lon, lat
centroid_csv <- "Y:/Ryan Armstrong/Pollution Modeling/Ambiant Air Pollution Data/Input Data/Predictors/CONUS_5km_Grid/conus_5km_grid_centroids_5070_attributes.csv"

output_folder <- "Y:/Ryan Armstrong/Pollution Modeling/Ambiant Air Pollution Data/Input Data/Predictors/LandScan_2010_5km_Summaries"
dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

out_csv              <- file.path(output_folder, "landscan_2010_5km_summary.csv")
out_parquet          <- file.path(output_folder, "landscan_2010_5km_summary.parquet")
out_missing_csv      <- file.path(output_folder, "landscan_2010_missingness_summary.csv")
out_diag_csv         <- file.path(output_folder, "landscan_2010_missingness_diagnostic.csv")
out_diag_parquet     <- file.path(output_folder, "landscan_2010_missingness_diagnostic.parquet")
out_map_gpkg         <- file.path(output_folder, "landscan_2010_missingness_map.gpkg")

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

message("[", Sys.time(), "] Grid cell count: ", format(nrow(grid), big.mark = ","))

if (!("grid_id" %in% names(grid))) {
  stop("grid_id field not found in grid file.")
}

# -------------------------------------------------
# Read LandScan raster
# -------------------------------------------------
message("[", Sys.time(), "] Reading LandScan raster...")
r_ls <- rast(landscan_file)

message("[", Sys.time(), "] CRS check:")
message("  Grid     : ", crs(grid))
message("  LandScan : ", crs(r_ls))

# -------------------------------------------------
# Transform grid footprint to raster CRS for crop
# -------------------------------------------------
message("[", Sys.time(), "] Projecting grid footprint to LandScan CRS for crop...")
grid_wgs84 <- project(grid, crs(r_ls))

message("[", Sys.time(), "] Cropping LandScan to transformed grid extent...")
r_ls_crop <- crop(r_ls, ext(grid_wgs84))

# Optional tighter mask to footprint
# message("[", Sys.time(), "] Masking cropped LandScan to transformed grid footprint...")
# r_ls_crop <- mask(r_ls_crop, grid_wgs84)

# -------------------------------------------------
# Project cropped LandScan to grid CRS
# -------------------------------------------------
message("[", Sys.time(), "] Projecting LandScan raster to grid CRS...")
r_ls_5070 <- project(r_ls_crop, crs(grid), method = "bilinear")

# -------------------------------------------------
# Extract summary statistics by 5 km polygon
# -------------------------------------------------
message("[", Sys.time(), "] Extracting LandScan population sum...")
ls_sum <- extract(r_ls_5070, grid, fun = sum, na.rm = TRUE)

message("[", Sys.time(), "] Extracting LandScan population mean...")
ls_mean <- extract(r_ls_5070, grid, fun = mean, na.rm = TRUE)

# -------------------------------------------------
# Build main summary table
# -------------------------------------------------
message("[", Sys.time(), "] Building main summary table...")

summary_dt <- data.table(
  grid_id = grid$grid_id,
  landscan_pop_sum_2010  = ls_sum[, 2],
  landscan_pop_mean_2010 = ls_mean[, 2]
)

summary_dt[, landscan_pop_density_km2_2010 := landscan_pop_sum_2010 / 25]

# Missingness flag
summary_dt[, landscan_missing_2010 := is.na(landscan_pop_sum_2010)]

message("[", Sys.time(), "] Output rows: ", format(nrow(summary_dt), big.mark = ","))

# -------------------------------------------------
# Missingness summary table
# -------------------------------------------------
message("[", Sys.time(), "] Building missingness summary table...")

n_total   <- nrow(summary_dt)
n_missing <- summary_dt[, sum(landscan_missing_2010, na.rm = TRUE)]
n_nonmiss <- n_total - n_missing

missing_summary_dt <- data.table(
  variable = "landscan_pop_sum_2010",
  total_grid_cells = n_total,
  non_missing_cells = n_nonmiss,
  missing_cells = n_missing,
  pct_non_missing = round(100 * n_nonmiss / n_total, 4),
  pct_missing = round(100 * n_missing / n_total, 4)
)

# -------------------------------------------------
# Mappable diagnostic table
# -------------------------------------------------
message("[", Sys.time(), "] Building mappable diagnostic table...")

if (file.exists(centroid_csv)) {
  message("[", Sys.time(), "] Reading centroid attribute table...")
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
  message("[", Sys.time(), "] Centroid CSV not found; deriving coordinates from grid centroids...")
  
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

diag_dt <- merge(
  cent_dt,
  summary_dt,
  by = "grid_id",
  all.x = TRUE,
  sort = FALSE
)

setcolorder(
  diag_dt,
  c(
    "grid_id",
    "x_5070", "y_5070", "lon", "lat",
    "landscan_pop_sum_2010",
    "landscan_pop_mean_2010",
    "landscan_pop_density_km2_2010",
    "landscan_missing_2010"
  )
)

# -------------------------------------------------
# Optional GIS map layer
# -------------------------------------------------
message("[", Sys.time(), "] Building optional GeoPackage map layer...")

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