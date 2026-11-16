library(terra)
library(data.table)

# -------------------------------------------------
# Input files
# -------------------------------------------------
grid_file <- "Y:/Ryan Armstrong/Pollution Modeling/Ambiant Air Pollution Data/Input Data/Predictors/CONUS_5km_Grid/conus_5km_grid_polygons_5070.gpkg"

tcc_file <- "Y:/Ryan Armstrong/Pollution Modeling/Ambiant Air Pollution Data/Input Data/Predictors/nlcd_tcc_conus_2000_v2025-6_wgs84/nlcd_tcc_conus_wgs84_v2025-6_20000101_20001231.tif"

imp_file <- "Y:/Ryan Armstrong/Pollution Modeling/Ambiant Air Pollution Data/Input Data/Predictors/Annual_NLCD_FctImp_2000_CU_C1V2/Annual_NLCD_FctImp_2000_CU_C1V2.tif"

lc_file <- "Y:/Ryan Armstrong/Pollution Modeling/Ambiant Air Pollution Data/Input Data/Predictors/Annual_NLCD_LndCov_2000_CU_C1V2/Annual_NLCD_LndCov_2000_CU_C1V2.tif"

output_folder <- "Y:/Ryan Armstrong/Pollution Modeling/Ambiant Air Pollution Data/Input Data/Predictors/NLCD_2000_5km_Summaries"
dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

out_csv <- file.path(output_folder, "nlcd_2000_5km_summary.csv")

# -------------------------------------------------
# Read grid
# -------------------------------------------------
message("[", Sys.time(), "] Reading 5 km polygon grid...")
grid <- vect(grid_file)

message("[", Sys.time(), "] Grid cell count: ", format(nrow(grid), big.mark = ","))

# -------------------------------------------------
# Read rasters
# -------------------------------------------------
message("[", Sys.time(), "] Reading NLCD rasters...")
r_tcc <- rast(tcc_file)
r_imp <- rast(imp_file)
r_lc  <- rast(lc_file)

message("[", Sys.time(), "] Raster CRS check:")
message("  TCC: ", crs(r_tcc))
message("  IMP: ", crs(r_imp))
message("  LC : ", crs(r_lc))

# -------------------------------------------------
# Project grid to raster CRS
# -------------------------------------------------
# Since these NLCD rasters are in geographic coordinates / WGS84,
# project the polygon grid to the raster CRS before extraction.
message("[", Sys.time(), "] Projecting 5 km grid to raster CRS...")
grid_rast_crs <- project(grid, crs(r_tcc))

# -------------------------------------------------
# Extract polygon means
# -------------------------------------------------
message("[", Sys.time(), "] Extracting mean tree canopy cover...")
tcc_ext <- extract(r_tcc, grid_rast_crs, fun = mean, na.rm = TRUE)

message("[", Sys.time(), "] Extracting mean imperviousness...")
imp_ext <- extract(r_imp, grid_rast_crs, fun = mean, na.rm = TRUE)

# NOTE:
# land cover is categorical, so mean class code is not scientifically ideal.
# This is included only as a temporary diagnostic/output.
message("[", Sys.time(), "] Extracting mean land cover code (diagnostic only)...")
lc_ext <- extract(r_lc, grid_rast_crs, fun = mean, na.rm = TRUE)

# -------------------------------------------------
# Build summary table
# -------------------------------------------------
message("[", Sys.time(), "] Building summary table...")

summary_dt <- data.table(
  grid_id = grid_rast_crs$grid_id,
  nlcd_tcc_mean_2000 = tcc_ext[, 2],
  nlcd_imp_mean_2000 = imp_ext[, 2],
  nlcd_lc_mean_code_2000 = lc_ext[, 2]
)

message("[", Sys.time(), "] Output rows: ", format(nrow(summary_dt), big.mark = ","))

# -------------------------------------------------
# Write outputs
# -------------------------------------------------
message("[", Sys.time(), "] Writing CSV...")
fwrite(summary_dt, out_csv)

if (requireNamespace("arrow", quietly = TRUE)) {
  out_parquet <- file.path(output_folder, "nlcd_2000_5km_summary.parquet")
  message("[", Sys.time(), "] Writing Parquet...")
  arrow::write_parquet(summary_dt, out_parquet)
} else {
  message("[", Sys.time(), "] Package 'arrow' not installed; skipping Parquet.")
}

message("[", Sys.time(), "] Done.")