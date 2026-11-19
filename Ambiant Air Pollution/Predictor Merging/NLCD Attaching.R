library(terra)
library(data.table)

# Input files

grid_file <- "Y:/Ryan Armstrong/Pollution Modeling/Ambiant Air Pollution Data/Input Data/Predictors/CONUS_5km_Grid/conus_5km_grid_polygons_5070.gpkg"

# tree canopy cover
tcc_file <- "Y:/Ryan Armstrong/Pollution Modeling/Ambiant Air Pollution Data/Input Data/Predictors/nlcd_tcc_conus_2000_v2025-6_wgs84/nlcd_tcc_conus_wgs84_v2025-6_20000101_20001231.tif"

# surface imperviousness
imp_file <- "Y:/Ryan Armstrong/Pollution Modeling/Ambiant Air Pollution Data/Input Data/Predictors/Annual_NLCD_FctImp_2000_CU_C1V2/Annual_NLCD_FctImp_2000_CU_C1V2.tif"

# land cover
lc_file <- "Y:/Ryan Armstrong/Pollution Modeling/Ambiant Air Pollution Data/Input Data/Predictors/Annual_NLCD_LndCov_2000_CU_C1V2/Annual_NLCD_LndCov_2000_CU_C1V2.tif"

output_folder <- "Y:/Ryan Armstrong/Pollution Modeling/Ambiant Air Pollution Data/Input Data/Predictors/NLCD_2000_5km_Summaries"
dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

out_csv <- file.path(output_folder, "nlcd_2000_5km_summary_compact.csv")

# Read grid
message("[", Sys.time(), "] Reading 5 km polygon grid...")
grid <- vect(grid_file) # this reads it

message("[", Sys.time(), "] Grid cell count: ", format(nrow(grid), big.mark = ","))

# Build aligned 5 km template
message("[", Sys.time(), "] Building aligned 5 km raster template...")
template <- rast(
  ext = ext(grid),
  resolution = 5000,
  crs = crs(grid)
)

message("[", Sys.time(), "] Rasterizing grid_id to template...")
grid_id_rast <- rasterize(grid, template, field = "grid_id", touches = TRUE)
names(grid_id_rast) <- "grid_id"

# Read source rasters
message("[", Sys.time(), "] Reading NLCD rasters...")
r_tcc <- rast(tcc_file)
r_imp <- rast(imp_file)
r_lc  <- rast(lc_file)

message("[", Sys.time(), "] CRS check:")
message("  Grid/template: ", crs(grid))
message("  TCC          : ", crs(r_tcc))
message("  IMP          : ", crs(r_imp))
message("  LC           : ", crs(r_lc))

# Continuous rasters: project to aligned 5 km template
message("[", Sys.time(), "] Projecting tree canopy cover...")
tcc_5km <- project(r_tcc, template, method = "bilinear")
names(tcc_5km) <- "nlcd_tcc_mean_2000"

message("[", Sys.time(), "] Projecting imperviousness...")
imp_5km <- project(r_imp, template, method = "bilinear")
names(imp_5km) <- "nlcd_imp_mean_2000"

# Land-cover via 500 m intermediate template
message("[", Sys.time(), "] Building aligned 500 m template for land cover...")
template_500m <- rast(
  ext = ext(grid),
  resolution = 500,
  crs = crs(grid)
)

message("[", Sys.time(), "] Projecting land-cover raster to 500 m template (nearest neighbor)...")
lc_500m <- project(r_lc, template_500m, method = "near")

message("[", Sys.time(), "] Converting 500 m land-cover raster to numeric/integer class values...")
lc_500m_int <- classify(lc_500m, matrix(c(-Inf, Inf, NA), ncol = 3, byrow = TRUE))
values(lc_500m_int) <- as.integer(round(values(lc_500m, mat = FALSE)))

message("[", Sys.time(), "] Creating developed binary raster at 500 m...")
dev_500m <- ifel(lc_500m %in% c(21, 22, 23, 24), 1, 0)

message("[", Sys.time(), "] Aggregating developed proportion to 5 km...")
dev_5km <- aggregate(dev_500m, fact = 10, fun = mean, na.rm = TRUE)
dev_5km <- resample(dev_5km, template, method = "bilinear")
names(dev_5km) <- "nlcd_2000_prop_developed"

message("[", Sys.time(), "] Creating developed_high binary raster at 500 m...")
dev_high_500m <- ifel(lc_500m %in% c(24), 1, 0)

message("[", Sys.time(), "] Aggregating developed_high proportion to 5 km...")
dev_high_5km <- aggregate(dev_high_500m, fact = 10, fun = mean, na.rm = TRUE)
dev_high_5km <- resample(dev_high_5km, template, method = "bilinear")
names(dev_high_5km) <- "nlcd_2000_prop_developed_high"

# Combine layers
message("[", Sys.time(), "] Combining layers...")
stack_5km <- c(
  grid_id_rast,
  tcc_5km,
  imp_5km,
  lc_class_5km,
  dev_5km,
  dev_high_5km
)

# -------------------------------------------------
# Raster layers -> table
# -------------------------------------------------
# Pull raw cell values directly from each raster layer.
# This avoids factor/type weirdness that can happen with
# as.data.frame(stack_5km), especially for categorical land-cover.

message("[", Sys.time(), "] Converting raster layers to table...")
summary_dt <- data.table(
  grid_id = values(grid_id_rast, mat = FALSE),
  nlcd_tcc_mean_2000 = values(tcc_5km, mat = FALSE),
  nlcd_imp_mean_2000 = values(imp_5km, mat = FALSE),
  nlcd_lc_mode_code_2000 = values(lc_class_5km, mat = FALSE),
  nlcd_2000_prop_developed = values(dev_5km, mat = FALSE),
  nlcd_2000_prop_developed_high = values(dev_high_5km, mat = FALSE)
)

# Keep only valid grid cells
summary_dt <- summary_dt[!is.na(grid_id)]

# Make sure columns have expected types
summary_dt[, grid_id := as.character(grid_id)]
summary_dt[, nlcd_tcc_mean_2000 := as.numeric(nlcd_tcc_mean_2000)]
summary_dt[, nlcd_imp_mean_2000 := as.numeric(nlcd_imp_mean_2000)]
summary_dt[, nlcd_lc_mode_code_2000 := as.integer(round(as.numeric(nlcd_lc_mode_code_2000)))]
summary_dt[, nlcd_2000_prop_developed := as.numeric(nlcd_2000_prop_developed)]
summary_dt[, nlcd_2000_prop_developed_high := as.numeric(nlcd_2000_prop_developed_high)]

# Clamp proportions to valid range
summary_dt[, nlcd_2000_prop_developed :=
             pmin(pmax(nlcd_2000_prop_developed, 0), 1)]
summary_dt[, nlcd_2000_prop_developed_high :=
             pmin(pmax(nlcd_2000_prop_developed_high, 0), 1)]

# -------------------------------------------------
# Add NLCD class labels without merge()
# -------------------------------------------------
message("[", Sys.time(), "] Adding NLCD class labels...")

nlcd_label_map <- c(
  "11" = "Open Water",
  "21" = "Developed, Open Space",
  "22" = "Developed, Low Intensity",
  "23" = "Developed, Medium Intensity",
  "24" = "Developed, High Intensity",
  "31" = "Barren Land",
  "41" = "Deciduous Forest",
  "42" = "Evergreen Forest",
  "43" = "Mixed Forest",
  "52" = "Shrub/Scrub",
  "71" = "Grassland/Herbaceous",
  "81" = "Pasture/Hay",
  "82" = "Cultivated Crops",
  "90" = "Woody Wetlands",
  "95" = "Emergent Herbaceous Wetlands"
)

summary_dt[, nlcd_lc_mode_label_2000 := nlcd_label_map[as.character(nlcd_lc_mode_code_2000)]]

# -------------------------------------------------
# Reorder columns
# -------------------------------------------------
setcolorder(summary_dt, c(
  "grid_id",
  "nlcd_tcc_mean_2000",
  "nlcd_imp_mean_2000",
  "nlcd_lc_mode_code_2000",
  "nlcd_lc_mode_label_2000",
  "nlcd_2000_prop_developed",
  "nlcd_2000_prop_developed_high"
))

message("[", Sys.time(), "] Output rows: ", format(nrow(summary_dt), big.mark = ","))

# -------------------------------------------------
# Write outputs
# -------------------------------------------------
message("[", Sys.time(), "] Writing CSV...")
fwrite(summary_dt, out_csv)

if (requireNamespace("arrow", quietly = TRUE)) {
  out_parquet <- file.path(output_folder, "nlcd_2000_5km_summary_compact.parquet")
  message("[", Sys.time(), "] Writing Parquet...")
  arrow::write_parquet(summary_dt, out_parquet)
} else {
  message("[", Sys.time(), "] Package 'arrow' not installed; skipping Parquet.")
}

message("[", Sys.time(), "] Done.")

names(summary_dt)
summary(summary_dt$nlcd_lc_mode_code_2000)
unique(summary_dt$nlcd_lc_mode_code_2000)[1:20]
table(summary_dt$nlcd_lc_mode_code_2000, useNA = "ifany")



# missingness table 

# -------------------------------------------------
# Build mappable missingness diagnostic table
# -------------------------------------------------
message("[", Sys.time(), "] Building missingness diagnostic table...")

# get centroids of grid cells
cent <- centroids(grid)

# coordinates in EPSG:5070
xy_5070 <- crds(cent)

# coordinates in lon/lat
cent_ll <- project(cent, "EPSG:4326")
xy_ll <- crds(cent_ll)

miss_dt <- data.table(
  grid_id = as.character(grid$grid_id),
  x_5070 = xy_5070[, 1],
  y_5070 = xy_5070[, 2],
  lon = xy_ll[, 1],
  lat = xy_ll[, 2]
)

# join missingness flags from summary table
miss_dt <- merge(
  miss_dt,
  summary_dt[, .(
    grid_id,
    nlcd_tcc_mean_2000,
    nlcd_imp_mean_2000,
    nlcd_lc_mode_code_2000
  )],
  by = "grid_id",
  all.x = TRUE
)

# create missingness indicators
miss_dt[, nlcd_tcc_missing_2000 := is.na(nlcd_tcc_mean_2000)]
miss_dt[, nlcd_imp_missing_2000 := is.na(nlcd_imp_mean_2000)]
miss_dt[, nlcd_lc_missing_2000 := is.na(nlcd_lc_mode_code_2000)]

miss_dt[, nlcd_any_missing_2000 :=
          nlcd_tcc_missing_2000 |
          nlcd_imp_missing_2000 |
          nlcd_lc_missing_2000]

# keep only diagnostic fields
miss_dt <- miss_dt[, .(
  grid_id,
  x_5070,
  y_5070,
  lon,
  lat,
  nlcd_tcc_missing_2000,
  nlcd_imp_missing_2000,
  nlcd_lc_missing_2000,
  nlcd_any_missing_2000
)]

# write outputs
out_miss_csv <- file.path(output_folder, "nlcd_2000_5km_missingness.csv")
message("[", Sys.time(), "] Writing missingness CSV...")
fwrite(miss_dt, out_miss_csv)

if (requireNamespace("arrow", quietly = TRUE)) {
  out_miss_parquet <- file.path(output_folder, "nlcd_2000_5km_missingness.parquet")
  message("[", Sys.time(), "] Writing missingness Parquet...")
  arrow::write_parquet(miss_dt, out_miss_parquet)
}

# -------------------------------------------------
# Build summary missingness table
# -------------------------------------------------
message("[", Sys.time(), "] Building summary missingness table...")

n_total <- nrow(miss_dt)

miss_summary_dt <- data.table(
  metric = c(
    "total_grid_cells",
    "tcc_missing_n",
    "tcc_missing_pct",
    "imp_missing_n",
    "imp_missing_pct",
    "lc_missing_n",
    "lc_missing_pct",
    "any_missing_n",
    "any_missing_pct"
  ),
  value = c(
    n_total,
    sum(miss_dt$nlcd_tcc_missing_2000, na.rm = TRUE),
    100 * mean(miss_dt$nlcd_tcc_missing_2000, na.rm = TRUE),
    sum(miss_dt$nlcd_imp_missing_2000, na.rm = TRUE),
    100 * mean(miss_dt$nlcd_imp_missing_2000, na.rm = TRUE),
    sum(miss_dt$nlcd_lc_missing_2000, na.rm = TRUE),
    100 * mean(miss_dt$nlcd_lc_missing_2000, na.rm = TRUE),
    sum(miss_dt$nlcd_any_missing_2000, na.rm = TRUE),
    100 * mean(miss_dt$nlcd_any_missing_2000, na.rm = TRUE)
  )
)

out_miss_summary_csv <- file.path(output_folder, "nlcd_2000_5km_missingness_summary.csv")
message("[", Sys.time(), "] Writing missingness summary CSV...")
fwrite(miss_summary_dt, out_miss_summary_csv)

if (requireNamespace("arrow", quietly = TRUE)) {
  out_miss_summary_parquet <- file.path(output_folder, "nlcd_2000_5km_missingness_summary.parquet")
  message("[", Sys.time(), "] Writing missingness summary Parquet...")
  arrow::write_parquet(miss_summary_dt, out_miss_summary_parquet)
}