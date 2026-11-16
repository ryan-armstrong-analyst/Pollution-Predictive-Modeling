library(sf)
library(data.table)

# -----------------------------
# Input / output paths
# -----------------------------
boundary_file <- "Y:/Ryan Armstrong/Pollution Modeling/Ambiant Air Pollution Data/Input Data/Predictors/cb_2025_us_all_5m/cb_2025_us_all_5m.gpkg"
boundary_layer <- "cb_2025_us_state_5m"

output_folder <- "Y:/Ryan Armstrong/Pollution Modeling/Ambiant Air Pollution Data/Input Data/Predictors/CONUS_5km_Grid"

dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# Settings
# -----------------------------
target_crs <- 5070
cell_size_m <- 5000

# contiguous U.S. states only
conus_state_fips <- c(
  "01","04","05","06","08","09","10","11","12","13","16","17","18","19","20",
  "21","22","23","24","25","26","27","28","29","30","31","32","33","34","35",
  "36","37","38","39","40","41","42","44","45","46","47","48","49","50","51",
  "53","54","55","56"
)

message("[", Sys.time(), "] Reading state boundary layer...")
us <- st_read(boundary_file, layer = boundary_layer, quiet = FALSE)

message("[", Sys.time(), "] Columns in state layer:")
print(names(us))

# -----------------------------
# Filter to CONUS
# -----------------------------
if (!"statefp" %in% names(us)) {
  stop("statefp column not found in state layer.")
}

message("[", Sys.time(), "] Filtering to CONUS states...")
us_conus <- us[us$statefp %in% conus_state_fips, ]

if (nrow(us_conus) == 0) {
  stop("Filtering to CONUS returned zero rows.")
}

message("[", Sys.time(), "] States kept: ", nrow(us_conus))

# dissolve to one CONUS boundary
message("[", Sys.time(), "] Dissolving state boundaries into one CONUS polygon...")
conus <- st_union(us_conus)
conus <- st_as_sf(data.frame(id = 1, geometry = conus), crs = st_crs(us))
conus <- st_make_valid(conus)

# -----------------------------
# Project to EPSG:5070
# -----------------------------
message("[", Sys.time(), "] Projecting CONUS boundary to EPSG:5070...")
conus_5070 <- st_transform(conus, target_crs)

# -----------------------------
# Build full 5 km grid
# -----------------------------
message("[", Sys.time(), "] Creating 5 km grid over CONUS extent...")
grid_full <- st_make_grid(
  conus_5070,
  cellsize = c(cell_size_m, cell_size_m),
  square = TRUE,
  what = "polygons"
)

grid_full <- st_as_sf(
  data.frame(grid_id = seq_along(grid_full), geometry = grid_full),
  crs = target_crs
)

message("[", Sys.time(), "] Full grid cells before filtering: ",
        format(nrow(grid_full), big.mark = ","))

# -----------------------------
# Keep cells intersecting CONUS
# -----------------------------
message("[", Sys.time(), "] Filtering grid to cells intersecting CONUS...")
hits <- st_intersects(grid_full, conus_5070, sparse = FALSE)[, 1]
grid_conus <- grid_full[hits, ]

message("[", Sys.time(), "] Grid cells after CONUS filter: ",
        format(nrow(grid_conus), big.mark = ","))

# reset grid IDs
grid_conus$grid_id <- seq_len(nrow(grid_conus))

# -----------------------------
# Create centroids
# -----------------------------
message("[", Sys.time(), "] Creating centroids...")
centroids_5070 <- st_centroid(grid_conus)

coords_5070 <- st_coordinates(centroids_5070)
centroids_5070$x_5070 <- coords_5070[, 1]
centroids_5070$y_5070 <- coords_5070[, 2]

message("[", Sys.time(), "] Transforming centroids to EPSG:4326...")
centroids_ll <- st_transform(centroids_5070, 4326)

coords_ll <- st_coordinates(centroids_ll)
centroids_ll$lon <- coords_ll[, 1]
centroids_ll$lat <- coords_ll[, 2]

# -----------------------------
# Flat centroid table
# -----------------------------
centroid_dt <- as.data.table(st_drop_geometry(centroids_ll))
setcolorder(centroid_dt, c("grid_id", "x_5070", "y_5070", "lon", "lat"))

message("[", Sys.time(), "] Final CONUS 5 km cell count: ",
        format(nrow(centroid_dt), big.mark = ","))

# -----------------------------
# Output paths
# -----------------------------
boundary_out <- file.path(output_folder, "conus_boundary_5070.gpkg")
grid_out <- file.path(output_folder, "conus_5km_grid_polygons_5070.gpkg")
centroids_out <- file.path(output_folder, "conus_5km_grid_centroids_4326.gpkg")
centroids_csv <- file.path(output_folder, "conus_5km_grid_centroids.csv")

# -----------------------------
# Write outputs
# -----------------------------
message("[", Sys.time(), "] Writing CONUS boundary...")
st_write(conus_5070, boundary_out, delete_dsn = TRUE, quiet = TRUE)

message("[", Sys.time(), "] Writing polygon grid...")
st_write(grid_conus, grid_out, delete_dsn = TRUE, quiet = TRUE)

message("[", Sys.time(), "] Writing centroid layer...")
st_write(centroids_ll, centroids_out, delete_dsn = TRUE, quiet = TRUE)

message("[", Sys.time(), "] Writing centroid CSV...")
fwrite(centroid_dt, centroids_csv)

if (requireNamespace("arrow", quietly = TRUE)) {
  centroids_parquet <- file.path(output_folder, "conus_5km_grid_centroids.parquet")
  message("[", Sys.time(), "] Writing centroid Parquet...")
  arrow::write_parquet(centroid_dt, centroids_parquet)
} else {
  message("[", Sys.time(), "] Package 'arrow' not installed; skipping Parquet.")
}

message("[", Sys.time(), "] Done.")