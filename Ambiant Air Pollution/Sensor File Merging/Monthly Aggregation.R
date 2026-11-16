library(data.table)

# folder containing merged daily pollutant CSVs
input_folder <- "Y:/Ryan Armstrong/Pollution Modeling/Ambiant Air Pollution Data/Input Data/Merged Air Pollution Data"

# folder where monthly aggregated outputs will be saved
output_folder <- "Y:/Ryan Armstrong/Pollution Modeling/Ambiant Air Pollution Data/Input Data/Monthly Aggregated Air Pollution Data"

# create output folder if it does not already exist
dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

# pollutant codes
# 44201 = Ozone
# 42101 = Carbon Monoxide (CO)
# 42602 = Nitrogen Dioxide (NO2)
# 42401 = Sulfur Dioxide (SO2)
codes <- c("44201", "42602", "42401", "42101")

# loop through each pollutant code
for (code in codes) {
  
  message("\n======================================")
  message("Processing pollutant code: ", code)
  message("======================================")
  
  # input file path for the merged daily data
  in_file <- file.path(input_folder, paste0("merged_", code, ".csv"))
  
  # skip if merged file does not exist
  if (!file.exists(in_file)) {
    message("Missing file: ", in_file)
    next
  }
  
  # read merged daily data
  message("Reading: ", in_file)
  dt <- fread(in_file)
  message("Rows read: ", format(nrow(dt), big.mark = ","))
  
  # convert Date.Local to Date format
  dt[, Date.Local := as.Date(Date.Local)]
  
  # keep only rows from years 2010 through 2019
  dt <- dt[!is.na(Date.Local) & as.integer(format(Date.Local, "%Y")) %in% 2010:2019]
  message("Rows after year filter (2010-2019): ", format(nrow(dt), big.mark = ","))
  
  # skip if no rows remain
  if (nrow(dt) == 0) {
    message("No rows for 2010-2019 in ", in_file)
    next
  }
  
  # filter Sample.Duration where needed
  # CO (42101): keep 8-hour running average values
  # SO2 (42401): keep 1-hour values
  if ("Sample.Duration" %in% names(dt)) {
    
    if (code == "42101") {
      dt <- dt[
        Sample.Duration == "8-HR RUN AVG END HOUR" |
          Sample.Duration == "8-HR RUN AVG BEGIN HOUR"
      ]
      message("Rows after CO Sample.Duration filter: ", format(nrow(dt), big.mark = ","))
    }
    
    if (code == "42401") {
      dt <- dt[Sample.Duration == "1 HOUR"]
      message("Rows after SO2 Sample.Duration filter: ", format(nrow(dt), big.mark = ","))
    }
  }
  
  # skip if no rows remain after duration filtering
  if (nrow(dt) == 0) {
    message("No rows left after Sample.Duration filter for ", code)
    next
  }
  
  # if duplicate rows still exist for the same site and date,
  # sort first and keep the first record per site-date
  sort_cols <- intersect(
    c("State.Code", "County.Code", "Site.Num", "Date.Local", "POC"),
    names(dt)
  )
  
  if (length(sort_cols) > 0) {
    setorderv(dt, sort_cols)
  }
  
  dt <- dt[, .SD[1], by = .(State.Code, County.Code, Site.Num, Date.Local)]
  message("Rows after site-date deduplication: ", format(nrow(dt), big.mark = ","))
  
  # create year and month fields
  dt[, year := as.integer(format(Date.Local, "%Y"))]
  dt[, month := as.integer(format(Date.Local, "%m"))]
  
  # aggregate to monthly mean by monitoring site
  monthly <- dt[
    ,
    .(
      monthly_mean = mean(Arithmetic.Mean, na.rm = TRUE),
      days_with_data = sum(!is.na(Arithmetic.Mean))
    ),
    by = .(
      State.Code,
      County.Code,
      Site.Num,
      Latitude,
      Longitude,
      year,
      month
    )
  ]
  
  message("Monthly rows created: ", format(nrow(monthly), big.mark = ","))
  
  # create year-month string and add pollutant code
  monthly[, year_month := sprintf("%04d-%02d", year, month)]
  monthly[, pollutant_code := code]
  
  # reorder columns for cleaner output
  setcolorder(monthly, c(
    "pollutant_code",
    "State.Code", "County.Code", "Site.Num",
    "Latitude", "Longitude",
    "year", "month", "year_month",
    "monthly_mean", "days_with_data"
  ))
  
  # write monthly aggregated output
  out_file <- file.path(output_folder, paste0("monthly_", code, "_2010_2019.csv"))
  fwrite(monthly, out_file)
  
  message("Saved: ", out_file)
}

message("\nDone.")

