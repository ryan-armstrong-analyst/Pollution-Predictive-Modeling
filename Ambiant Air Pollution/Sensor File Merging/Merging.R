library(data.table)

# folder containing ZIP files
input_folder <- "Y:/Ryan Armstrong/Pollution Modeling/Ambiant Air Pollution Data/Input Data/Air Pollution Input"

# folder where merged CSVs will be saved
output_folder <- "Y:/Ryan Armstrong/Pollution Modeling/Ambiant Air Pollution Data/Input Data/Merged Air Pollution Data"

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
  message("Starting pollutant code: ", code)
  message("======================================")
  
  # find all ZIP files matching this pollutant code
  zip_files <- list.files(
    path = input_folder,
    pattern = paste0("^daily_", code, "_\\d{4}\\.zip$"),
    full.names = TRUE
  )
  
  # skip if no matching files are found
  if (length(zip_files) == 0) {
    message("No files found for ", code)
    next
  }
  
  # sort files, usually by year
  zip_files <- sort(zip_files)
  
  message("Found ", length(zip_files), " ZIP files for code ", code)
  
  # read each ZIP file into a list
  dat_list <- lapply(seq_along(zip_files), function(i) {
    z <- zip_files[i]
    
    message("[", i, "/", length(zip_files), "] Reading: ", basename(z))
    
    # list contents inside the ZIP
    inside <- unzip(z, list = TRUE)$Name
    
    # find the first CSV inside the ZIP
    csv_name <- inside[grepl("\\.csv$", inside, ignore.case = TRUE)][1]
    
    # skip ZIP if no CSV is found
    if (is.na(csv_name)) {
      message("   -> No CSV found inside ", basename(z), ", skipping.")
      return(NULL)
    }
    
    # read CSV directly from ZIP
    df <- read.csv(unz(z, csv_name), stringsAsFactors = FALSE)
    
    # add source ZIP filename for tracking
    df$source_zip <- basename(z)
    
    message("   -> Rows read: ", format(nrow(df), big.mark = ","))
    
    df
  })
  
  # remove any NULL entries
  dat_list <- Filter(Negate(is.null), dat_list)
  
  # skip if no valid CSV data was read
  if (length(dat_list) == 0) {
    message("No readable CSV data found for ", code)
    next
  }
  
  message("Combining ", length(dat_list), " data tables...")
  
  # combine all data frames into one table
  merged <- rbindlist(dat_list, fill = TRUE)
  
  message("Total merged rows: ", format(nrow(merged), big.mark = ","))
  
  # output file path
  out_file <- file.path(output_folder, paste0("merged_", code, ".csv"))
  
  message("Writing output file...")
  
  # write merged CSV
  fwrite(merged, out_file)
  
  message("Saved: ", out_file)
  message("Finished code: ", code)
}

message("\nAll processing complete.")

