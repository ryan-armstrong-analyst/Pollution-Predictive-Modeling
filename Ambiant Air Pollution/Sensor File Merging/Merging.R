
library(data.table)
# all zip files (and their destination after)
input_folder <- "Y:/Ryan Armstrong/Pollution Modeling/Ambiant Air Pollution/Input Data/Air Pollution Input"
output_folder <- "Y:/Ryan Armstrong/Pollution Modeling/Ambiant Air Pollution/Input Data/Merged Air Pollution Data"

# creates output folder
dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

# these are the 4 codes of pollutants (Ozone, CO, NO2, SO2)

codes <- c("44201", "42602", "42401", "42101")

for (code in codes) {
  
  zip_files <- list.files(
    input_folder,
    pattern = paste0("^daily_", code, "_\\d{4}\\.zip$"),
    full.names = TRUE
  )
  
  if (length(zip_files) == 0) {
    message("No files found for ", code)
    next
  }
  
  zip_files <- sort(zip_files)
  
  dat_list <- lapply(zip_files, function(z) {
    message("Reading: ", basename(z))
    
    inside <- unzip(z, list = TRUE)$Name
    csv_name <- inside[grepl("\\.csv$", inside, ignore.case = TRUE)][1]
    
    if (is.na(csv_name)) return(NULL)
    
    df <- read.csv(unz(z, csv_name), stringsAsFactors = FALSE)
    df$source_zip <- basename(z)
    df
  })
  
  dat_list <- Filter(Negate(is.null), dat_list)
  
  merged <- rbindlist(dat_list, fill = TRUE)
  
  out_file <- file.path(output_folder, paste0("merged_", code, ".csv"))
  fwrite(merged, out_file)
  
  message("Saved: ", out_file)
}

message("Done.")
