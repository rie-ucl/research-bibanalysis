# Purpose: Check if the TRC data storage is set up correctly

# ------------------------------------------------------------------------------
# Copy files from the local directory to the TRC data storage
# ------------------------------------------------------------------------------
from_dir <- "data/bio/"
to_dir <- "T:/raw/bio/"

files <- list.files(path = from_dir, full.names = TRUE)

for (from in files) {
  base <- basename(from)

  # Remove "1305_"
  base <- sub("1305_", "", base)

  # Replace first two "-" with "_"
  base <- sub("-", "_", base)
  base <- sub("-", "_", base)

  # Extract the date part and remove it from the main part
  date_part <- sub(".*_(\\d{4})_(\\d{2})_(\\d{2})_.*", "\\1\\2\\3", base)
  base_no_date <- sub("_(\\d{4})_(\\d{2})_(\\d{2})", "", base)

  # Add "_raw" before extension
  base_no_date <- sub("\\.rda$", "_raw.rda", base_no_date)

  # Construct new filename with date at the end (before extension)
  base_new <- sub("_raw\\.rda$", paste0("_raw_", date_part, ".rda"), base_no_date)

  to <- file.path(to_dir, base_new)

  file.copy(from = from, to = to)
}

# ------------------------------------------------------------------------------
# Check if the TRC data storage is set up correctly
# ------------------------------------------------------------------------------
# Load required libraries
library(bibliokit)

# Load the data storage path from environment variable
data_storage <- Sys.getenv("TRC_DATA_STORAGE")
if (data_storage == "") {
  stop("Environment variable TRC_DATA_STORAGE is not set.")
}

# Convert raw data to authors.tib
raw_path <- file.path(data_storage, "raw")
bibliokit::load_all_bibdata( raw_path )

authors.tib <- dfs_to_authors()

save_path <- file.path( data_storage, "processed/bio_2005-2024_authors.rds" )
saveRDS( authors.tib, file = save_path )

authdata_path <- file.path( data_storage, "processed/bio_2005-2024_authors.rds" )
readRDS( )
