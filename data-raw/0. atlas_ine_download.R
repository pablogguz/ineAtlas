#*------------------------------------------------------------------------------
#* Author: Pablo García Guzmán
#* Project: ineAtlas
#* This script: Downloads data from INE website
#*------------------------------------------------------------------------------

packages_to_load <- c(
    "tidyverse", 
    "haven", 
    "countrycode", 
    "data.table"
) 

package.check <- lapply(
  packages_to_load,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
    }
  }
)

lapply(packages_to_load, require, character=T)

# Paths (OS-aware)
if (.Platform$OS.type == "windows") {
  home <- paste0("C:/Users/", Sys.getenv("USERNAME"))
} else {
  home <- Sys.getenv("HOME")
}

root <- file.path(home, "Dropbox", "ineAtlas_data")
raw <- file.path(root, "raw_atlasdata")
proc <- file.path(root, "proc")

dir.create(raw, recursive = TRUE, showWarnings = FALSE)

# Script starts ------

# INE file code -> indicator name mapping.
# Each file corresponds to an INE Atlas table at
#   https://www.ine.es/jaxiT3/Tabla.htm?t={code}
# File names match what `1. process_ine_data.r` expects downstream.
file_map <- c(
  "30824" = "income_raw",
  "30825" = "income_sources_raw",
  "30826" = "distribution_sex_abs",
  "30827" = "distribution_sex_age_abs",
  "30828" = "distribution_sex_nationality_abs",
  "30829" = "distribution_sex_rel",
  "30830" = "distribution_sex_age_rel",
  "30831" = "distribution_sex_nationality_rel",
  "30832" = "demographics_raw",
  "37677" = "gini_p80p20_raw"
)

# Function to download and save INE data
download_ine_data <- function(file_code, out_name) {
  url <- paste0("https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/", file_code, ".csv?nocab=1")
  message(sprintf("Downloading %s (code %s)", out_name, file_code))

  file_name <- paste0(out_name, ".csv")

  timeout_original <- getOption("timeout")
  options(timeout = 600)

  tryCatch({
    data <- fread(url)
    write.csv(data, file.path(raw, file_name), row.names = FALSE)
    message(paste("File saved as:", file_name))
    options(timeout = timeout_original)
    return(TRUE)
  }, error = function(e) {
    message(paste("Failed to download:", out_name))
    message(e)
    options(timeout = timeout_original)
    return(FALSE)
  })
}

# Download all files. To download only a subset, filter `file_map` before the loop.
for (code in names(file_map)) {
  download_ine_data(code, file_map[[code]])
}

