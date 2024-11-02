#*------------------------------------------------------------------------------
#* Author: Pablo García Guzmán
#* Project: ineAtlas
#* This script: Simple master script that runs all data processing scripts
#*------------------------------------------------------------------------------

# Load required packages
required_packages <- c("tidyverse")

# Install and load packages
for(pkg in required_packages) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Set paths
username <- Sys.getenv("USERNAME")
script_dir <- paste0("C:/Users/", username, "/Documents/GitHub/ineAtlas/data-raw/")

# List of scripts to run in order
script_files <- c(
  "1a. income.r",
  "1b. income_sources.r",
  "1c. demographics.r",
  "1d. distribution_sex.r",
  "1e. distribution_sex_age.r",
  "1f. distribution_sex_age_nationality.r",
  "1g. gini_p80p20.r"
)

# Create simple log function
log_message <- function(msg) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(sprintf("[%s] %s\n", timestamp, msg))
}

# Run each script
log_message("Starting data processing pipeline")

for(script in script_files) {
  script_path <- file.path(script_dir, script)
  
  tryCatch({
    log_message(paste("Running", script))
    source(script_path, encoding = "UTF-8")
    log_message(paste("Completed", script))
  }, 
  error = function(e) {
    log_message(paste("ERROR in", script, ":", conditionMessage(e)))
    stop(paste("Script failed:", script))
  },
  warning = function(w) {
    log_message(paste("Warning in", script, ":", conditionMessage(w)))
  })
}

log_message("Data processing pipeline completed successfully")