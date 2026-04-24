#* ------------------------------------------------------------------------------
#* Process INE data files by indicator type
#* ------------------------------------------------------------------------------

library(tidyverse)
library(data.table)

# Paths (OS-aware)
if (.Platform$OS.type == "windows") {
  home <- paste0("C:/Users/", Sys.getenv("USERNAME"))
} else {
  home <- Sys.getenv("HOME")
}

root <- file.path(home, "Dropbox", "ineAtlas_data")
gitdata <- file.path(home, "Documents", "GitHub", "ineAtlas.data", "data")

raw <- file.path(root, "raw_atlasdata")
proc <- file.path(root, "proc")

# Run function ----
source("data-raw/1. process_ine_data.r")

process_ine_data("income")
process_ine_data("income_sources")
process_ine_data("demographics")
process_ine_data("distribution_sex")
process_ine_data("distribution_sex_age")
process_ine_data("distribution_sex_nationality")
process_ine_data("gini_p80p20")