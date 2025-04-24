#* ------------------------------------------------------------------------------
#* Process INE data files by indicator type
#* ------------------------------------------------------------------------------

library(tidyverse)
library(data.table)

# Set paths
username <- Sys.getenv("USERNAME")

root <- paste0("C:/Users/", username, "/Dropbox/ineAtlas_data/")
gitdata <- paste0("C:/Users/", username, "/Documents/GitHub/ineAtlas.data/data/")

raw <- paste0(root, "/raw_atlasdata/")
proc <- paste0(root, "/proc/")

# Run function ----
source("data-raw/1. process_ine_data.r")

# process_ine_data("income")
# process_ine_data("income_sources")
# process_ine_data("demographics")
# process_ine_data("distribution_sex")
# process_ine_data("distribution_sex_age")
process_ine_data("distribution_sex_nationality")
# process_ine_data("gini_p80p20")