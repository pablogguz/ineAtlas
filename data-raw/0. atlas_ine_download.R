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

# Set paths 
username <- Sys.getenv("USERNAME")

root <- paste0("C:/Users/", username, "/Dropbox/ineAtlas_data/")

raw <- paste0(root, "/raw_atlasdata/")
proc <- paste0(root, "/proc/")

# Script starts ------

# Function to download and save INE data
download_ine_data <- function(file_code) {
  url <- paste0("https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/", file_code, ".csv?nocab=1")
  message(paste("Downloading file code:", file_code))
  
  # File name is simply the code
  file_name <- paste0(file_code, ".csv")
  
  # Increase timeout for large files (in seconds)
  timeout_original <- getOption("timeout")
  options(timeout = 600)  # Set timeout to 5 minutes
  
  # Download and save the data
  tryCatch({
    data <- fread(url)
    write.csv(data, paste0(raw, file_name), row.names = FALSE)
    message(paste("File saved as:", file_name))
    # Restore original timeout
    options(timeout = timeout_original)
    return(TRUE)
  }, error = function(e) {
    message(paste("Failed to download file code:", file_code))
    message(e)
    # Restore original timeout
    options(timeout = timeout_original)
    return(FALSE)
  })
}

# List of file codes to download
# file_codes <- c("30824", "30825", "30826", "30827", "30828", 
#                 "30829", "30830", "30831", "30832", "37677")

file_codes <- c("30827", "30828", 
                "30829", "30830", "30831", "30832", "37677")

# Download all files
for (code in file_codes) {
  download_ine_data(code)
}

