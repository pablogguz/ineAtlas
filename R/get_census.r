#' Fetch 2021 Census data from the ineAtlas data repository
#' 
#' Downloads and extracts compressed census data files from the ineAtlas data repository,
#' providing access to detailed demographic, socioeconomic and housing indicators at 
#' different geographic levels from the 2021 Population and Housing Census.
#' 
#' @param level Character string specifying the geographic level. Must be one of:
#'   "municipality", "district", or "tract"
#' @param cache Logical indicating whether to cache the extracted data. Default is TRUE.
#'   Cached data is stored uncompressed for faster access.
#' @param cache_dir Character string specifying the cache directory. Default is tempdir().
#' 
#' @return A tibble containing the requested census data at the specified geographic level.
#'   The data includes demographic, socioeconomic and housing indicators from the 2021
#'   Population and Housing Census. The data is automatically extracted from compressed 
#'   files and cached locally if requested.
#' 
#' @importFrom dplyr %>% filter select mutate
#' @importFrom readr read_csv write_csv
#' @importFrom httr GET stop_for_status
#' @importFrom stringr str_extract str_remove str_trim
#' 
#' @export
#' 
#' @examples
#' \donttest{
#' # Get municipality level census data
#' mun_data <- get_census("municipality")
#' 
#' # Get district level census data without caching
#' dist_data <- get_census("district", cache = FALSE)
#' 
#' # Get census tract level data
#' tract_data <- get_census("tract")
#' }
#' 
#' @note Data files are stored compressed on the repository to reduce size and
#'   download times. The function handles decompression automatically. Census data
#'   is only available for 2021.
get_census <- function(level, cache = TRUE, cache_dir = tempdir()) {
  # Validate inputs
  valid_levels <- c("municipality", "district", "tract")
  
  if (!level %in% valid_levels) {
    stop("Level must be one of: ", paste(valid_levels, collapse = ", "))
  }
  
  # Construct the GitHub raw content URL
  base_url <- "https://raw.githubusercontent.com/pablogguz/ineAtlas.data/main/data"
  
  # Construct filename
  filename <- paste0("census_2021/census_2021_", level, ".zip")
  
  url <- file.path(base_url, filename)
  
  # Set up caching (still using .csv for cached files)
  if (cache) {
    cache_file <- file.path(cache_dir, paste0("ineatlas_census_2021_", level, ".csv"))
    
    # Check if cached file exists and is less than 24 hours old
    if (file.exists(cache_file)) {
      file_age <- difftime(Sys.time(), file.mtime(cache_file), units = "hours")
      if (file_age < 24) {
        message("Using cached data from ", basename(cache_file))
        data <- readr::read_csv(cache_file, show_col_types = FALSE)
        message("Data successfully loaded from cache: ", nrow(data), " rows, ", ncol(data), " columns")
        return(data)
      }
    }
  }
  
  # Try to download and process the data
  tryCatch({
    message("Downloading data from ", basename(filename), "...")
    # Create temporary files for zip and csv
    temp_zip <- tempfile(fileext = ".zip")
    temp_dir <- tempfile()
    dir.create(temp_dir)
    
    # Download zip file
    response <- httr::GET(url, httr::write_disk(temp_zip, overwrite = TRUE))
    
    # Check for HTTP errors
    httr::stop_for_status(response)
    
    # Unzip to temporary directory
    utils::unzip(temp_zip, exdir = temp_dir)
    
    # Find the CSV file in the temp directory
    csv_file <- list.files(temp_dir, pattern = "\\.csv$", full.names = TRUE)[1]
    if (is.na(csv_file)) {
      stop("No CSV file found in downloaded zip")
    }
    
    # Read the CSV
    data <- readr::read_csv(csv_file, show_col_types = FALSE)
    
    # Clean up temporary files
    unlink(temp_zip)
    unlink(temp_dir, recursive = TRUE)
    
    # Cache the data if requested
    if (cache) {
      message("Caching data to ", basename(cache_file))
      dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
      readr::write_csv(data, cache_file)
    }
    
    message("Data successfully loaded: ", nrow(data), " rows, ", ncol(data), " columns")
    
    return(data)
    
  }, error = function(e) {
    # Check for cached data if download fails
    if (cache && file.exists(cache_file)) {
      warning("Download failed, using cached data. Error: ", e$message)
      data <- readr::read_csv(cache_file, show_col_types = FALSE)
      message("Data successfully loaded from cache: ", nrow(data), " rows, ", ncol(data), " columns")
      return(data)
    } else {
      stop("Failed to download data and no cache available. Error: ", e$message)
    }
  })
}