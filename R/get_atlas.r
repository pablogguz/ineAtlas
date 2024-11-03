#' Fetch data from the ineAtlas data repository
#' 
#' Downloads and extracts compressed data files from the ineAtlas data repository,
#' providing access to various socioeconomic indicators at different geographic levels.
#' 
#' @param category Character string specifying the data category. Must be one of:
#'   "income", "income_sources", "demographics", "distribution_sex",
#'   "distribution_sex_age", or "distribution_sex_nationality"
#' @param level Character string specifying the geographic level. Must be one of:
#'   "municipality", "district", or "tract"
#' @param cache Logical indicating whether to cache the extracted data. Default is TRUE.
#'   Cached data is stored uncompressed for faster access.
#' @param cache_dir Character string specifying the cache directory. Default is tempdir().
#' 
#' @return A tibble containing the requested data. Distribution data will include
#'   additional columns for demographic breakdowns (sex, age, nationality).
#'   The data is automatically extracted from compressed files and cached
#'   locally if requested.
#' 
#' @importFrom dplyr %>% filter select mutate
#' @importFrom readr read_csv write_csv
#' @importFrom httr GET stop_for_status
#' @importFrom stringr str_extract str_remove str_trim
#' 
#' @export
#' 
#' @examples
#' # Get municipality level income data
#' income_data <- get_atlas("income", "municipality")
#' 
#' # Get district level demographics without caching
#' demo_data <- get_atlas("demographics", "district", cache = FALSE)
#' 
#' # Get income distribution indicators by sex
#' sex_dist <- get_atlas("distribution_sex", "municipality")
#' 
#' # Get income distribution indicators by sex and age
#' age_dist <- get_atlas("distribution_sex_age", "district")
#' 
#' @note Data files are stored compressed on the repository to reduce size and
#'   download times. The function handles decompression automatically.
get_atlas <- function(category, level, cache = TRUE, cache_dir = tempdir()) {
  # Validate inputs
  valid_categories <- c(
    "income", 
    "income_sources", 
    "demographics",
    "distribution_sex",
    "distribution_sex_age",
    "distribution_sex_nationality"
  )
  
  valid_levels <- c("municipality", "district", "tract")
  
  if (!category %in% valid_categories) {
    stop("Category must be one of: ", paste(valid_categories, collapse = ", "))
  }
  if (!level %in% valid_levels) {
    stop("Level must be one of: ", paste(valid_levels, collapse = ", "))
  }
  
  # Construct the GitHub raw content URL
  base_url <- "https://raw.githubusercontent.com/pablogguz/ineAtlas.data/main/data"
  
  # Determine the correct filename based on category (now using .zip extension)
  if (category == "distribution_sex") {
    filename <- paste0("distribution_sex/distribution_sex_", level, ".zip")
  } else if (category == "distribution_sex_age") {
    filename <- paste0("distribution_sex_age/distribution_sex_age_", level, ".zip")
  } else if (category == "distribution_sex_nationality") {
    filename <- paste0("distribution_sex_nationality/distribution_sex_nationality_", level, ".zip")
  } else {
    filename <- paste0(category, "/", category, "_", level, ".zip")
  }
  
  url <- file.path(base_url, filename)
  
  # Set up caching (still using .csv for cached files)
  if (cache) {
    cache_file <- file.path(cache_dir, paste0("ineatlas_", category, "_", level, ".csv"))
    
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
    # Download with a timeout of 60 seconds
    options(timeout = 60)
    
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