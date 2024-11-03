#' Get census tract boundary geometries
#' 
#' Downloads and extracts census tract boundary files from the ineAtlas repository,
#' returning an sf object with the geometries for the specified year.
#' 
#' @param year Numeric. Year of the census tract boundaries to retrieve (2015-2022)
#' @param cache Logical indicating whether to cache the extracted data. Default is TRUE.
#'   Cached data is stored uncompressed for faster access.
#' @param cache_dir Character string specifying the cache directory. Default is tempdir().
#' 
#' @return An sf object containing census tract boundaries with the following columns:
#'   \itemize{
#'     \item year: The reference year
#'     \item tract_code: Census tract identifier
#'     \item municipality: Municipality name
#'     \item province: Province name
#'     \item geometry: Census tract boundary geometry
#'   }
#' 
#' @importFrom sf st_read
#' @importFrom httr GET stop_for_status write_disk
#' @importFrom zip unzip
#' 
#' @export
#' 
#' @examples
#' \donttest{
#' # Get census tract boundaries for 2020
#' tracts_2020 <- get_tract_geom(2020)
#' 
#' # Get boundaries without caching
#' tracts_2019 <- get_tract_geom(2019, cache = FALSE)
#' }
get_tract_geom <- function(year, cache = TRUE, cache_dir = tempdir()) {
  # Validate year
  valid_years <- 2015:2022
  if (!year %in% valid_years) {
    stop("Year must be between ", min(valid_years), " and ", max(valid_years))
  }
  
  # Construct the GitHub raw content URL
  base_url <- "https://raw.githubusercontent.com/pablogguz/ineAtlas.data/main/data/geometries"
  filename <- sprintf("census_tracts_%d.gpkg.zip", year)
  url <- file.path(base_url, filename)
  
  # Set up caching (using .gpkg for cached files)
  if (cache) {
    cache_file <- file.path(cache_dir, sprintf("ineatlas_tracts_%d.gpkg", year))
    
    # Check if cached file exists and is less than 24 hours old
    if (file.exists(cache_file)) {
      file_age <- difftime(Sys.time(), file.mtime(cache_file), units = "hours")
      if (file_age < 24) {
        message("Using cached geometries from ", basename(cache_file))
        data <- sf::st_read(cache_file, quiet = TRUE)
        message("Geometries successfully loaded from cache: ", nrow(data), " features")
        return(data)
      }
    }
  }
  
  # Try to download and process the data
  tryCatch({
    message("Downloading geometries for ", year, "...")
    # Download with a timeout of 60 seconds
    options(timeout = 60)
    
    # Create temporary files for zip and gpkg
    temp_zip <- tempfile(fileext = ".zip")
    temp_dir <- tempfile()
    dir.create(temp_dir)
    
    # Download zip file
    response <- httr::GET(url, httr::write_disk(temp_zip, overwrite = TRUE))
    
    # Check for HTTP errors
    httr::stop_for_status(response)
    
    # Unzip to temporary directory
    utils::unzip(temp_zip, exdir = temp_dir)
    
    # Find the GeoPackage file
    gpkg_file <- list.files(temp_dir, pattern = "\\.gpkg$", full.names = TRUE)[1]
    if (is.na(gpkg_file)) {
      stop("No GeoPackage file found in downloaded zip")
    }
    
    # Read the GeoPackage
    data <- sf::st_read(gpkg_file, quiet = TRUE)
    
    # Clean up temporary files
    unlink(temp_zip)
    unlink(temp_dir, recursive = TRUE)
    
    # Cache the data if requested
    if (cache) {
      message("Caching geometries to ", basename(cache_file))
      dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
      sf::st_write(data, cache_file, quiet = TRUE)
    }
    
    message("Geometries successfully loaded: ", nrow(data), " features")
    
    return(data)
    
  }, error = function(e) {
    # Check for cached data if download fails
    if (cache && file.exists(cache_file)) {
      warning("Download failed, using cached geometries. Error: ", e$message)
      data <- sf::st_read(cache_file, quiet = TRUE)
      message("Geometries successfully loaded from cache: ", nrow(data), " features")
      return(data)
    } else {
      stop("Failed to download geometries and no cache available. Error: ", e$message)
    }
  })
}