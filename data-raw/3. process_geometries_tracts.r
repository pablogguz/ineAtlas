#*------------------------------------------------------------------------------
#* Author: Pablo García Guzmán
#* Project: ineAtlas
#* This script: Process census tract boundary files for the ineAtlas.data repository
#*------------------------------------------------------------------------------

# Load required packages
packages_to_load <- c(
    "tidyverse",
    "sf",
    "zip"
)

package.check <- lapply(
    packages_to_load,
    FUN = function(x) {
        if (!require(x, character.only = TRUE)) {
            install.packages(x, dependencies = TRUE)
        }
    }
)

lapply(packages_to_load, require, character = TRUE)

# Set paths 
username <- Sys.getenv("USERNAME")

root <- paste0("C:/Users/", username, "/Dropbox/ineAtlas_data/")
gitdata <- paste0("C:/Users/", username, "/Documents/GitHub/ineAtlas.data/data/")

raw <- paste0(root, "/raw_geometries/")
out_dir <- file.path(gitdata, "geometries")

# Create output directory if it doesn't exist
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Years to process
years <- 2015:2022

# Function to process a single year's shapefile
process_shapefile <- function(year) {
    cat(sprintf("\nProcessing year %d...", year))
    
    # Input zip file path
    zip_file <- file.path(raw, sprintf("seccionado_%d.zip", year))
    
    if (!file.exists(zip_file)) {
        warning(sprintf("File for year %d not found: %s", year, zip_file))
        return(NULL)
    }
    
    # Create temporary directory for extraction
    temp_dir <- tempfile()
    dir.create(temp_dir)
    
    tryCatch({
        # Extract zip file
        cat("\nExtracting zip file...")
        unzip(zip_file, exdir = temp_dir)
        
        # Find the shapefile
        shp_file <- list.files(temp_dir, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)[1]
        
        if (is.na(shp_file)) {
            warning(sprintf("No shapefile found in zip for year %d", year))
            return(NULL)
        }
        
        # Read shapefile
        cat("\nReading shapefile...")
        sf_data <- st_read(shp_file, quiet = TRUE)
        
        # Clean and standardize the data
        cat("\nCleaning data...")
        sf_data <- sf_data %>%
            # Add year column
            mutate(year = year) %>%
            # Select and rename relevant columns
            select(
                year,
                tract_code = CUSEC,  # Census tract code
                municipality = NMUN,  # Municipality name
                province = NPRO      # Province name
            ) %>%
            # Ensure valid geometry
            st_make_valid()
        
        # Create output file path
        out_file <- file.path(out_dir, sprintf("census_tracts_%d.gpkg", year))
        
        # Save as GeoPackage
        cat("\nSaving to GeoPackage...")
        st_write(sf_data, out_file, delete_layer = TRUE)
        
        # Compress the GeoPackage
        cat("\nCompressing...")
        zip_file_out <- paste0(out_file, ".zip")
        zip::zip(
            zipfile = zip_file_out,
            files = out_file,
            mode = "cherry-pick"
        )
        
        # Remove uncompressed file
        unlink(out_file)
        
        # Report file sizes
        orig_size <- file.size(out_file)
        zip_size <- file.size(zip_file_out)
        compression_ratio <- (1 - zip_size / orig_size) * 100
        
        cat(sprintf(
            "\nCompressed %d: %.1f MB -> %.1f MB (%.1f%% reduction)",
            year, orig_size / 1e6, zip_size / 1e6, compression_ratio
        ))
        
        return(TRUE)
        
    }, error = function(e) {
        warning(sprintf("Error processing year %d: %s", year, e$message))
        return(NULL)
        
    }, finally = {
        # Clean up temporary directory
        unlink(temp_dir, recursive = TRUE)
    })
}

# Process all years
results <- lapply(years, process_shapefile)

# Report summary
successful_years <- sum(!sapply(results, is.null))
cat(sprintf("\n\nProcessing complete. Successfully processed %d out of %d years.",
            successful_years, length(years)))