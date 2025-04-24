#*------------------------------------------------------------------------------
#* Author: Pablo García Guzmán
#* Project: ineAtlas
#* This script: Scrapes data from INE website
#*------------------------------------------------------------------------------

packages_to_load <- c(
    "tidyverse", 
    "haven", 
    "countrycode",
    "rvest",
    "RSelenium"
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

raw <- paste0(root, "/raw/")
proc <- paste0(root, "/proc/")

# Script starts ------

# Function to kill existing Selenium server processes
cleanup_selenium <- function() {
  if (.Platform$OS.type == "windows") {
    system("taskkill /F /IM java.exe", show.output.on.console = FALSE)
  } else {
    system("pkill -f 'java.*selenium'", ignore.stdout = TRUE, ignore.stderr = TRUE)
  }
  # Wait a bit to ensure processes are terminated
  Sys.sleep(0.25)
}
cleanup_selenium() 

# Set up Firefox profile for downloads
fprof <- makeFirefoxProfile(list(
  browser.download.dir = paste0(raw, "/raw/"),
  browser.download.folderList = 2,
  browser.download.useDownloadDir = TRUE,
  browser.download.viewableInternally.enabledTypes = "",
  browser.helperApps.neverAsk.saveToDisk = "text/csv,application/csv,text/plain",
  browser.download.manager.showWhenStarting = FALSE
))

# Start the Selenium server with error handling
tryCatch({
  rD <- rsDriver(browser = "firefox", chromever = NULL, port = 4445L, extraCapabilities = fprof)
  remDr <- rD$client
}, error = function(e) {
  message("Error starting Selenium server: ", e$message)
  # You might want to add additional error handling here
  stop("Unable to start Selenium server")
})

# Navigate to the main page
remDr$navigate("https://www.ine.es/dynt3/inebase/index.htm?padre=7132")

Sys.sleep(0.25) 

# Function to click and wait
click_and_wait <- function(element) {
  element$clickElement()
  Sys.sleep(0.25)  # Wait for 2 seconds after clicking
}

# Function to handle downloads for all buttons in a sub-element
handle_downloads <- function(province_index) {
  download_buttons <- remDr$findElements("css", ".ii-download.ii-circle")
  
  if (length(download_buttons) > 0) {
    for (i in seq_along(download_buttons)) {
      tryCatch({
        # Get fresh reference to buttons for each iteration
        current_buttons <- remDr$findElements("css", ".ii-download.ii-circle")
        if (length(current_buttons) >= i) {
          handle_single_download(current_buttons[[i]], province_index)  # Pass province_index here
        }
        
        # Navigate back and wait
        remDr$navigate("https://www.ine.es/dynt3/inebase/index.htm?padre=7132")
        Sys.sleep(3)
        
        # Re-click the correct province using the index
        province_elements <- remDr$findElements("css", ".secciones > li > a")
        if (length(province_elements) >= province_index) {
          province_name <- tryCatch({
            province_elements[[province_index]]$getElementText()[[1]]
          }, error = function(e) {
            "Unknown"
          })
          cat("Re-clicking province", province_index, ":", province_name, "\n")
          click_and_wait(province_elements[[province_index]])
        }
      }, error = function(e) {
        cat("Error processing download button:", conditionMessage(e), "\n")
        
        # Try to recover by navigating back
        tryCatch({
          remDr$navigate("https://www.ine.es/dynt3/inebase/index.htm?padre=7132")
          Sys.sleep(3)
          # Re-click the correct province after error recovery
          province_elements <- remDr$findElements("css", ".secciones > li > a")
          if (length(province_elements) >= province_index) {
            click_and_wait(province_elements[[province_index]])
          }
        }, error = function(e) {
          cat("Error recovering from failed download:", conditionMessage(e), "\n")
        })
      })
    }
  } else {
    cat("No download buttons found\n")
  }
}

# Function to handle the download for a single button
handle_single_download <- function(download_button, province_index) {  # Add province_index parameter
  tryCatch({
    # Click the download button and wait for iframe
    click_and_wait(download_button)
    Sys.sleep(2)
    
    # Switch to iframe
    iframe <- remDr$findElement("css", "#thickBoxINEfrm")
    remDr$switchToFrame(iframe)
    
    # Get the CSV link href
    tryCatch({
      csv_link <- remDr$findElement("css", "a[title='CSV: separado por tabuladores']")
      href <- csv_link$getElementAttribute("href")[[1]]
      cat("\nFound CSV link with href:", href, "\n")
      
      # Extract filename from href
      filename <- basename(strsplit(href, "\\?")[[1]][1])
      download_path <- file.path(raw, filename)

      # Use the href directly as it's already a full URL
      download_url <- href
      
      cat("\nAttempting to download from URL:", download_url, "\n")
      cat("To path:", download_path, "\n")
      
      # Try to get response headers first
      headers <- tryCatch({
        con <- url(download_url)
        h <- try(curlGetHeaders(download_url))
        close(con)
        cat("\nResponse headers:\n")
        print(h)
        h
      }, error = function(e) {
        cat("\nError getting headers:", conditionMessage(e), "\n")
        NULL
      })
      
      # Attempt download with verbose output
      result <- tryCatch({
        download.file(
          url = download_url,
          destfile = download_path,
          method = "auto",
          mode = "wb",
          quiet = FALSE,
          cacheOK = FALSE,
          extra = list(verbose = TRUE)
        )
        
        # Check if file exists and has content
        if (file.exists(download_path)) {
          file_size <- file.size(download_path)
          cat("\nFile downloaded. Size:", file_size, "bytes\n")
          if (file_size == 0) {
            cat("Warning: Downloaded file is empty\n")
          }
        } else {
          cat("Error: File was not created at", download_path, "\n")
        }
        
        return(result)
      }, error = function(e) {
        cat("\nError during download:", conditionMessage(e), "\n")
        
        # Try alternative download method
        cat("\nTrying alternative download method with curl...\n")
        tryCatch({
          library(curl)
          h <- new_handle()
          handle_setopt(h, verbose = TRUE)
          curl_download(download_url, download_path, handle = h)
          cat("Curl download completed\n")
        }, error = function(e) {
          cat("Curl download also failed:", conditionMessage(e), "\n")
        })
      })
      
    }, error = function(e) {
      cat("Error getting CSV link:", conditionMessage(e), "\n")
    })
    
    # Switch back to main frame
    remDr$switchToFrame(NULL)
    
  }, error = function(e) {
    cat("Error in download process:", conditionMessage(e), "\n")
    
    # Always try to switch back to main frame
    tryCatch({
      remDr$switchToFrame(NULL)
    }, error = function(e) {
      cat("Error switching back to main frame:", conditionMessage(e), "\n")
    })
  })
  
  # Wait before proceeding to next action
  Sys.sleep(2)
}

# Get all province elements
province_elements <- remDr$findElements("css", ".secciones > li > a")

# Print available provinces for debugging
cat("Found", length(province_elements), "provinces\n")
for (i in seq_along(province_elements)) {
  tryCatch({
    name <- province_elements[[i]]$getElementText()[[1]]
    cat("Province", i, ":", name, "\n")
  }, error = function(e) {
    cat("Error getting name for province", i, ":", conditionMessage(e), "\n")
  })
}

# Ask which province to start from
start_from <- 11  # Skip first province

# Loop through provinces starting from selected index
for (i in start_from:length(province_elements)) {
  tryCatch({
    cat("\n--- Processing province", i, "---\n")
    
    # Get fresh list of province elements (to avoid stale references)
    province_elements <- remDr$findElements("css", ".secciones > li > a")
    
    # Get province name for logging
    province_name <- tryCatch({
      province_elements[[i]]$getElementText()[[1]]
    }, error = function(e) {
      "Unknown"
    })
    
    cat("Clicking province:", province_name, "\n")
    
    # Click the province and handle downloads
    click_and_wait(province_elements[[i]])
    handle_downloads(i)  # Pass the province index here
    
    # Navigate back to main page
    cat("Navigating back to main page...\n")
    remDr$navigate("https://www.ine.es/dynt3/inebase/index.htm?padre=7132")
    Sys.sleep(3)  # Wait longer after navigation
    
  }, error = function(e) {
    cat("Error processing province", i, ":", conditionMessage(e), "\n")
    
    # Try to recover by navigating back to main page
    tryCatch({
      cat("Attempting to recover by navigating back...\n")
      remDr$navigate("https://www.ine.es/dynt3/inebase/index.htm?padre=7132")
      Sys.sleep(3)
    }, error = function(e2) {
      cat("Error during recovery:", conditionMessage(e2), "\n")
    })
  })
}

# Close the browser
remDr$close()

# Stop the Selenium server
rD[["server"]]$stop()
