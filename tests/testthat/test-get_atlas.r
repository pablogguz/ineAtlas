# tests/testthat/test-get_atlas.R

library(testthat)
library(httr)
library(mockery)
library(readr)

# Helper function to create mock CSV content
create_mock_csv <- function(category, level) {
  # Base data for all types
  data <- data.frame(
    mun_code = c("28001", "28002"),
    mun_name = c("Madrid", "Barcelona"),
    year = c(2021, 2021),
    stringsAsFactors = FALSE
  )
  
  # Add level-specific columns
  if (level == "district") {
    data$district_code <- c("01", "02")
  } else if (level == "section") {
    data$district_code <- c("01", "02")
    data$section_code <- c("001", "002")
  }
  
  # Add category-specific columns
  if (category == "income") {
    data$net_income_pc <- c(25000, 24000)
  } else if (category == "demographics") {
    data$population <- c(3000000, 1600000)
  } else if (category == "income_sources") {
    data$wage <- c(70, 72)
  }
  
  # Create temporary CSV file
  temp_csv <- tempfile(fileext = ".csv")
  write.csv(data, temp_csv, row.names = FALSE)
  
  # Create temporary zip file
  temp_zip <- tempfile(fileext = ".zip")
  utils::zip(temp_zip, temp_csv, flags = "-j9X")
  
  # Read zip file as binary
  zip_content <- readBin(temp_zip, raw(), file.info(temp_zip)$size)
  
  # Clean up temporary files
  unlink(c(temp_csv, temp_zip))
  
  return(zip_content)
}

# Helper function to create mock response
create_mock_response <- function(category, level, status_code = 200) {
  structure(
    list(
      status_code = status_code,
      content = create_mock_csv(category, level)
    ),
    class = "response"
  )
}

# Mock file writing functions
mock_write_disk <- function(path, overwrite = TRUE) {
  function(x) {
    writeBin(x$content, path)
    x
  }
}

test_that("get_atlas validates input parameters correctly", {
  expect_error(
    get_atlas("invalid_category", "municipality"),
    "Category must be one of: income, income_sources, demographics"
  )
  
  expect_error(
    get_atlas("income", "invalid_level"),
    "Level must be one of: municipality, district, section"
  )
})

test_that("get_atlas constructs correct URLs for zip files", {
  expected_url <- "https://raw.githubusercontent.com/pablogguz/ineAtlas.data/main/income/income_municipality.zip"
  
  # Store the URL that GET is called with
  called_url <- NULL
  stub(
    get_atlas,
    'httr::GET',
    function(url, ...) {
      called_url <<- url
      create_mock_response("income", "municipality")
    }
  )
  
  # Mock the unzip and write_disk functions
  stub(get_atlas, 'httr::write_disk', mock_write_disk)
  stub(get_atlas, 'utils::unzip', function(...) NULL)
  
  get_atlas("income", "municipality", cache = FALSE)
  expect_equal(called_url, expected_url)
})

test_that("get_atlas handles caching correctly with zip files", {
  temp_cache <- tempfile()
  dir.create(temp_cache)
  on.exit(unlink(temp_cache, recursive = TRUE))
  
  # Count GET calls
  get_calls <- 0
  stub(
    get_atlas,
    'httr::GET',
    function(url, ...) {
      get_calls <<- get_calls + 1
      create_mock_response("income", "municipality")
    }
  )
  
  # Mock zip handling functions
  stub(get_atlas, 'httr::write_disk', mock_write_disk)
  stub(get_atlas, 'utils::unzip', function(...) NULL)
  stub(get_atlas, 'list.files', function(...) tempfile(fileext = ".csv"))
  stub(get_atlas, 'readr::read_csv', function(...) data.frame(
    mun_code = "28001",
    mun_name = "Madrid",
    year = 2021,
    net_income_pc = 25000
  ))
  
  # First call should download and cache
  result1 <- get_atlas("income", "municipality", cache = TRUE, cache_dir = temp_cache)
  expect_true(file.exists(file.path(temp_cache, "ineatlas_income_municipality.csv")))
  expect_equal(get_calls, 1)
  
  # Second call should use cache
  result2 <- get_atlas("income", "municipality", cache = TRUE, cache_dir = temp_cache)
  expect_equal(get_calls, 1)  # GET should not have been called again
  expect_equal(result1, result2)
})

test_that("get_atlas handles temporary files correctly", {
  # Count file cleanup calls
  unlink_calls <- 0
  stub(get_atlas, 'unlink', function(...) {
    unlink_calls <<- unlink_calls + 1
    NULL
  })
  
  # Mock other functions
  stub(get_atlas, 'httr::GET', function(...) create_mock_response("income", "municipality"))
  stub(get_atlas, 'httr::write_disk', mock_write_disk)
  stub(get_atlas, 'utils::unzip', function(...) NULL)
  stub(get_atlas, 'list.files', function(...) tempfile(fileext = ".csv"))
  stub(get_atlas, 'readr::read_csv', function(...) data.frame(
    mun_code = "28001",
    mun_name = "Madrid",
    year = 2021,
    net_income_pc = 25000
  ))
  
  get_atlas("income", "municipality", cache = FALSE)
  expect_gte(unlink_calls, 2)  # Should clean up both temp zip and temp directory
})

# test the data structure
test_that("get_atlas returns correct data structure", {
  stub(get_atlas, 'httr::GET', function(...) create_mock_response("income", "municipality"))
  stub(get_atlas, 'httr::write_disk', mock_write_disk)
  stub(get_atlas, 'utils::unzip', function(...) NULL)
  stub(get_atlas, 'list.files', function(...) tempfile(fileext = ".csv"))
  stub(get_atlas, 'readr::read_csv', function(...) data.frame(
    mun_code = "28001",
    mun_name = "Madrid",
    year = 2021,
    net_income_pc = 25000
  ))
  
  result <- get_atlas("income", "municipality", cache = FALSE)
  expect_s3_class(result, "data.frame")
  expect_true(all(c("mun_code", "mun_name", "year") %in% names(result)))
})

# Geographic levels test modified similarly...
test_that("get_atlas handles different geographic levels correctly", {
  # Common mocks
  stub(get_atlas, 'httr::write_disk', mock_write_disk)
  stub(get_atlas, 'utils::unzip', function(...) NULL)
  stub(get_atlas, 'list.files', function(...) tempfile(fileext = ".csv"))
  
  # Test district level
  stub(get_atlas, 'httr::GET', function(...) create_mock_response("income", "district"))
  stub(get_atlas, 'readr::read_csv', function(...) data.frame(
    mun_code = "28001",
    mun_name = "Madrid",
    district_code = "01",
    year = 2021,
    net_income_pc = 25000
  ))
  
  district_data <- get_atlas("income", "district", cache = FALSE)
  expect_true("district_code" %in% names(district_data))
  
  # Test section level
  stub(get_atlas, 'httr::GET', function(...) create_mock_response("income", "section"))
  stub(get_atlas, 'readr::read_csv', function(...) data.frame(
    mun_code = "28001",
    mun_name = "Madrid",
    district_code = "01",
    section_code = "001",
    year = 2021,
    net_income_pc = 25000
  ))
  
  section_data <- get_atlas("income", "section", cache = FALSE)
  expect_true(all(c("district_code", "section_code") %in% names(section_data)))
})