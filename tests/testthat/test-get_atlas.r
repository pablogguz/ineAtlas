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
  
  # Convert to CSV string
  csv_content <- paste(
    capture.output(write.csv(data, row.names = FALSE)),
    collapse = "\n"
  )
  return(csv_content)
}

# Helper function to create mock response
create_mock_response <- function(category, level, status_code = 200) {
  structure(
    list(
      status_code = status_code,
      content = charToRaw(create_mock_csv(category, level))
    ),
    class = "response"
  )
}

# Test cases
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

test_that("get_atlas constructs correct URLs", {
  expected_url <- "https://raw.githubusercontent.com/pablogguz/ineAtlas.data/main/income/income_municipality.csv"
  
  # Store the URL that GET is called with
  called_url <- NULL
  stub(
    get_atlas,
    'httr::GET',
    function(url) {
      called_url <<- url
      create_mock_response("income", "municipality")
    }
  )
  
  get_atlas("income", "municipality", cache = FALSE)
  expect_equal(called_url, expected_url)
})

test_that("get_atlas handles caching correctly", {
  temp_cache <- tempfile()
  dir.create(temp_cache)
  on.exit(unlink(temp_cache, recursive = TRUE))
  
  # Count GET calls
  get_calls <- 0
  stub(
    get_atlas,
    'httr::GET',
    function(url) {
      get_calls <<- get_calls + 1
      create_mock_response("income", "municipality")
    }
  )
  
  # First call should download and cache
  result1 <- get_atlas("income", "municipality", cache = TRUE, cache_dir = temp_cache)
  expect_true(file.exists(file.path(temp_cache, "ineatlas_income_municipality.csv")))
  expect_equal(get_calls, 1)
  
  # Second call should use cache
  result2 <- get_atlas("income", "municipality", cache = TRUE, cache_dir = temp_cache)
  expect_equal(get_calls, 1)  # GET should not have been called again
  expect_equal(result1, result2)
})

test_that("get_atlas returns correct data structure", {
  stub(
    get_atlas,
    'httr::GET',
    function(url) create_mock_response("income", "municipality")
  )
  
  result <- get_atlas("income", "municipality", cache = FALSE)
  expect_s3_class(result, "data.frame")
  expect_true(all(c("mun_code", "mun_name", "year") %in% names(result)))
})

test_that("get_atlas handles different geographic levels correctly", {
  # Test district level
  stub(
    get_atlas,
    'httr::GET',
    function(url) create_mock_response("income", "district")
  )
  
  district_data <- get_atlas("income", "district", cache = FALSE)
  expect_true("district_code" %in% names(district_data))
  
  # Test section level
  stub(
    get_atlas,
    'httr::GET',
    function(url) create_mock_response("income", "section")
  )
  
  section_data <- get_atlas("income", "section", cache = FALSE)
  expect_true(all(c("district_code", "section_code") %in% names(section_data)))
})

test_that("get_atlas handles different data categories correctly", {
  # Test demographics data
  stub(
    get_atlas,
    'httr::GET',
    function(url) create_mock_response("demographics", "municipality")
  )
  
  demo_data <- get_atlas("demographics", "municipality", cache = FALSE)
  expect_true("population" %in% names(demo_data))
  
  # Test income sources data
  stub(
    get_atlas,
    'httr::GET',
    function(url) create_mock_response("income_sources", "municipality")
  )
  
  sources_data <- get_atlas("income_sources", "municipality", cache = FALSE)
  expect_true("wage" %in% names(sources_data))
})