#' @srrstats {G5.1} Testing basic functionality
#' @srrstats {G5.2} Testing error conditions
#' @srrstats {G5.3} Testing edge cases
#' @srrstats {SP6.0} Testing sf output

test_that("get_tract_geom validates input parameters", {
  # Invalid year
  expect_error(
    get_tract_geom(2014),
    "Year must be between 2015 and 2025"
  )
  expect_error(
    get_tract_geom(2026),
    "Year must be between 2015 and 2025"
  )
})

test_that("get_tract_geom fails gracefully when the download fails", {
  mockery::stub(get_tract_geom, "httr::GET", function(...) stop("Could not resolve host"))
  expect_message(
    result <- get_tract_geom(2015, cache = FALSE),
    "Failed to download geometries"
  )
  expect_null(result)
})

test_that("get_tract_geom returns correct object type", {
  skip_on_cran()
  skip_if_offline()
  
  # Test with minimal data to keep test fast
  tracts <- get_tract_geom(2015)
  
  # Check object type
  expect_true(inherits(tracts, "sf"))
  
  # Check required columns with correct geometry column name
  required_cols <- c("year", "tract_code", "municipality", "province", "geom")
  actual_cols <- names(tracts)
  
  expect_true(
    all(required_cols %in% actual_cols),
    info = "Not all required columns present in output"
  )
})

test_that("get_tract_geom caching works", {
  skip_on_cran()
  skip_if_offline()
  
  # Create temp cache dir
  temp_cache <- tempfile()
  dir.create(temp_cache)
  
  # First call should download
  tracts1 <- get_tract_geom(2015, cache = TRUE, cache_dir = temp_cache)
  
  # Second call should use cache
  tracts2 <- get_tract_geom(2015, cache = TRUE, cache_dir = temp_cache)
  
  # Both should be identical
  expect_identical(tracts1, tracts2)
  
  # Clean up
  unlink(temp_cache, recursive = TRUE)
})