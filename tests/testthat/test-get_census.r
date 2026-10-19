test_that("get_census validates input parameters", {
  expect_error(
    get_census("invalid_level"),
    "Level must be one of: municipality, district, tract"
  )
})

test_that("get_census fails gracefully when the download fails", {
  mockery::stub(get_census, "httr::GET", function(...) stop("Could not resolve host"))
  expect_message(
    result <- get_census("municipality", cache = FALSE),
    "Failed to download data"
  )
  expect_null(result)
})
