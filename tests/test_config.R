# Unit tests for config.R module

source_r_file <- function(file) {
  path <- file.path("R", file)
  if (file.exists(path)) {
    source(path, local = FALSE)
  } else {
    source(file.path("..", path), local = FALSE)
  }
}

test_that("Configuration loads successfully", {
  skip_if_not_installed("testthat")
  
  source_r_file("config.R")
  
  # Check app metadata
  expect_true(exists("APP_NAME"))
  expect_true(exists("APP_VERSION"))
  
  # Check colours
  expect_true(exists("DARK"))
  expect_true(exists("LIGHT"))
  expect_true(exists("COLOUR_HIGH"))
  expect_true(exists("COLOUR_LOW"))
})

test_that("Feature metadata is complete", {
  skip_if_not_installed("testthat")
  
  source_r_file("config.R")
  
  # Check FEATURE_META exists
  expect_true(exists("FEATURE_META"))
  expect_true(is.list(FEATURE_META))
  
  # Check each feature has required fields
  for (gene in names(FEATURE_META)) {
    meta <- FEATURE_META[[gene]]
    expect_true("label" %in% names(meta))
    expect_true("unit" %in% names(meta))
    expect_true("type" %in% names(meta))
    expect_true("description" %in% names(meta))
  }
})

test_that("Split methods are defined", {
  skip_if_not_installed("testthat")
  
  source_r_file("config.R")
  
  expect_true(exists("SPLIT_METHODS"))
  expect_true(is.list(SPLIT_METHODS))
  expect_true(length(SPLIT_METHODS) >= 3)  # At least 3 methods
  expect_true("median" %in% names(SPLIT_METHODS))
  expect_true("tertile" %in% names(SPLIT_METHODS))
  expect_true("quartile" %in% names(SPLIT_METHODS))
})

test_that("Feature flags are boolean", {
  skip_if_not_installed("testthat")
  
  source_r_file("config.R")
  
  expect_true(is.logical(USE_REAL_DATA))
})
