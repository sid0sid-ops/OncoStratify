# Unit tests for data.R module

source_r_file <- function(file) {
  path <- file.path("R", file)
  if (file.exists(path)) {
    source(path, local = FALSE)
  } else {
    source(file.path("..", path), local = FALSE)
  }
}

test_that("Cohort data is generated correctly", {
  skip_if_not_installed("testthat")
  
  source_r_file("config.R")
  source_r_file("logging.R")
  source_r_file("data.R")
  
  # Check data exists
  expect_true(exists("COHORT_DATA"))
  
  # Check dimensions
  expect_equal(nrow(COHORT_DATA), N_PATIENTS)
  expect_true(ncol(COHORT_DATA) > 3)  # At least patient_id, time, status + biomarkers
  
  # Check columns exist
  expect_true("patient_id" %in% names(COHORT_DATA))
  expect_true("time" %in% names(COHORT_DATA))
  expect_true("status" %in% names(COHORT_DATA))
})

test_that("Survival data is valid", {
  skip_if_not_installed("testthat")
  
  source_r_file("config.R")
  source_r_file("logging.R")
  source_r_file("data.R")
  
  # Check survival times are positive
  expect_true(all(COHORT_DATA$time > 0, na.rm = TRUE))
  
  # Check status is binary
  expect_true(all(COHORT_DATA$status %in% c(0, 1), na.rm = TRUE))
  
  # Check some events exist
  expect_true(sum(COHORT_DATA$status, na.rm = TRUE) > 0)
})

test_that("Biomarkers have valid values", {
  skip_if_not_installed("testthat")
  
  source_r_file("config.R")
  source_r_file("logging.R")
  source_r_file("data.R")
  
  biomarkers <- c("HPV_E6E7", "Tobacco_Exposure", "TP53", "CD8A", "EGFR", "MKI67")
  
  for (gene in biomarkers) {
    # Check gene exists
    expect_true(gene %in% names(COHORT_DATA))
    
    # Check values are in reasonable range (expressions are [0, 16], tobacco up to 120)
    vals <- COHORT_DATA[[gene]]
    if (gene == "Tobacco_Exposure") {
      expect_true(all(vals >= 0 & vals <= 120, na.rm = TRUE))
    } else {
      expect_true(all(vals >= 0 & vals <= 16, na.rm = TRUE))
    }
  }
})
