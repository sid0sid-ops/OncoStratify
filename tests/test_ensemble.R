# Unit tests for ensemble.R module

source_r_file <- function(file) {
  path <- file.path("R", file)
  if (file.exists(path)) {
    source(path, local = FALSE)
  } else {
    source(file.path("..", path), local = FALSE)
  }
}

test_that("Ensemble Stacking training is successful", {
  skip_if_not_installed("testthat")
  
  source_r_file("config.R")
  source_r_file("logging.R")
  source_r_file("data.R")
  source_r_file("ensemble.R")
  
  # Train ensemble
  ensemble <- train_stacked_ensemble(COHORT_DATA, k = 3) # Use k=3 for faster tests
  
  # Assert structure
  expect_type(ensemble, "list")
  expect_s3_class(ensemble$full_cox, "coxph")
  expect_s3_class(ensemble$full_rsf, "rfsrc")
  expect_s3_class(ensemble$meta_fit, "coxph")
  
  # Assert weights and C-indexes
  expect_true(unname(ensemble$trust_cox) >= 0 && unname(ensemble$trust_cox) <= 100)
  expect_true(unname(ensemble$trust_rsf) >= 0 && unname(ensemble$trust_rsf) <= 100)
  expect_equal(unname(ensemble$trust_cox + ensemble$trust_rsf), 100, tolerance = 0.1)
  
  expect_true(ensemble$c_index_cox >= 0.4 && ensemble$c_index_cox <= 1.0)
  expect_true(ensemble$c_index_rsf >= 0.4 && ensemble$c_index_rsf <= 1.0)
  expect_true(ensemble$c_index_meta >= 0.4 && ensemble$c_index_meta <= 1.0)
})

test_that("Ensemble Stacking inference is correct", {
  skip_if_not_installed("testthat")
  
  source_r_file("config.R")
  source_r_file("logging.R")
  source_r_file("data.R")
  source_r_file("ensemble.R")
  
  ensemble <- train_stacked_ensemble(COHORT_DATA, k = 3)
  
  # Create a dummy new patient
  new_pt <- data.frame(
    HPV_E6E7         = 8.5,
    Tobacco_Exposure = 20.0,
    TP53             = 7.2,
    CD8A             = 8.1,
    EGFR             = 9.3,
    MKI67            = 8.4,
    TumorPurity      = 0.75,
    MutationBurden   = 15
  )
  
  pred <- predict_stacked_ensemble(ensemble, new_pt)
  
  # Assert prediction outputs
  expect_type(pred, "list")
  expect_type(pred$unified_risk, "double")
  expect_length(pred$unified_risk, 1)
  
  # Check survival probabilities range
  expect_true(pred$surv_1yr >= 0 && pred$surv_1yr <= 100)
  expect_true(pred$surv_3yr >= 0 && pred$surv_3yr <= 100)
  expect_true(pred$surv_5yr >= 0 && pred$surv_5yr <= 100)
  
  # Check monotonic decrease of survival over time
  expect_true(pred$surv_1yr >= pred$surv_3yr)
  expect_true(pred$surv_3yr >= pred$surv_5yr)
})
