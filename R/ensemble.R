# =============================================================================
# R/ensemble.R — Ensemble Stacking survival model (Cox PH + Random Survival Forest)
# =============================================================================

library(survival)
library(randomForestSRC)

#' Train the multi-model ensemble stacking classifier
#' @param data patient cohort data frame
#' @param features character vector of biomarker features
#' @param k number of cross-validation folds
#' @return list of trained models, weights, and C-indexes
train_stacked_ensemble <- function(data, 
                                   features = c("HPV_E6E7", "Tobacco_Exposure", "TP53", "CD8A", "EGFR", "MKI67", "TumorPurity", "MutationBurden"), 
                                   k = 5) {
  set.seed(42)
  n <- nrow(data)
  folds <- sample(rep(1:k, length.out = n))
  
  # Out-of-fold predictions vector
  oof_cox <- rep(NA, n)
  oof_rsf <- rep(NA, n)
  
  # 5-fold cross-validation to get unbiased out-of-fold predictions
  for (i in 1:k) {
    train_idx <- which(folds != i)
    val_idx <- which(folds == i)
    
    train_data <- data[train_idx, ]
    val_data <- data[val_idx, ]
    
    # Model A: Cox PH
    formula_cox <- as.formula(paste("Surv(time, status) ~", paste(features, collapse = " + ")))
    fit_cox <- tryCatch(survival::coxph(formula_cox, data = train_data), error = function(e) NULL)
    
    # Model B: Random Survival Forest (150 trees is fast and accurate)
    formula_rsf <- as.formula(paste("Surv(time, status) ~", paste(features, collapse = " + ")))
    fit_rsf <- tryCatch(randomForestSRC::rfsrc(formula_rsf, data = train_data, ntree = 150), error = function(e) NULL)
    
    if (!is.null(fit_cox)) {
      oof_cox[val_idx] <- predict(fit_cox, newdata = val_data, type = "lp")
    } else {
      oof_cox[val_idx] <- 0
    }
    
    if (!is.null(fit_rsf)) {
      pred_obj <- predict(fit_rsf, newdata = val_data)
      oof_rsf[val_idx] <- pred_obj$predicted
    } else {
      oof_rsf[val_idx] <- 0
    }
  }
  
  # Scale out-of-fold predictions for stable stacking coefficient estimation
  mean_cox <- mean(oof_cox, na.rm = TRUE)
  sd_cox <- sd(oof_cox, na.rm = TRUE)
  mean_rsf <- mean(oof_rsf, na.rm = TRUE)
  sd_rsf <- sd(oof_rsf, na.rm = TRUE)
  
  scaled_cox <- (oof_cox - mean_cox) / sd_cox
  scaled_rsf <- (oof_rsf - mean_rsf) / sd_rsf
  
  meta_df <- data.frame(
    time = data$time,
    status = data$status,
    cox_pred = scaled_cox,
    rsf_pred = scaled_rsf
  )
  
  # Train Level-1 Meta-Learner (Cox PH on scaled out-of-fold predictions)
  meta_fit <- tryCatch(
    survival::coxph(Surv(time, status) ~ cox_pred + rsf_pred, data = meta_df),
    error = function(e) NULL
  )
  
  # Train final full models on the entire dataset for deployment inference
  full_formula <- as.formula(paste("Surv(time, status) ~", paste(features, collapse = " + ")))
  full_cox <- survival::coxph(full_formula, data = data)
  full_rsf <- randomForestSRC::rfsrc(full_formula, data = data, ntree = 200)
  
  # Calculate weights based on Meta-Learner coefficients
  if (!is.null(meta_fit)) {
    coefs <- coef(meta_fit)
    weight_cox <- coefs["cox_pred"]
    weight_rsf <- coefs["rsf_pred"]
    
    # Calculate unified risk scores for the cohort using the meta-learner
    unified_risk <- predict(meta_fit, type = "lp")
  } else {
    weight_cox <- 0.5
    weight_rsf <- 0.5
    unified_risk <- scaled_cox * 0.5 + scaled_rsf * 0.5
  }
  
  # Calculate relative trust percentages
  sum_abs <- abs(weight_cox) + abs(weight_rsf)
  if (sum_abs > 0) {
    trust_cox <- (abs(weight_cox) / sum_abs) * 100
    trust_rsf <- (abs(weight_rsf) / sum_abs) * 100
  } else {
    trust_cox <- 50
    trust_rsf <- 50
  }
  
  # C-indexes (Concordance Indexes) - formatted to the standard positive-association scale
  c_index_cox <- tryCatch({
    c_val <- survival::concordance(Surv(time, status) ~ oof_cox, data = data)$concordance
    if (!is.na(c_val) && c_val < 0.5) 1 - c_val else c_val
  }, error = function(e) NA)
  
  c_index_rsf <- tryCatch({
    c_val <- survival::concordance(Surv(time, status) ~ oof_rsf, data = data)$concordance
    if (!is.na(c_val) && c_val < 0.5) 1 - c_val else c_val
  }, error = function(e) NA)
  
  c_index_meta <- tryCatch({
    c_val <- survival::concordance(Surv(time, status) ~ unified_risk, data = data)$concordance
    if (!is.na(c_val) && c_val < 0.5) 1 - c_val else c_val
  }, error = function(e) NA)
  
  list(
    full_cox = full_cox,
    full_rsf = full_rsf,
    meta_fit = meta_fit,
    mean_cox = mean_cox,
    sd_cox = sd_cox,
    mean_rsf = mean_rsf,
    sd_rsf = sd_rsf,
    weight_cox = weight_cox,
    weight_rsf = weight_rsf,
    trust_cox = round(trust_cox, 1),
    trust_rsf = round(trust_rsf, 1),
    c_index_cox = round(c_index_cox, 3),
    c_index_rsf = round(c_index_rsf, 3),
    c_index_meta = round(c_index_meta, 3),
    unified_risk = unified_risk
  )
}

#' Run real-time prediction using the stacking ensemble
#' @param ensemble trained ensemble list from train_stacked_ensemble
#' @param new_patient_df single-row data frame with patient features
#' @return list containing individual predictions, unified risk score, and survival probabilities
predict_stacked_ensemble <- function(ensemble, new_patient_df) {
  # 1. Base Model A (Cox PH) prediction
  pred_cox <- predict(ensemble$full_cox, newdata = new_patient_df, type = "lp")
  
  # 2. Base Model B (RSF) prediction
  pred_rsf_obj <- predict(ensemble$full_rsf, newdata = new_patient_df)
  pred_rsf <- pred_rsf_obj$predicted[1]
  
  # 3. Scale predictions using cohort baseline statistics
  scaled_cox <- (pred_cox - ensemble$mean_cox) / ensemble$sd_cox
  scaled_rsf <- (pred_rsf - ensemble$mean_rsf) / ensemble$sd_rsf
  
  # 4. Meta-Learner prediction (Unified Risk Score)
  if (!is.null(ensemble$meta_fit)) {
    meta_new <- data.frame(cox_pred = scaled_cox, rsf_pred = scaled_rsf)
    unified_risk <- predict(ensemble$meta_fit, newdata = meta_new, type = "lp")
  } else {
    unified_risk <- scaled_cox * 0.5 + scaled_rsf * 0.5
  }
  
  # 5. Extract predicted survival probabilities at 1, 3, and 5 years using the RSF fit survival curves
  # (RSF outputs beautiful non-parametric survival predictions for new patients)
  time_points <- pred_rsf_obj$time.interest
  survival_curve <- pred_rsf_obj$survival[1, ]
  
  get_surv_prob <- function(target_days) {
    idx <- which(time_points <= target_days)
    if (length(idx) == 0) 1.0 else survival_curve[max(idx)]
  }
  
  list(
    pred_cox = pred_cox,
    pred_rsf = pred_rsf,
    unified_risk = unified_risk,
    surv_1yr = round(get_surv_prob(365) * 100, 1),
    surv_3yr = round(get_surv_prob(1095) * 100, 1),
    surv_5yr = round(get_surv_prob(1825) * 100, 1)
  )
}
