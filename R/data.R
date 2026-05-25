# =============================================================================
# R/data.R — Patient cohort data
# =============================================================================

#' Generate simulated TCGA-like patient cohort
#' @param n_patients number of patients
#' @param seed for reproducibility
#' @return data.frame with survival data and biomarkers
generate_cohort <- function(n_patients = 200, seed = 42) {
  set.seed(seed)
  
  # 1. Base Etiology Factors
  # HPV status (log2 E6/E7-like expression or indicator)
  # Tobacco exposure (in pack-years, highly right-skewed)
  hpv_status <- rbinom(n_patients, 1, 0.35) # 35% HPV-positive (common in oral/oropharynx)
  tobacco_exp <- ifelse(hpv_status == 1, 
                        rlnorm(n_patients, meanlog = 1.2, sdlog = 0.8), # HPV+ patients typically have low tobacco exposure
                        rlnorm(n_patients, meanlog = 3.2, sdlog = 0.7)) # HPV- patients have high tobacco exposure
  tobacco_exp <- pmax(0, pmin(120, round(tobacco_exp, 1)))
  
  # HPV E6/E7 viral gene expression
  hpv_expr <- ifelse(hpv_status == 1, rnorm(n_patients, mean = 9.5, sd = 1.5), rnorm(n_patients, mean = 0.5, sd = 0.4))
  hpv_expr <- pmax(0, pmin(15, hpv_expr))
  
  # 2. Survival time generation (highly dependent on HPV and Tobacco)
  # Base survival lambda
  # HPV+ has much better survival. Tobacco-high has much worse survival.
  hazard_ratio <- exp(0.015 * tobacco_exp - 1.2 * hpv_status)
  base_time <- rexp(n_patients, rate = 1 / 1800)
  time <- base_time / hazard_ratio
  # Clamp follow-up between 100 and 2500 days
  time <- pmax(100, pmin(2500, time))
  
  # Event status: 1 = death, 0 = censored
  # Higher probability of event if survival is short
  event_prob <- plogis(0.5 - 0.001 * time + 0.5 * (1 - hpv_status) + 0.01 * tobacco_exp)
  status <- rbinom(n_patients, 1, event_prob)
  
  # 3. Biomarkers (TP53, CD8A, EGFR, MKI67) correlated with etiology
  # TP53: Mutated/High expression is highly correlated with Tobacco
  tp53_expr <- rnorm(n_patients, mean = 6.0, sd = 1.5) + (tobacco_exp * 0.04) - (hpv_status * 1.5)
  tp53_expr <- pmax(0, pmin(16, tp53_expr))
  
  # CD8A: Immune infiltration is much higher in HPV+ tumors
  cd8a_expr <- rnorm(n_patients, mean = 7.0, sd = 1.8) + (hpv_status * 2.2) - (tobacco_exp * 0.01)
  cd8a_expr <- pmax(0, pmin(16, cd8a_expr))
  
  # EGFR: Growth receptor overexpressed in tobacco-driven oral cancers
  egfr_expr <- rnorm(n_patients, mean = 9.0, sd = 1.5) + (tobacco_exp * 0.03) - (hpv_status * 0.8)
  egfr_expr <- pmax(0, pmin(16, egfr_expr))
  
  # MKI67: High proliferation
  mki67_expr <- rnorm(n_patients, mean = 8.0, sd = 2.0) + (status * 1.5) + (tobacco_exp * 0.02)
  mki67_expr <- pmax(0, pmin(16, mki67_expr))
  
  # 4. Clinical scores
  # Tumor purity
  purity <- runif(n_patients, 0.45, 0.92) - (cd8a_expr * 0.01) # higher immune infiltration lowers purity
  purity <- pmax(0.2, pmin(0.98, purity))
  
  # Tumor mutational burden (TMB): highly elevated by tobacco carcinogens
  tmb <- rpois(n_patients, lambda = 3 + (tobacco_exp * 0.2) + (status * 2))
  tmb <- pmax(1, tmb)
  
  data <- data.frame(
    patient_id       = sprintf("PAT-%04d", seq_len(n_patients)),
    time             = round(time, 1),
    status           = status,
    HPV_E6E7         = round(hpv_expr, 2),
    Tobacco_Exposure = round(tobacco_exp, 1),
    TP53             = round(tp53_expr, 2),
    CD8A             = round(cd8a_expr, 2),
    EGFR             = round(egfr_expr, 2),
    MKI67            = round(mki67_expr, 2),
    TumorPurity      = round(purity, 3),
    MutationBurden   = tmb
  )
  
  data
}

# Generate both cohorts
GLOBAL_COHORT <- generate_cohort(n_patients = 200, seed = 42)
INDIAN_COHORT <- generate_cohort(n_patients = 46, seed = 101)

# Default active cohort for backward compatibility
COHORT_DATA <- GLOBAL_COHORT

cat("✓ Data module loaded:", nrow(GLOBAL_COHORT), "Global and", nrow(INDIAN_COHORT), "Indian patients\n")