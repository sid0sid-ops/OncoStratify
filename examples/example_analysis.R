# Example: Using OncoStratify programmatically
# 
# This script shows how to use OncoStratify's functions
# directly in R without the web interface

# Setup
source("R/config.R")
source("R/logging.R")
source("R/data.R")
source("R/themes.R")

library(survival)
library(survminer)
library(ggplot2)
library(dplyr)

# ===================================================================
# Example 1: Stratify by TP53 and create KM curve
# ===================================================================

log_info("Example 1: TP53 stratification")

# Get data
cohort <- COHORT_DATA

# Stratify by median TP53 expression
tp53_vals <- cohort$TP53
median_val <- median(tp53_vals, na.rm = TRUE)
cohort$tp53_group <- ifelse(tp53_vals >= median_val, "High", "Low")

# Fit Kaplan-Meier
km_fit <- survfit(Surv(time, status) ~ tp53_group, data = cohort)

# Test for difference (log-rank)
logrank_test <- survdiff(Surv(time, status) ~ tp53_group, data = cohort)
logrank_p <- 1 - pchisq(logrank_test$chisq, df = length(logrank_test$n) - 1)

# Fit Cox model
cox_model <- coxph(Surv(time, status) ~ tp53_group, data = cohort)

# Print results
cat("\n=== TP53 STRATIFICATION ===\n")
cat(sprintf("Median TP53: %.2f\n", median_val))
cat(sprintf("High group (n): %d\n", sum(cohort$tp53_group == "High")))
cat(sprintf("Low group (n): %d\n", sum(cohort$tp53_group == "Low")))
cat(sprintf("Log-rank p-value: %.4f\n", logrank_p))
cat(sprintf("Cox Hazard Ratio: %.2f\n", exp(cox_model$coefficients[1])))

# Plot
pdf("examples/tp53_km_curve.pdf", width = 10, height = 7)
ggsurvplot(
  km_fit,
  data = cohort,
  risk.table = TRUE,
  conf.int = TRUE,
  pval = TRUE,
  palette = c(High = "#f85149", Low = "#3fb950"),
  title = "TP53 Expression and Survival",
  xlab = "Time (days)",
  ylab = "Survival Probability",
  ggtheme = LIGHT_PLOT_THEME
)
dev.off()

cat("✓ Plot saved to: examples/tp53_km_curve.pdf\n")

# ===================================================================
# Example 2: Compare multiple biomarkers
# ===================================================================

log_info("Example 2: Multi-biomarker comparison")

biomarkers <- c("TP53", "MKI67", "CD8A", "FOXP3")

results <- data.frame(
  Biomarker = character(),
  P_value = numeric(),
  HR = numeric(),
  CI_lower = numeric(),
  CI_upper = numeric(),
  stringsAsFactors = FALSE
)

for (bm in biomarkers) {
  cohort_bm <- COHORT_DATA %>%
    mutate(
      group = ifelse(!!sym(bm) >= median(!!sym(bm), na.rm = TRUE), "High", "Low")
    )
  
  # Log-rank test
  test <- survdiff(Surv(time, status) ~ group, data = cohort_bm)
  pval <- 1 - pchisq(test$chisq, df = length(test$n) - 1)
  
  # Cox model
  cox <- coxph(Surv(time, status) ~ group, data = cohort_bm)
  hr <- exp(cox$coefficients[1])
  ci <- exp(confint(cox))
  
  results <- rbind(results, data.frame(
    Biomarker = bm,
    P_value = pval,
    HR = hr,
    CI_lower = ci[1, 1],
    CI_upper = ci[1, 2]
  ))
}

cat("\n=== MULTI-BIOMARKER COMPARISON ===\n")
print(results)

# Save results
write.csv(results, "examples/biomarker_comparison.csv", row.names = FALSE)
cat("\n✓ Results saved to: examples/biomarker_comparison.csv\n")

# ===================================================================
# Example 3: Tertile stratification
# ===================================================================

log_info("Example 3: Tertile stratification")

cohort_tertile <- COHORT_DATA %>%
  mutate(
    tp53_tertile = cut(TP53, 
                       breaks = quantile(TP53, c(0, 1/3, 2/3, 1)), 
                       labels = c("Low", "Mid", "High"),
                       include.lowest = TRUE)
  ) %>%
  filter(tp53_tertile %in% c("Low", "High"))  # Drop middle

# Fit models
km_tertile <- survfit(Surv(time, status) ~ tp53_tertile, data = cohort_tertile)
cox_tertile <- coxph(Surv(time, status) ~ tp53_tertile, data = cohort_tertile)

cat("\n=== TERTILE STRATIFICATION ===\n")
cat(sprintf("Remaining patients: %d\n", nrow(cohort_tertile)))
cat(sprintf("Cox HR (Low vs High): %.2f\n", exp(cox_tertile$coefficients[1])))

# ===================================================================
# Session Info
# ===================================================================

cat("\n=== SESSION INFO ===\n")
cat(sprintf("R version: %s\n", R.version$version.string))
cat(sprintf("Shiny version: %s\n", packageVersion("shiny")))
cat(sprintf("Survival version: %s\n", packageVersion("survival")))
cat(sprintf("Analysis date: %s\n", Sys.time()))

log_info("Example analysis complete")
