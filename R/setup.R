# =============================================================================
#
#   OncoStratify v2.0 — Automated Package Setup & Environment Initialization
#
# =============================================================================

message("\n═══════════════════════════════════════════════════════════")
message("  OncoStratify — Automated Setup")
message("═══════════════════════════════════════════════════════════\n")

# Check R version
r_version <- paste(R.version$major, R.version$minor, sep = ".")
if (numeric_version(r_version) < numeric_version("4.0.0")) {
  stop(sprintf("R 4.0.0+ required (you have %s)", r_version))
}
message(sprintf("✓ R %s", r_version))

# Required packages with fallback handling
required_packages <- c(
  "shiny", "bslib", "dplyr", "ggplot2", 
  "survival", "survminer", "DT", "shinycssloaders", "shinyjs", "bsicons", "randomForestSRC"
)

# Install missing packages silently
missing <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

if (length(missing) > 0) {
  message(sprintf("\n→ Installing %d missing package(s)...", length(missing)))
  
  for (pkg in missing) {
    tryCatch({
      install.packages(pkg, 
                      repos = "https://cran.rstudio.com",
                      quiet = TRUE)
      message(sprintf("  ✓ %s installed", pkg))
    }, error = function(e) {
      warning(sprintf("Could not install %s: %s", pkg, e$message))
    })
  }
}

# Load all packages
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(ggplot2)
  library(survival)
  library(survminer)
  library(DT)
  library(shinycssloaders)
  library(shinyjs)
  library(bsicons)
  library(randomForestSRC)
})

message("✓ All packages loaded")
