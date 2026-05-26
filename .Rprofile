# =============================================================================
# .Rprofile — OncoStratify Development Environment
#
# This file runs automatically when R starts in this directory.
# It sets up the development environment and provides helpful functions.
#
# =============================================================================

# Unset SHINY_SERVER_VERSION to prevent internal Shiny version comparison errors in Docker/CI
Sys.setenv(SHINY_SERVER_VERSION = "")

# Set working directory safely
if (interactive() && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  tryCatch({
    setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  }, error = function(e) {
    # Ignore error
  })
}

# Custom console message
.onLoad <- function(libname, pkgname) {
  cat("\n")
  cat("╔════════════════════════════════════════════════════════════╗\n")
  cat("║       OncoStratify — Development Environment               ║\n")
  cat("║                                                            ║\n")
  cat("║  Run the app:                                             ║\n")
  cat("║    shiny::runApp()                                        ║\n")
  cat("║                                                            ║\n")
  cat("║  Useful commands:                                         ║\n")
  cat("║    ?start_dev()      — development tips                  ║\n")
  cat("║    ?test()           — run tests                         ║\n")
  cat("║    ?build_docker()   — build Docker image                ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n\n")
}

# Helper function: Start development
start_dev <- function() {
  cat("
  OncoStratify Development Guide
  ═══════════════════════════════════════════════════════════
  
  1. RUN THE APP
     shiny::runApp()
     
  2. VIEW LOGS
     tail(readLines('logs/oncostratify_log.txt'), 20)
     
  3. RUN TESTS
     testthat::test_dir('tests/')
     
  4. BUILD DOCKER
     system('docker build -t oncostratify:dev .')
     
  5. VIEW CONFIG
     source('R/config.R')
     ls()  # List all constants
     
  6. EDIT A MODULE
     file.edit('R/ui.R')    # Page layout
     file.edit('R/server.R')  # Logic
     file.edit('R/themes.R')  # Styling
     
  7. CHECK CODE STYLE
     styler::style_file('R/ui.R')
     
  ═══════════════════════════════════════════════════════════
  ")
  invisible(NULL)
}

# Helper function: Run tests
test <- function() {
  if (!require("testthat", quietly = TRUE)) {
    install.packages("testthat")
  }
  testthat::test_dir("tests/")
}

# Helper function: Build Docker image
build_docker <- function(tag = "dev") {
  version <- "2.0"
  image_name <- sprintf("oncostratify:%s", tag)
  
  cat(sprintf("Building Docker image: %s\n", image_name))
  
  system(sprintf('docker build -t %s .', image_name))
  
  cat("\n")
  cat(sprintf("✓ Image built! Run with:\n"))
  cat(sprintf("  docker run -p 3838:3838 %s\n", image_name))
  
  invisible(NULL)
}

# Helper function: Quick preview of configuration
show_config <- function() {
  cat("
  OncoStratify Configuration
  ═══════════════════════════════════════════════════════════
  ")
  
  source("R/config.R", local = FALSE)
  
  cat(sprintf("  App Version: %s\n", APP_VERSION))
  cat(sprintf("  Features: %d biomarkers loaded\n", length(FEATURE_META)))
  cat(sprintf("  Data: %d simulated patients\n", N_PATIENTS))
  cat(sprintf("  Default theme: %s\n", DEFAULT_THEME_MODE))
  
  cat("\n  Biomarkers available:\n")
  for (name in names(FEATURE_META)) {
    cat(sprintf("    • %s (%s)\n", FEATURE_META[[name]]$label, name))
  }
  
  cat("\n═══════════════════════════════════════════════════════════\n\n")
  
  invisible(NULL)
}

# Make helper functions available
if (interactive()) {
  cat("✓ Development environment ready\n")
  cat("  Type: start_dev() for help\n\n")
}

# Useful options for development
options(
  width = 120,                          # Wider console output
  scipen = 999,                         # No scientific notation
  show.error.locations = TRUE,          # Show error locations
  warning.length = 8170                 # Longer warning messages
)

# Load custom functions into global environment
.GlobalEnv$start_dev <- start_dev
.GlobalEnv$test <- test
.GlobalEnv$build_docker <- build_docker
.GlobalEnv$show_config <- show_config
