# =============================================================================
#
#   OncoStratify v2.0 — Kaplan-Meier Survival Stratification App
#   A professional bioinformatics tool for survival biomarker discovery
#
#   ENTRY POINT — This is the ONLY file you need to run:
#   
#     shiny::runApp()
#
#   Everything else is AUTOMATIC:
#   ✅ Checks R version
#   ✅ Installs missing packages
#   ✅ Fixes bslib version issues
#   ✅ Loads configuration
#   ✅ Starts the app
#
# =============================================================================

# Step 1: Automated package setup (RUNS FIRST)
# ============================================

source("R/setup.R", local = FALSE)

# Register resource path for custom CSS/JS assets
shiny::addResourcePath("www", "www")

# Step 2: Load configuration and modules
# ======================================

message("\n→ Loading modules...")

# Load in correct order
source("R/config.R", local = FALSE)      # Configuration first
source("R/logging.R", local = FALSE)     # Logging
source("R/data.R", local = FALSE)        # Data
source("R/ensemble.R", local = FALSE)    # Ensemble Stacking Model
source("R/themes.R", local = FALSE)      # Themes (after config)
source("R/ux.R", local = FALSE)          # UX patterns
source("R/ui.R", local = FALSE)          # UI (after themes)
source("R/server.R", local = FALSE)      # Server (after everything)

message("✓ All modules loaded")

# Step 3: Create and run the Shiny app
# ====================================

message("\n═══════════════════════════════════════════════════════════")
message("  ✅ SETUP COMPLETE — Starting app...\n")
message("═══════════════════════════════════════════════════════════\n")

# Create app
app <- shinyApp(
  ui = app_ui(),
  server = app_server,
  onStart = function() {
    message("\n→ App started successfully!")
    message("→ Open in browser: http://localhost:3838\n")
  }
)

# Run app
shinyApp(ui = app_ui(), server = app_server)
