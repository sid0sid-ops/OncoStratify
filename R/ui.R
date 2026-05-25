# =============================================================================
# R/ui.R — User interface (Genomic Workstation Redesign)
# =============================================================================

app_ui <- function() {
  
  # Browser theme detection
  theme_detection_js <- tags$script(HTML("
    document.addEventListener('DOMContentLoaded', function() {
      var isDark = window.matchMedia('(prefers-color-scheme: dark)').matches;
      Shiny.setInputValue('browser_is_dark', isDark);
    });
  "))
  
  fluidPage(
    shinyjs::useShinyjs(),
    
    # Establish base bootstrap settings compatible with custom theme
    theme = bs_theme(version = 5, primary = "#7c3aed"),
    
    # Head includes for fonts, icons, custom CSS, and JS
    tags$head(
      tags$link(
        rel  = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&family=JetBrains+Mono:wght@400;700&display=swap"
      ),
      tags$link(rel = "stylesheet", href = "custom.css"),
      theme_detection_js,
      ux_css(),
      ux_js(FEATURE_META),
      tags$script(src = "custom.js"),
      tags$meta(charset = "UTF-8"),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")
    ),
    
    tags$div(
      class = "app-container d-flex flex-column",
      style = "min-height: 100vh;",
      
      # == TOP NAVIGATION BAR ==
      tags$div(
        class = "main-header-navbar",
        tags$div(
          class = "header-left-group",
          tags$button(
            id    = "btn_sidebar_toggle",
            type  = "button",
            class = "btn-hamburger",
            title = "Toggle sidebar",
            bs_icon("list")
          ),
          tags$div(
            class = "brand-container",
            tags$div(class = "brand-title", "OncoStratify"),
            tags$div(class = "brand-subtitle", "Bioinformatics Survival Workspace")
          )
        ),
        
        tags$div(
          class = "header-right-group",
          # Active biomarker indicator pill
          uiOutput("active_biomarker_pill"),
          # Theme toggle button
          actionButton("toggle_theme", "🌙 Toggle Theme", class = "btn-outline-custom")
        )
      ),
      
      # == MAIN BODY (sidebar + workspace) ==
      tags$div(
        class = "app-body-layout",
        
        # Sidebar overlay for mobile collapse
        tags$div(id = "sidebar_overlay", class = "sidebar-overlay"),
        
        # Collapsible Left Sidebar Panel
        tags$div(
          class = "sidebar-left",
          id = "main_sidebar",
          
          # Target Cohort Selector
          tags$div(
            class = "form-group mb-3",
            tags$label("Target Cohort:", class = "form-label-sm", `for` = "cohort_select"),
            selectInput(
              "cohort_select",
              label = NULL,
              choices = c(
                "Global Cohort (TCGA-HNSC Simulated)" = "global",
                "Indian OSCC-GB Cohort (GSE213862)" = "indian",
                "Uploaded Cohort (Custom CSV)" = "uploaded"
              ),
              selected = "global"
            )
          ),
          
          # Custom Cohort Upload (Conditional on Uploaded Cohort selected)
          conditionalPanel(
            condition = "input.cohort_select == 'uploaded'",
            tags$div(
              class = "form-group mb-3 p-3 rounded",
              style = "background: rgba(124, 58, 237, 0.05); border: 1px dashed var(--accent-light);",
              fileInput(
                "uploaded_data_file",
                "Upload Custom CSV:",
                accept = c(".csv", "text/csv"),
                buttonLabel = "Browse...",
                placeholder = "No file selected"
              ),
              tags$div(
                style = "font-size: 0.75rem; color: var(--text-muted); line-height: 1.4;",
                tags$p("Required columns: patient_id, time, status (0/1), and all 8 biomarkers (TP53, EGFR, etc.).", style = "margin-bottom: 6px;"),
                downloadLink("download_template", "📥 Download CSV Template", style = "font-weight: 600; color: var(--accent);")
              )
            )
          ),
          
          # Analysis Mode
          tags$div(
            class = "form-group mb-3",
            tags$label("Analysis Mode:", class = "form-label-sm"),
            radioButtons(
              "analysis_mode",
              label = NULL,
              choices = c(
                "Single Biomarker" = "single",
                "Ensemble Stacking (MoE)" = "ensemble"
              ),
              selected = "single",
              inline = FALSE
            )
          ),
          
          # Feature selector (Conditional on Single Biomarker Mode)
          conditionalPanel(
            condition = "input.analysis_mode == 'single'",
            tags$div(
              class = "form-group mb-3",
              tags$label("Select Biomarker:", class = "form-label-sm", `for` = "feature"),
              selectInput(
                "feature",
                label = NULL,
                choices = structure(names(FEATURE_META), 
                                    names = sapply(FEATURE_META, function(x) x$label)),
                selected = "TP53"
              )
            )
          ),
          
          # Split method
          tags$div(
            class = "form-group mb-3",
            tags$label("Split Method:", class = "form-label-sm"),
            radioButtons(
              "split_method",
              label = NULL,
              choices = structure(names(SPLIT_METHODS), names = sapply(SPLIT_METHODS, identity)),
              selected = "median",
              inline = FALSE
            )
          ),
          
          # Plot options
          tags$div(
            class = "form-group mb-3",
            tags$label("Plot Options:", class = "form-label-sm"),
            checkboxInput("show_ci", "Show Confidence Interval", value = TRUE),
            checkboxInput("show_risktab", "Show Risk Table", value = TRUE),
            checkboxInput("show_pval", "Show P-value", value = TRUE)
          ),
          
          tags$hr(class = "sidebar-divider"),
          
          # Cohort info
          tags$div(
            class = "form-group mb-3",
            tags$label("Cohort Info:", class = "form-label-sm"),
            uiOutput("cohort_info")
          ),
          
          tags$hr(class = "sidebar-divider"),
          
          # Downloads
          tags$div(
            class = "form-group mb-3",
            tags$label("Downloads:", class = "form-label-sm"),
            downloadButton("download_pdf", "📊 Export Plot (PDF)", class = "btn-outline-custom w-100 mb-2"),
            downloadButton("download_csv", "📋 Export Data (CSV)", class = "btn-outline-custom w-100")
          ),
          
          
          tags$small("OncoStratify v2.0 | Kaplan-Meier Survival Analysis", class = "text-muted d-block mt-2")
        ),
        
        # Center Workspace (Main analysis dashboard)
        tags$div(
          class = "center-workspace",
          
          tags$div(
            class = "workspace-pad",
            
            # KM PLOT CARD
            tags$div(
              class = "custom-card",
              tags$div(
                class = "custom-card-header d-flex justify-content-between align-items-center",
                tags$span("Kaplan-Meier Plot"),
                actionButton(
                  "btn_expand_plot",
                  label = NULL,
                  icon = bs_icon("fullscreen"),
                  class = "btn-icon-only",
                  title = "Expand Plot"
                )
              ),
              tags$div(
                class = "p-3 position-relative",
                km_skeleton(),
                plotOutput("km_plot", height = "520px")
              ),
              # Interpretation Panel
              tags$div(
                class = "card-footer-interpretation border-top p-3",
                style = "background: rgba(255, 255, 255, 0.015); border-color: var(--border) !important;",
                uiOutput("survival_interpretation")
              )
            ),
            
            # STATISTICS CARDS GRID ROW
            tags$div(
              class = "stat-cards-row",
              
              # HIGH group
              tags$div(
                class = "stat-card",
                tags$div(class = "stat-card-icon-wrapper danger-icon", bs_icon("chevron-double-up")),
                tags$div(
                  class = "stat-card-content",
                  tags$div(class = "stat-card-label", htmlOutput("header_high", inline = TRUE)),
                  stat_skeleton("stats_high"),
                  uiOutput("stats_high")
                )
              ),
              
              # LOW group
              tags$div(
                class = "stat-card",
                tags$div(class = "stat-card-icon-wrapper green-icon", bs_icon("chevron-double-down")),
                tags$div(
                  class = "stat-card-content",
                  tags$div(class = "stat-card-label", htmlOutput("header_low", inline = TRUE)),
                  stat_skeleton("stats_low"),
                  uiOutput("stats_low")
                )
              ),
              
              # COX model
              tags$div(
                class = "stat-card",
                tags$div(class = "stat-card-icon-wrapper purple-icon", bs_icon("activity")),
                tags$div(
                  class = "stat-card-content",
                  tags$div(class = "stat-card-label", uiOutput("header_cox_label", inline = TRUE)),
                  stat_skeleton("stats_cox"),
                  uiOutput("stats_cox")
                )
              )
            ),
            
            # AI SURVIVAL PREDICTOR PANEL
            tags$div(
              class = "custom-card",
              tags$div(
                class = "custom-card-header d-flex align-items-center gap-2",
                bs_icon("robot"),
                "AI Survival Predictor"
              ),
              tags$div(
                class = "p-3",
                
                # Model intro badge
                tags$div(
                  class = "d-flex align-items-center gap-2 mb-3",
                  tags$span(
                    class = "badge",
                    style = "background: linear-gradient(135deg, var(--accent), var(--accent-light)); color: #fff; padding: 5px 10px; border-radius: 20px; font-size: 0.75rem;",
                    "Cox PH + Random Forest Ensemble"
                  ),
                  tags$span(
                    style = "font-size: 0.78rem; color: var(--text-muted);",
                    "Enter patient biomarkers below to get a personalized survival risk prediction"
                  )
                ),
                
                # Input sliders for the 8 biomarkers/etiologies
                tags$div(
                  class = "row g-2 mb-3",
                  tags$div(class = "col-6 col-md-3", sliderInput("pred_HPV_E6E7", "HPV E6/E7 (log₂ TPM)", min=0, max=15, value=2, step=0.1)),
                  tags$div(class = "col-6 col-md-3", sliderInput("pred_Tobacco_Exposure", "Tobacco Exposure (Pack-Yrs)", min=0, max=120, value=15, step=1)),
                  tags$div(class = "col-6 col-md-3", sliderInput("pred_TP53", "TP53 Expr (log₂ TPM)", min=0, max=16, value=6, step=0.1)),
                  tags$div(class = "col-6 col-md-3", sliderInput("pred_CD8A", "CD8A Expr (log₂ TPM)", min=0, max=16, value=7, step=0.1)),
                  tags$div(class = "col-6 col-md-3", sliderInput("pred_EGFR", "EGFR Expr (log₂ TPM)", min=0, max=16, value=9, step=0.1)),
                  tags$div(class = "col-6 col-md-3", sliderInput("pred_MKI67", "MKI67 Proliferation", min=0, max=16, value=8, step=0.1)),
                  tags$div(class = "col-6 col-md-3", sliderInput("pred_TumorPurity", "Tumor Purity (0-1)", min=0.2, max=0.98, value=0.7, step=0.01)),
                  tags$div(class = "col-6 col-md-3", sliderInput("pred_MutBurden", "Mutation Burden (TMB)", min=1, max=50, value=8, step=1))
                ),
                
                actionButton(
                  "btn_predict",
                  tagList(bs_icon("play-circle-fill"), " Run Prediction"),
                  class = "btn btn-primary w-100",
                  style = "font-weight: 600; border-radius: 8px; padding: 10px;"
                ),
                
                tags$hr(style = "border-color: var(--border); margin: 16px 0;"),
                
                uiOutput("prediction_result")
              )
            ),
            
            # PATIENT DATA TABLE CARD
            tags$div(
              class = "custom-card",
              tags$div(
                class = "custom-card-header d-flex justify-content-between align-items-center",
                tags$span(
                  tags$span(class = "me-2", bs_icon("table")),
                  "Patient Cohort Data"
                ),
                tags$div(
                  class = "d-flex align-items-center gap-2",
                  tags$span(
                    class = "badge",
                    style = "background: rgba(var(--accent-rgb, 99, 102, 241), 0.15); color: var(--accent); border: 1px solid var(--accent); border-radius: 12px; padding: 3px 10px; font-size: 0.72rem;",
                    "n = 200 patients"
                  ),
                  tags$a(
                    href = "https://www.cancer.gov/tcga",
                    target = "_blank",
                    class = "badge text-decoration-none",
                    style = "background: rgba(34,197,94,0.15); color: #22c55e; border: 1px solid #22c55e; border-radius: 12px; padding: 3px 10px; font-size: 0.72rem;",
                    "Source: TCGA-simulated"
                  )
                )
              ),
              tags$div(
                class = "px-3 pb-2",
                style = "font-size: 0.78rem; color: var(--text-muted); line-height: 1.5; border-left: 3px solid var(--accent); padding-left: 10px; margin-bottom: 8px;",
                tags$strong("Data provenance: "),
                "This cohort (200 patients, seed 42) is a synthetic dataset modelled on ",
                tags$a("TCGA (The Cancer Genome Atlas)", href = "https://www.cancer.gov/tcga", target = "_blank", style = "color: var(--accent);"),
                " expression distributions. Gene expression values are log₂ TPM. Survival outcomes are probabilistically generated to reflect known TCGA biomarker-survival relationships. For real TCGA integration see ",
                tags$code("DATA_SOURCES.md"), "."
              ),
              tags$div(
                DTOutput("data_table")
              )
            )
          )
        )
      )
    )
  )
}

if (interactive()) cat("✓ UI loaded\n")
