# =============================================================================
# R/config.R — Centralized configuration
# NO HARDCODED VALUES — Everything here
# =============================================================================

# App metadata
APP_NAME    <- "OncoStratify — Oral Cancer Workspace"
APP_VERSION <- "2.0"
APP_ROOT    <- getwd()

# Colours - LIGHT MODE
LIGHT <- list(
  bg           = "#f4f6f9",
  bg_card      = "#ffffff",
  bg_card2     = "#f0f2f5",
  fg           = "#1a1f2e",
  fg_muted     = "#5a6474",
  border       = "#d1d9e0",
  accent       = "#7c3aed", # Purple accent
  grid         = "#e2e8f0"
)

# Colours - DARK MODE
DARK <- list(
  bg           = "#0d1117",
  bg_card      = "#161b22",
  bg_card2     = "#1c2128",
  fg           = "#e6edf3",
  fg_muted     = "#7d8590",
  border       = "#30363d",
  accent       = "#a78bfa", # Light purple accent
  grid         = "#21262d"
)

# Group colours (same in both themes)
COLOUR_HIGH  <- "#f85149"  # Red
COLOUR_LOW   <- "#3fb950"  # Green
COLOUR_TEAL  <- "#00d4aa"
COLOUR_BLUE  <- "#58a6ff"

# Plot settings
KM_PLOT_HEIGHT      <- 520
CI_ALPHA            <- 0.15
RISK_TABLE_FONTSIZE <- 3.8
DT_PAGE_LENGTH      <- 10

# Performance
CACHE_HIT_THRESHOLD_MS <- 80
SKELETON_ANIMATION_MS  <- 1600

# Feature flags
USE_REAL_DATA <- FALSE  # Set to TRUE after TCGA-HNSC integration

# Simulated data config
N_PATIENTS   <- 200
N_GENES      <- 10
SEED         <- 42

# Feature metadata (biomarkers tailored for OSCC/HNSCC)
FEATURE_META <- list(
  HPV_E6E7 = list(
    label = "HPV16/18 E6/E7 Expression",
    unit  = "log₂ TPM",
    type  = "Viral Load",
    description = "Oncogenic viral transcripts; high = HPV-driven OSCC (better prognosis)"
  ),
  Tobacco_Exposure = list(
    label = "Tobacco Exposure",
    unit  = "Pack-Years",
    type  = "Etiology",
    description = "Lifetime tobacco exposure; high = tobacco-driven OSCC (poorer prognosis)"
  ),
  TP53 = list(
    label = "TP53 Expression",
    unit  = "log₂ TPM",
    type  = "Gene Expression",
    description = "Tumor suppressor; inactivated/altered in tobacco-induced oral cancers"
  ),
  CD8A = list(
    label = "CD8A Expression",
    unit  = "log₂ TPM",
    type  = "Immune Infiltration",
    description = "Cytotoxic T-cell marker; denotes immune-active tumor microenvironment"
  ),
  EGFR = list(
    label = "EGFR Expression",
    unit  = "log₂ TPM",
    type  = "Targeted Therapy",
    description = "Growth factor receptor overexpressed in 90% of oral squamous cell carcinomas"
  ),
  MKI67 = list(
    label = "MKI67 (Ki-67) Expression",
    unit  = "log₂ TPM",
    type  = "Proliferation",
    description = "Proliferation index; high Ki-67 indicates rapid cell division and aggressive tumor"
  ),
  TumorPurity = list(
    label = "Tumor Purity Score",
    unit  = "Fraction (0-1)",
    type  = "Clinical Score",
    description = "Percentage of cancer cells vs. stroma and immune cells in tumor sample"
  ),
  MutationBurden = list(
    label = "Tumor Mutation Burden",
    unit  = "Mutations per Mb",
    type  = "Clinical Score",
    description = "Somatic mutation density; highly correlated with tobacco carcinogen exposure"
  )
)

# Split methods
SPLIT_METHODS <- list(
  median = "Median Split (all patients)",
  tertile = "Tertile Split (drop middle third)",
  quartile = "Quartile Split (drop middle half)"
)

# Message
if (interactive()) {
  cat("\n✓ Configuration loaded\n")
}
