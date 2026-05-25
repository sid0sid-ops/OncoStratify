# =============================================================================
# R/themes.R — Bootstrap themes with VERSION COMPATIBILITY
# 
# FIXES THE bs_theme ERROR by supporting multiple bslib versions
# =============================================================================

# Check bslib version and log it
bslib_version <- tryCatch(
  {
    pkg_version <- packageVersion("bslib")
    sprintf("%s.%s.%s", pkg_version$major, pkg_version$minor, pkg_version$patch)
  },
  error = function(e) "unknown"
)

log_info("bslib version", bslib_version)

# =============================================================================
# FUNCTION: Create bs_theme safely across versions
# =============================================================================

create_bs_theme_safe <- function(mode = "dark") {
  
  palette <- if (mode == "dark") DARK else LIGHT
  
  # bslib 0.5+ uses font_google / font_face helpers — list() with 'family' is removed
  # Use only supported scalar arguments that work across bslib 0.4–0.11
  tryCatch({
    bslib::bs_theme(
      bg        = palette$bg,
      fg        = palette$fg,
      primary   = palette$accent,
      secondary = palette$accent,
      success   = COLOUR_LOW,
      danger    = COLOUR_HIGH,
      warning   = "#d29922",
      info      = COLOUR_TEAL
    )
  }, error = function(e) {
    
    log_warn("Modern bs_theme failed", e$message)
    
    # Minimal fallback
    tryCatch({
      bslib::bs_theme(
        version = 5,
        bg      = palette$bg,
        fg      = palette$fg,
        primary = palette$accent
      )
    }, error = function(e2) {
      log_warn("Using Bootstrap defaults", "themes may not be fully styled")
      NULL
    })
  })
}

# =============================================================================
# CREATE THEMES
# =============================================================================

# Dark theme
DARK_BSLIB_THEME <- create_bs_theme_safe("dark")

# Light theme
LIGHT_BSLIB_THEME <- create_bs_theme_safe("light")

# =============================================================================
# ggplot2 THEMES (for KM plots)
# =============================================================================

#' Build ggplot2 theme for given palette
build_plot_theme <- function(palette) {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      # Background
      plot.background    = ggplot2::element_rect(fill = palette$bg, colour = NA),
      panel.background   = ggplot2::element_rect(fill = palette$bg, colour = NA),
      panel.grid.major   = ggplot2::element_line(colour = palette$grid, linewidth = 0.3),
      panel.grid.minor   = ggplot2::element_blank(),
      
      # Text
      text               = ggplot2::element_text(colour = palette$fg, family = "sans"),
      plot.title         = ggplot2::element_text(colour = palette$fg, size = 14, face = "bold"),
      axis.title         = ggplot2::element_text(colour = palette$fg, size = 12),
      axis.text          = ggplot2::element_text(colour = palette$fg, size = 11),
      legend.text        = ggplot2::element_text(colour = palette$fg, size = 11),
      legend.title       = ggplot2::element_text(colour = palette$fg, size = 12, face = "bold"),
      
      # Legend
      legend.background  = ggplot2::element_rect(fill = palette$bg_card, colour = palette$border),
      legend.position    = "right",
      
      # Strips
      strip.text         = ggplot2::element_text(colour = palette$fg, size = 11, face = "bold"),
      strip.background   = ggplot2::element_rect(fill = palette$bg_card2, colour = NA),
      
      # Axis
      axis.line          = ggplot2::element_line(colour = palette$border, linewidth = 0.5),
      axis.ticks         = ggplot2::element_line(colour = palette$border, linewidth = 0.5)
    )
}

# Dark and light plot themes
DARK_PLOT_THEME  <- build_plot_theme(DARK)
LIGHT_PLOT_THEME <- build_plot_theme(LIGHT)

# =============================================================================
# HELPER: Get DT table styling for theme
# =============================================================================

get_dt_styles <- function(mode = "dark") {
  palette <- if (mode == "dark") DARK else LIGHT
  
  sprintf("
    .dataTables_wrapper {
      color: %s;
    }
    .dataTables_wrapper th {
      background-color: %s;
      color: %s;
      border-color: %s;
    }
    .dataTables_wrapper td {
      border-color: %s;
    }
    .dataTables_wrapper tr:hover {
      background-color: %s !important;
    }
    .dataTables_paginate .paginate_button.current {
      background-color: %s !important;
    }
  ", palette$fg, palette$bg_card2, palette$fg, palette$border,
      palette$border, palette$bg_card2, palette$accent)
}

log_info("Themes created", sprintf("Dark=%s, Light=%s", 
                                   ifelse(is.null(DARK_BSLIB_THEME), "NULL", "OK"),
                                   ifelse(is.null(LIGHT_BSLIB_THEME), "NULL", "OK")))

if (interactive()) cat("✓ Themes loaded\n")
