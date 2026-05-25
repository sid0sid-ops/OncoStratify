# =============================================================================
# R/ux.R — UX Patterns
# =============================================================================

# Skeleton CSS and JS
ux_css <- function() {
  tags$style(HTML("
    @keyframes os-shimmer-move {
      0%   { background-position: 100% 50%; }
      100% { background-position: 0% 50%; }
    }
    
    .skel-shimmer {
      background: linear-gradient(90deg,
        var(--os-bg-card2) 0%,
        var(--os-border)   30%,
        var(--os-bg-card2) 60%);
      background-size: 300% 100%;
      animation: os-shimmer-move 1.6s ease infinite;
      border-radius: 3px;
      display: inline-block;
    }
    
    .stat-skeleton {
      display: none;
      padding: 4px 0;
    }
    
    .skel-stat-row {
      display: flex;
      justify-content: space-between;
      align-items: center;
      padding: 6px 0;
      border-bottom: 1px solid var(--os-border);
    }
    
    .skel-label-bar {
      width: 45%;
      height: 12px;
    }
    
    .skel-value-bar {
      width: 28%;
      height: 12px;
    }
    
    .km-skeleton {
      display: none;
      position: absolute;
      top: 0;
      left: 0;
      width: 100%;
      height: 100%;
      z-index: 10;
      overflow: hidden;
      background: var(--panel-bg);
    }
    
    .os-computing {
      opacity: 0.35;
      pointer-events: none;
      transition: opacity 0.15s ease;
    }
  "))
}

# Skeleton JS
ux_js <- function(feature_meta) {
  
  # Build feature labels JS object
  labels <- paste(
    sprintf('"%s": "%s"', names(feature_meta), 
            sapply(feature_meta, function(x) x$label)),
    collapse = ", "
  )
  
  tags$script(HTML(sprintf("
    var ONCOSTRAT_LABELS = {%s};
    var STAT_OUTPUT_IDS = ['stats_high', 'stats_low', 'stats_cox'];
    
    $(document).on('shiny:outputinvalidated', function(event) {
      var id = event.target.id;
      if (STAT_OUTPUT_IDS.indexOf(id) !== -1) {
        $('#skel-' + id).show();
      }
      if (id === 'km_plot') {
        $('#skel-km_plot').show();
      }
    });
    
    $(document).on('shiny:value', function(event) {
      var id = event.name;
      if (STAT_OUTPUT_IDS.indexOf(id) !== -1) {
        $('#skel-' + id).hide();
        $('#' + id).removeClass('os-computing');
      }
      if (id === 'km_plot') {
        $('#skel-km_plot').hide();
      }
    });
  ", labels)))
}

# Skeleton HTML elements
km_skeleton <- function() {
  tags$div(
    id = "skel-km_plot",
    class = "km-skeleton",
    tags$div(class = "skel-shimmer", style = "width: 100%; height: 100%;")
  )
}

stat_skeleton <- function(output_id, n_rows = 5) {
  rows <- lapply(1:n_rows, function(i) {
    tags$div(
      class = "skel-stat-row",
      tags$div(class = "skel-shimmer skel-label-bar"),
      tags$div(class = "skel-shimmer skel-value-bar")
    )
  })
  
  tags$div(
    id = paste0("skel-", output_id),
    class = "stat-skeleton",
    do.call(tags$div, rows)
  )
}

if (interactive()) cat("✓ UX module loaded\n")
