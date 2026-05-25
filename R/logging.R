# =============================================================================
# R/logging.R — Logging and error handling
# =============================================================================

#' Log a message with timestamp
log_message <- function(level, msg, details = "") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  if (is.null(details) || length(details) == 0 || details[1] == "") {
    full_msg <- sprintf("[%s] [%s] %s", timestamp, level, msg)
  } else {
    full_msg <- sprintf("[%s] [%s] %s — %s", timestamp, level, msg, details[1])
  }
  message(full_msg)
  invisible(full_msg)
}

#' Log info message
log_info <- function(msg, details = "") {
  log_message("INFO", msg, details)
}

#' Log warning message
log_warn <- function(msg, details = "") {
  log_message("WARN", msg, details)
}

#' Log error message
log_error <- function(msg, details = "") {
  log_message("ERROR", msg, details)
}

#' Safe calculation wrapper with error handling
safe_calculate <- function(expr, fallback = NULL, error_msg = "Calculation failed") {
  tryCatch({
    expr
  }, error = function(e) {
    log_error(error_msg, e$message)
    fallback
  })
}

# Export
if (interactive()) cat("✓ Logging loaded\n")
