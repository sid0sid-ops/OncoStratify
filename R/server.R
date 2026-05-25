# =============================================================================
# R/server.R — Server-side reactive logic
# =============================================================================

app_server <- function(input, output, session) {
  
  log_info("Server started")
  
  # ==========================================================================
  # ==========================================================================
  # DYNAMIC REACTIVE ENSEMBLE SURVIVAL MODEL
  # Trained reactively on selected target cohort using Stacking (Cox + RSF)
  # ==========================================================================
  
  # Download Template CSV
  output$download_template <- downloadHandler(
    filename = function() {
      "OncoStratify_Cohort_Template.csv"
    },
    content = function(file) {
      write.csv(GLOBAL_COHORT[1:5, ], file, row.names = FALSE)
    }
  )

  # Reactive custom uploaded data parsing and validation
  uploaded_data <- reactive({
    req(input$uploaded_data_file)
    
    df <- tryCatch({
      read.csv(input$uploaded_data_file$datapath, stringsAsFactors = FALSE)
    }, error = function(e) {
      validate(need(FALSE, paste("Failed to read CSV file:", e$message)))
      NULL
    })
    
    req_cols <- c("patient_id", "time", "status", "HPV_E6E7", "Tobacco_Exposure", 
                  "TP53", "CD8A", "EGFR", "MKI67", "TumorPurity", "MutationBurden")
    
    missing_cols <- setdiff(req_cols, colnames(df))
    validate(
      need(length(missing_cols) == 0, 
           paste("Uploaded CSV is missing required columns:", paste(missing_cols, collapse = ", ")))
    )
    
    validate(
      need(is.numeric(df$time), "Survival 'time' column must be numeric."),
      need(is.numeric(df$status), "Event 'status' column must be numeric (0 = Censored, 1 = Death)."),
      need(all(df$status %in% c(0, 1)), "Event 'status' must contain only values of 0 or 1.")
    )
    
    # Standardize types
    df$time <- as.numeric(df$time)
    df$status <- as.numeric(df$status)
    df$HPV_E6E7 <- as.numeric(df$HPV_E6E7)
    df$Tobacco_Exposure <- as.numeric(df$Tobacco_Exposure)
    df$TP53 <- as.numeric(df$TP53)
    df$CD8A <- as.numeric(df$CD8A)
    df$EGFR <- as.numeric(df$EGFR)
    df$MKI67 <- as.numeric(df$MKI67)
    df$TumorPurity <- as.numeric(df$TumorPurity)
    df$MutationBurden <- as.numeric(df$MutationBurden)
    
    df <- df[!is.na(df$time) & !is.na(df$status), ]
    
    validate(
      need(nrow(df) >= 10, "Custom cohort must contain at least 10 valid patient records to run survival analysis.")
    )
    
    log_info("Custom cohort uploaded", sprintf("patients=%d", nrow(df)))
    df
  })

  ensemble_results <- reactive({
    req(input$cohort_select)
    df <- if (input$cohort_select == "indian") {
      INDIAN_COHORT
    } else if (input$cohort_select == "uploaded") {
      req(uploaded_data())
    } else {
      GLOBAL_COHORT
    }
    log_info("Training Stacking Ensemble reactively", sprintf("cohort=%s", input$cohort_select))
    train_stacked_ensemble(df, features = names(FEATURE_META))
  }) |> bindCache(input$cohort_select, input$uploaded_data_file$datapath)
  
  # Theme mode reactive
  current_mode <- reactiveVal("dark")
  
  observe({
    is_dark <- if (is.null(input$browser_is_dark)) TRUE else input$browser_is_dark
    if (is_dark) {
      current_mode("dark")
      session$setCurrentTheme(DARK_BSLIB_THEME)
      shinyjs::removeClass(selector = "body", class = "light-theme")
      shinyjs::addClass(selector = "body", class = "dark-theme")
    } else {
      current_mode("light")
      session$setCurrentTheme(LIGHT_BSLIB_THEME)
      shinyjs::removeClass(selector = "body", class = "dark-theme")
      shinyjs::addClass(selector = "body", class = "light-theme")
    }
  })
  
  # Theme toggle button
  observeEvent(input$toggle_theme, {
    new_mode <- if (current_mode() == "dark") "light" else "dark"
    current_mode(new_mode)
    session$setCurrentTheme(if (new_mode == "dark") DARK_BSLIB_THEME else LIGHT_BSLIB_THEME)
    if (new_mode == "dark") {
      shinyjs::removeClass(selector = "body", class = "light-theme")
      shinyjs::addClass(selector = "body", class = "dark-theme")
    } else {
      shinyjs::removeClass(selector = "body", class = "dark-theme")
      shinyjs::addClass(selector = "body", class = "light-theme")
    }
  })
  
  # Active biomarker indicator pill
  output$active_biomarker_pill <- renderUI({
    if (req(input$analysis_mode) == "ensemble") {
      tags$div(
        class = "active-biomarker-pill",
        style = "background: rgba(124, 58, 237, 0.15); border-color: rgba(124, 58, 237, 0.4); color: var(--accent);",
        bs_icon("robot"),
        tags$span("Ensemble Stacking (MoE): Active")
      )
    } else {
      req(input$feature)
      meta <- FEATURE_META[[input$feature]]
      tags$div(
        class = "active-biomarker-pill",
        bs_icon("activity"),
        tags$span(sprintf("%s: %s (%s)", meta$type, input$feature, meta$unit))
      )
    }
  })
  
  # ==========================================================================
  # DATA STRATIFICATION
  # ==========================================================================
  
  stratified_data <- reactive({
    req(input$cohort_select, input$analysis_mode, input$split_method)
    
    # Choose cohort data
    df <- if (input$cohort_select == "indian") {
      INDIAN_COHORT
    } else if (input$cohort_select == "uploaded") {
      req(uploaded_data())
    } else {
      GLOBAL_COHORT
    }
    
    if (input$analysis_mode == "single") {
      req(input$feature)
      log_info("Stratifying Single Biomarker", sprintf("feature=%s, cohort=%s, method=%s", input$feature, input$cohort_select, input$split_method))
      vals <- df[[input$feature]]
    } else {
      log_info("Stratifying Ensemble Stacking", sprintf("cohort=%s, method=%s", input$cohort_select, input$split_method))
      ens <- ensemble_results()
      vals <- ens$unified_risk
      df$unified_risk <- vals
    }
    
    # Compute split points
    if (input$split_method == "median") {
      cut_val <- median(vals, na.rm = TRUE)
      df$group <- ifelse(vals >= cut_val, "High", "Low")
    } else if (input$split_method == "tertile") {
      cuts <- quantile(vals, probs = c(1/3, 2/3), na.rm = TRUE)
      keep <- vals <= cuts[1] | vals >= cuts[2]
      df <- df[keep, ]
      if (input$analysis_mode == "single") {
        vals_f <- df[[input$feature]]
      } else {
        vals_f <- df$unified_risk
      }
      df$group <- ifelse(vals_f >= median(vals_f, na.rm = TRUE), "High", "Low")
    } else if (input$split_method == "quartile") {
      cuts <- quantile(vals, probs = c(0.25, 0.75), na.rm = TRUE)
      keep <- vals <= cuts[1] | vals >= cuts[2]
      df <- df[keep, ]
      if (input$analysis_mode == "single") {
        vals_f <- df[[input$feature]]
      } else {
        vals_f <- df$unified_risk
      }
      df$group <- ifelse(vals_f >= median(vals_f, na.rm = TRUE), "High", "Low")
    }
    
    df$group <- factor(df$group, levels = c("High", "Low"))
    df
  }) |> bindCache(input$cohort_select, input$uploaded_data_file$datapath, input$analysis_mode, input$feature, input$split_method)
  
  # ==========================================================================
  # SURVIVAL ANALYSIS
  # ==========================================================================
  
  km_fit <- reactive({
    req(stratified_data())
    survival::survfit(Surv(time, status) ~ group, data = stratified_data())
  }) |> bindCache(input$cohort_select, input$uploaded_data_file$datapath, input$analysis_mode, input$feature, input$split_method)
  
  logrank_p <- reactive({
    req(stratified_data())
    test <- survival::survdiff(Surv(time, status) ~ group, data = stratified_data())
    1 - pchisq(test$chisq, df = length(test$n) - 1)
  }) |> bindCache(input$cohort_select, input$uploaded_data_file$datapath, input$analysis_mode, input$feature, input$split_method)
  
  cox_model <- reactive({
    req(stratified_data())
    survival::coxph(Surv(time, status) ~ group, data = stratified_data())
  }) |> bindCache(input$cohort_select, input$uploaded_data_file$datapath, input$analysis_mode, input$feature, input$split_method)
  
  # ==========================================================================
  # OUTPUT: KM PLOT
  # ==========================================================================
  
  output$km_plot <- renderPlot({
    req(km_fit(), stratified_data(), cox_model())
    
    theme <- if (current_mode() == "dark") DARK_PLOT_THEME else LIGHT_PLOT_THEME
    palette <- if (current_mode() == "dark") DARK else LIGHT
    
    model <- cox_model()
    coef <- model$coefficients[1]
    hr <- exp(coef)
    ci <- exp(confint(model))
    pval <- logrank_p() # Use the robust log-rank p-value
    
    # Choose title, legend, and stats annotation based on mode
    if (input$analysis_mode == "ensemble") {
      ens <- ensemble_results()
      title_str <- sprintf("KM Survival Curve — Stacking Ensemble (%s split)", input$split_method)
      legend_labs <- c("High Risk Group", "Low Risk Group")
      stats_label <- sprintf(
        "Stacking Ensemble (MoE) Stats:\nCox Expert C-index: %.3f\nRSF Expert C-index: %.3f\nMeta-Learner C-index: %.3f\nArbitrator Weight (Cox): %.1f%%\nArbitrator Weight (RSF): %.1f%%\nLog-rank p-value: %.4f",
        ens$c_index_cox, ens$c_index_rsf, ens$c_index_meta,
        ens$trust_cox, ens$trust_rsf, pval
      )
    } else {
      title_str <- sprintf("KM Survival Curve — %s (%s split)", 
                           FEATURE_META[[input$feature]]$label,
                           input$split_method)
      legend_labs <- c("High Expression", "Low Expression")
      stats_label <- sprintf(
        "Cox Hazard Ratio (HR): %.2f\n95%% CI: %.2f – %.2f\nLog-rank p-value: %.4f\nVerdict: %s",
        hr, ci[1], ci[2], pval,
        if (pval < 0.05) "SIGNIFICANT BIOMARKER" else "NON-SIGNIFICANT"
      )
    }
    
    p <- suppressWarnings(suppressMessages(survminer::ggsurvplot(
      km_fit(),
      data = stratified_data(),
      risk.table = input$show_risktab,
      conf.int = input$show_ci,
      pval = FALSE,
      palette = c(COLOUR_HIGH, COLOUR_LOW),
      title = title_str,
      xlab = "Survival Follow-up Time (days)",
      ylab = "Probability of Overall Survival",
      legend.labs = legend_labs,
      censor.shape = "|",
      censor.size = 5.5,
      size = 1.2,
      risk.table.col = "strata",
      ggtheme = theme
    )))
    
    # Programmatically remove colour label to prevent ggplot2 "Ignoring unknown labels: colour : Strata" warnings
    p$plot$labels$colour <- NULL
    if (!is.null(p$table)) {
      p$table$labels$colour <- NULL
    }
    
    # Add stats label annotation
    p$plot <- p$plot +
      ggplot2::annotate(
        "text",
        x = 50,
        y = 0.12,
        label = stats_label,
        hjust = 0,
        color = palette$fg,
        size = 4.2,
        fontface = "bold.italic",
        lineheight = 1.2
      ) +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = palette$bg, colour = NA)
      )
    
    # Apply theme customization to risk table if it exists
    if (input$show_risktab && !is.null(p$table)) {
      p$table <- p$table +
        ggplot2::theme(
          plot.background = ggplot2::element_rect(fill = palette$bg, colour = NA),
          panel.background = ggplot2::element_rect(fill = palette$bg, colour = NA),
          text = ggplot2::element_text(colour = palette$fg),
          axis.text.x = ggplot2::element_text(colour = palette$fg),
          axis.title.x = ggplot2::element_text(colour = palette$fg)
        )
    }
    
    suppressWarnings(print(p))
  })
  
  # ==========================================================================
  # OUTPUT: STATISTICS
  # ==========================================================================
  
  output$header_high <- renderUI({
    label <- if (input$analysis_mode == "ensemble") "HIGH Risk" else "HIGH Group"
    tags$span(sprintf("● %s", label), style = sprintf("color: %s; font-weight: bold;", COLOUR_HIGH))
  })
  
  output$header_low <- renderUI({
    label <- if (input$analysis_mode == "ensemble") "LOW Risk" else "LOW Group"
    tags$span(sprintf("● %s", label), style = sprintf("color: %s; font-weight: bold;", COLOUR_LOW))
  })
  
  output$header_cox_label <- renderUI({
    if (input$analysis_mode == "ensemble") "Meta-Learner" else "Cox Model"
  })
  
  # Helper: compute group stats
  compute_group_stats <- function(data, group_val) {
    df <- data[data$group == group_val, ]
    n_patients <- nrow(df)
    n_events <- sum(df$status, na.rm = TRUE)
    
    fit <- survival::survfit(Surv(time, status) ~ 1, data = df)
    median_os <- fit$time[which.max(fit$surv <= 0.5)]
    if (length(median_os) == 0) median_os <- max(df$time)
    
    ci_lower <- fit$lower[which.max(fit$surv <= 0.5)]
    ci_upper <- fit$upper[which.max(fit$surv <= 0.5)]
    if (is.na(ci_lower)) ci_lower <- NA
    if (is.na(ci_upper)) ci_upper <- NA
    
    list(
      n = n_patients,
      events = n_events,
      median_os = median_os,
      ci_lower = ci_lower,
      ci_upper = ci_upper
    )
  }
  
  output$stats_high <- renderUI({
    req(stratified_data())
    stats <- compute_group_stats(stratified_data(), "High")
    tags$div(
      tags$div(sprintf("N patients: %d", stats$n), class = "small mb-2"),
      tags$div(sprintf("Events: %d", stats$events), class = "small mb-2"),
      tags$div(sprintf("Median OS: %d days", round(stats$median_os)), class = "small mb-2"),
      tags$div(sprintf("95%% CI: %d–%d", round(stats$ci_lower), round(stats$ci_upper)), class = "small")
    )
  })
  
  output$stats_low <- renderUI({
    req(stratified_data())
    stats <- compute_group_stats(stratified_data(), "Low")
    tags$div(
      tags$div(sprintf("N patients: %d", stats$n), class = "small mb-2"),
      tags$div(sprintf("Events: %d", stats$events), class = "small mb-2"),
      tags$div(sprintf("Median OS: %d days", round(stats$median_os)), class = "small mb-2"),
      tags$div(sprintf("95%% CI: %d–%d", round(stats$ci_lower), round(stats$ci_upper)), class = "small")
    )
  })
  
  output$stats_cox <- renderUI({
    if (input$analysis_mode == "ensemble") {
      ens <- ensemble_results()
      tags$div(
        tags$div(sprintf("Meta C-index: %.3f", ens$c_index_meta), class = "small mb-2"),
        tags$div(sprintf("Cox weight: %.1f%%", ens$trust_cox), class = "small mb-2"),
        tags$div(sprintf("RSF weight: %.1f%%", ens$trust_rsf), class = "small")
      )
    } else {
      req(cox_model())
      model <- cox_model()
      coef <- model$coefficients[1]
      hr <- exp(coef)
      ci <- exp(confint(model))
      pval <- logrank_p()
      
      tags$div(
        tags$div(sprintf("HR: %.2f", hr), class = "small mb-2"),
        tags$div(sprintf("95%% CI: %.2f–%.2f", ci[1], ci[2]), class = "small mb-2"),
        tags$div(sprintf("p-value: %.4f", pval), class = "small"),
        if (pval < 0.05) tags$div("✓ Significant (p < 0.05)", class = "small text-success mt-2")
      )
    }
  })
  
  # ==========================================================================
  # OUTPUT: COHORT INFO
  # ==========================================================================
  
  output$cohort_info <- renderUI({
    active_cohort <- if (input$cohort_select == "indian") {
      INDIAN_COHORT
    } else if (input$cohort_select == "uploaded") {
      req(uploaded_data())
    } else {
      GLOBAL_COHORT
    }
    data <- stratified_data()
    tags$div(
      tags$small(sprintf("Total: %d", nrow(active_cohort))),
      tags$br(),
      tags$small(sprintf("Current: %d", nrow(data))),
      tags$br(),
      tags$small(sprintf("Events: %d", sum(data$status)))
    )
  })
  
  # ==========================================================================
  # POPUP: EXPANDED KM PLOT MODAL
  # ==========================================================================
  
  observeEvent(input$btn_expand_plot, {
    title_str <- if (input$analysis_mode == "ensemble") {
      sprintf("Expanded Workstation View: Stacking Ensemble (%s split)", input$split_method)
    } else {
      sprintf("Expanded Workstation View: %s (%s split)", 
              FEATURE_META[[input$feature]]$label,
              input$split_method)
    }
    
    showModal(modalDialog(
      title = title_str,
      size = "xl",
      easyClose = TRUE,
      fade = TRUE,
      tags$div(
        class = "position-relative p-2",
        style = "background: var(--os-bg-card); border-radius: 8px;",
        plotOutput("km_plot_modal", height = "600px")
      ),
      footer = modalButton("Close Workspace View")
    ))
  })
  
  output$km_plot_modal <- renderPlot({
    req(km_fit(), stratified_data(), cox_model())
    
    theme <- if (current_mode() == "dark") DARK_PLOT_THEME else LIGHT_PLOT_THEME
    palette <- if (current_mode() == "dark") DARK else LIGHT
    
    model <- cox_model()
    coef <- model$coefficients[1]
    hr <- exp(coef)
    ci <- exp(confint(model))
    pval <- logrank_p()
    
    # Choose title, legend, and stats annotation based on mode
    if (input$analysis_mode == "ensemble") {
      ens <- ensemble_results()
      title_str <- sprintf("KM Survival Curve — Stacking Ensemble (%s split)", input$split_method)
      legend_labs <- c("High Risk Group", "Low Risk Group")
      stats_label <- sprintf(
        "Stacking Ensemble (MoE) Stats:\nCox Expert C-index: %.3f\nRSF Expert C-index: %.3f\nMeta-Learner C-index: %.3f\nArbitrator Weight (Cox): %.1f%%\nArbitrator Weight (RSF): %.1f%%\nLog-rank p-value: %.4f",
        ens$c_index_cox, ens$c_index_rsf, ens$c_index_meta,
        ens$trust_cox, ens$trust_rsf, pval
      )
    } else {
      title_str <- sprintf("KM Survival Curve — %s (%s split)", 
                           FEATURE_META[[input$feature]]$label,
                           input$split_method)
      legend_labs <- c("High Expression", "Low Expression")
      stats_label <- sprintf(
        "Cox Hazard Ratio (HR): %.2f\n95%% CI: %.2f – %.2f\nLog-rank p-value: %.4f\nVerdict: %s",
        hr, ci[1], ci[2], pval,
        if (pval < 0.05) "SIGNIFICANT BIOMARKER" else "NON-SIGNIFICANT"
      )
    }
    
    p <- suppressWarnings(suppressMessages(survminer::ggsurvplot(
      km_fit(),
      data = stratified_data(),
      risk.table = input$show_risktab,
      conf.int = input$show_ci,
      pval = FALSE,
      palette = c(COLOUR_HIGH, COLOUR_LOW),
      title = title_str,
      xlab = "Survival Follow-up Time (days)",
      ylab = "Probability of Overall Survival",
      legend.labs = legend_labs,
      censor.shape = "|",
      censor.size = 5.5,
      size = 1.2,
      risk.table.col = "strata",
      ggtheme = theme
    )))
    
    # Programmatically remove colour label to prevent ggplot2 "Ignoring unknown labels: colour : Strata" warnings
    p$plot$labels$colour <- NULL
    if (!is.null(p$table)) {
      p$table$labels$colour <- NULL
    }
    
    # Add stats label annotation
    p$plot <- p$plot +
      ggplot2::annotate(
        "text",
        x = 50,
        y = 0.12,
        label = stats_label,
        hjust = 0,
        color = palette$fg,
        size = 4.2,
        fontface = "bold.italic",
        lineheight = 1.2
      ) +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = palette$bg, colour = NA)
      )
    
    # Apply theme customization to risk table if it exists
    if (input$show_risktab && !is.null(p$table)) {
      p$table <- p$table +
        ggplot2::theme(
          plot.background = ggplot2::element_rect(fill = palette$bg, colour = NA),
          panel.background = ggplot2::element_rect(fill = palette$bg, colour = NA),
          text = ggplot2::element_text(colour = palette$fg),
          axis.text.x = ggplot2::element_text(colour = palette$fg),
          axis.title.x = ggplot2::element_text(colour = palette$fg)
        )
    }
    
    suppressWarnings(print(p))
  })
  
  # ==========================================================================
  # OUTPUT: CLINICAL INTERPRETATION & AUTOMATED ANNOTATION
  # ==========================================================================
  
  output$survival_interpretation <- renderUI({
    req(km_fit(), stratified_data(), cox_model())
    
    # Cox calculations
    model <- cox_model()
    coef <- model$coefficients[1]
    hr <- exp(coef)
    ci <- exp(confint(model))
    pval <- logrank_p()
    signif <- pval < 0.05
    
    # Medians extraction
    tbl <- km_fit()$table
    high_med <- if("group=High" %in% rownames(tbl)) tbl["group=High", "median"] else NA
    low_med <- if("group=Low" %in% rownames(tbl)) tbl["group=Low", "median"] else NA
    
    # Probability projections
    s_sum <- summary(km_fit(), times = c(365, 1095, 1825))
    get_surv_prob <- function(strata_val, time_val) {
      idx <- which(s_sum$strata == strata_val & s_sum$time == time_val)
      if (length(idx) > 0) {
        sprintf("%.1f%%", s_sum$surv[idx] * 100)
      } else {
        "N/A"
      }
    }
    
    high_1y <- get_surv_prob("group=High", 365)
    high_3y <- get_surv_prob("group=High", 1095)
    high_5y <- get_surv_prob("group=High", 1825)
    
    low_1y <- get_surv_prob("group=Low", 365)
    low_3y <- get_surv_prob("group=Low", 1095)
    low_5y <- get_surv_prob("group=Low", 1825)
    
    verdict_title <- if (signif) "STATISTICALLY SIGNIFICANT RISK SEPARATION" else "NON-SIGNIFICANT DIFFERENCE"
    verdict_color <- if (signif) "var(--success)" else "var(--warning)"
    
    if (input$analysis_mode == "ensemble") {
      ens <- ensemble_results()
      cohort_lbl <- if (input$cohort_select == "indian") {
        "Indian OSCC-GB (GSE213862)"
      } else if (input$cohort_select == "uploaded") {
        "Uploaded Custom Cohort"
      } else {
        "Global HNSC (TCGA)"
      }
      
      direction_text <- sprintf(
        "Using the <strong>Ensemble Stacking (Mixture of Experts)</strong> architecture, patients are stratified into High vs. Low Risk groups using their composite multi-biomarker profiles. The Stacking Meta-Learner integrates predictions from the <strong>Cox Proportional Hazards Expert</strong> (representing linear clinical trends) and the <strong>Random Survival Forest Expert</strong> (representing non-linear genomic-environmental interactions). On the <strong>%s</strong> cohort, the Meta-Learner achieves a consensus C-index of <strong>%.3f</strong>, assigning <strong>%.1f%%</strong> weight to Cox and <strong>%.1f%%</strong> weight to RSF.",
        cohort_lbl, ens$c_index_meta, ens$trust_cox, ens$trust_rsf
      )
      
      biological_link <- sprintf(
        "<strong>Clinical & Etiological Synthesis</strong>: The ensemble incorporates viral status (HPV-E6/E7), carcinogen exposure (Tobacco Pack-Years), somatic alterations (TP53), proliferation (MKI67), immune microenvironment (CD8A), and tumor properties (Tumor Purity, Mutation Burden). In Indian oral squamous cell carcinoma (OSCC-GB), chewing tobacco (associated with the <strong>SBS29 mutational signature</strong>) heavily synergizes with TP53 status to suppress anti-tumor immune responses (CD8A), which the Random Survival Forest expert captures via high-order interaction nodes."
      )
      
      banner_text <- if (signif) {
        sprintf("Survival profiles indicate Stacking Ensemble risk score significantly separates patient cohorts (p = %.4f).", pval)
      } else {
        sprintf("Survival profiles do not show a statistically significant risk separation (p = %.4f).", pval)
      }
      
      high_label <- "HIGH Risk Group"
      low_label <- "LOW Risk Group"
    } else {
      gene_label <- FEATURE_META[[input$feature]]$label
      gene_desc <- FEATURE_META[[input$feature]]$description
      
      direction_text <- if (hr > 1) {
        sprintf("Patients with <strong>High</strong> expression of %s have a <strong>%.1f%% higher risk</strong> of death (Hazard Ratio = %.2f, 95%% CI: %.2f–%.2f) compared to the Low group. Elevated expression is an <strong>adverse prognostic indicator</strong>.", 
                gene_label, (hr - 1) * 100, hr, ci[1], ci[2])
      } else {
        sprintf("Patients with <strong>High</strong> expression of %s have a <strong>%.1f%% lower risk</strong> of death (Hazard Ratio = %.2f, 95%% CI: %.2f–%.2f) compared to the Low group. Elevated expression is a <strong>favorable prognostic indicator</strong>.", 
                gene_label, (1 - hr) * 100, hr, ci[1], ci[2])
      }
      
      biological_link <- sprintf(
        "<strong>Functional Context</strong>: %s performs: <em>%s</em>. The clinical model shows that %s.",
        gene_label, gene_desc,
        if (hr > 1) {
          "higher expression correlates with poorer clinical outcomes, suggesting its activity might drive tumor progression or resistance"
        } else {
          "lower expression correlates with poorer clinical outcomes, indicating its role in tumor suppression or anti-tumor immunity"
        }
      )
      
      banner_text <- if (signif) {
        sprintf("Survival profiles indicate %s levels significantly separate patient risk (p = %.4f).", gene_label, pval)
      } else {
        sprintf("Survival profiles do not show a statistically significant difference (p = %.4f).", pval)
      }
      
      high_label <- "HIGH Group"
      low_label <- "LOW Group"
    }
    
    tags$div(
      class = "interpretation-layout mt-2",
      style = "font-family: 'Inter', sans-serif; color: var(--text);",
      
      # Title & Verdict Banner
      tags$div(
        style = sprintf("border-left: 3px solid %s; padding-left: 12px; margin-bottom: 16px;", verdict_color),
        tags$div(verdict_title, style = sprintf("font-size: 0.75rem; font-weight: 800; letter-spacing: 0.05em; color: %s;", verdict_color)),
        tags$div(
          class = "fs-5 fw-bold",
          banner_text
        )
      ),
      
      # Content Split: Interpretation & Prediction table
      tags$div(
        class = "row g-4",
        
        # Left: Natural language analysis
        tags$div(
          class = "col-12 col-md-7",
          tags$p(HTML(direction_text), class = "mb-3", style = "line-height: 1.6;"),
          tags$p(HTML(biological_link), class = "mb-0", style = "line-height: 1.6; font-size: 0.9rem; color: var(--text-muted);")
        ),
        
        # Right: Projected Survival Table (Predictive results)
        tags$div(
          class = "col-12 col-md-5",
          tags$div(
            class = "p-3",
            style = "background: var(--os-bg-card2); border: 1px solid var(--border); border-radius: 8px;",
            tags$div("PROJECTED PATIENT SURVIVAL RATES", style = "font-size: 0.7rem; font-weight: 700; letter-spacing: 0.05em; color: var(--text-muted); margin-bottom: 8px;"),
            tags$table(
              class = "table table-borderless table-sm mb-0 text-white",
              style = "font-size: 0.85rem; --bs-table-bg: transparent;",
              tags$thead(
                tags$tr(
                  tags$th("Cohort Group"),
                  tags$th("1-Year", class = "text-end"),
                  tags$th("3-Year", class = "text-end"),
                  tags$th("5-Year", class = "text-end")
                )
              ),
              tags$tbody(
                tags$tr(
                  tags$td(tags$span(sprintf("● %s", high_label), style = sprintf("color: %s; font-weight: 600;", COLOUR_HIGH))),
                  tags$td(high_1y, class = "text-end"),
                  tags$td(high_3y, class = "text-end"),
                  tags$td(high_5y, class = "text-end")
                ),
                tags$tr(
                  tags$td(tags$span(sprintf("● %s", low_label), style = sprintf("color: %s; font-weight: 600;", COLOUR_LOW))),
                  tags$td(low_1y, class = "text-end"),
                  tags$td(low_3y, class = "text-end"),
                  tags$td(low_5y, class = "text-end")
                )
              )
            )
          )
        )
      ),
      
      # Interactive Glossary (Hover or Click definitions)
      tags$div(
        class = "mt-4 pt-3 border-top",
        style = "border-color: var(--border) !important;",
        tags$div("💡 CLICK BELOW TO UNDERSTAND KEY SURVIVAL CONCEPTS:", style = "font-size: 0.7rem; font-weight: 700; color: var(--text-muted); margin-bottom: 12px;"),
        
        tags$div(
          class = "d-flex flex-column gap-2",
          
          tags$details(
            tags$summary("What does the Log-Rank p-value mean?", style = "cursor: pointer; font-size: 0.85rem; font-weight: 600; outline: none;"),
            tags$div(
              style = "padding: 8px 12px; font-size: 0.8rem; color: var(--text-muted); line-height: 1.5;",
              "The Log-Rank test compares the overall survival curves of the High vs Low risk groups. A p-value of less than 0.05 means that the difference we see between the two lines is highly unlikely to have occurred by chance, proving the statistical model significantly separates patient survival outcomes."
            )
          ),
          
          tags$details(
            tags$summary("What is a Hazard Ratio (HR) and its Confidence Interval (CI)?", style = "cursor: pointer; font-size: 0.85rem; font-weight: 600; outline: none;"),
            tags$div(
              style = "padding: 8px 12px; font-size: 0.8rem; color: var(--text-muted); line-height: 1.5;",
              "The Hazard Ratio compares the instant rate of events (death) in the High group compared to the Low group. An HR of 2.0 means patients in the High group die twice as fast. A 95% Confidence Interval (CI) shows the range in which we are 95% confident the true HR lies."
            )
          ),
          
          tags$details(
            tags$summary("What do the tick marks and steps on the curves represent?", style = "cursor: pointer; font-size: 0.85rem; font-weight: 600; outline: none;"),
            tags$div(
              style = "padding: 8px 12px; font-size: 0.8rem; color: var(--text-muted); line-height: 1.5;",
              "Each downward step represents the occurrence of a clinical event (death) for one or more patients, which reduces the overall survival probability. The small vertical tick marks represent censored patients—individuals who did not experience the event during follow-up."
            )
          )
        )
      )
    )
  })
  
  # ==========================================================================
  # OUTPUT: DATA TABLE
  # ==========================================================================
  
  output$data_table <- renderDT({
    # If in ensemble mode, show the unified_risk column
    feat <- if (input$analysis_mode == "ensemble") "unified_risk" else input$feature
    feat_label <- if (input$analysis_mode == "ensemble") "Unified Risk Score" else FEATURE_META[[feat]]$label
    
    data <- stratified_data() %>%
      select(patient_id, group, time, status, !!sym(feat)) %>%
      mutate(
        Group = group,
        `Follow-up (d)` = time,
        `Event` = ifelse(status == 1, "Death", "Censored"),
        `Biomarker` = round(!!sym(feat), 2)
      ) %>%
      select(patient_id, Group, `Follow-up (d)`, Event, Biomarker)
    
    # Rename Biomarker column to actual label
    names(data)[names(data) == "Biomarker"] <- feat_label
    
    DT::datatable(
      data,
      options = list(
        pageLength = 10,
        lengthMenu = list(c(10, 20, -1), c("10", "20", "All")),
        dom = "lfrtip"
      ),
      rownames = FALSE
    ) %>%
      DT::formatStyle(
        "Group",
        target = "row",
        backgroundColor = DT::styleEqual(
          c("High", "Low"),
          c(paste0(COLOUR_HIGH, "20"), paste0(COLOUR_LOW, "20"))
        )
      )
  })
  
  # ==========================================================================
  # AI SURVIVAL PREDICTOR — Multi-variate Stacking Ensemble
  # ==========================================================================
  
  output$prediction_result <- renderUI({
    # Show placeholder before first run
    if (is.null(input$btn_predict) || input$btn_predict == 0) {
      return(tags$div(
        class = "text-center py-4",
        style = "color: var(--text-muted); font-size: 0.85rem;",
        bs_icon("robot"),
        tags$p("Set your biomarker values above and click ", tags$strong("Run Prediction"), " to see the AI-generated survival risk analysis.", style = "margin-top: 6px;")
      ))
    }
    NULL
  })
  
  observeEvent(input$btn_predict, {
    ens <- ensemble_results()
    
    # Build new patient data frame with all 8 features
    new_pt <- data.frame(
      HPV_E6E7         = input$pred_HPV_E6E7,
      Tobacco_Exposure = input$pred_Tobacco_Exposure,
      TP53             = input$pred_TP53,
      CD8A             = input$pred_CD8A,
      EGFR             = input$pred_EGFR,
      MKI67            = input$pred_MKI67,
      TumorPurity      = input$pred_TumorPurity,
      MutationBurden   = input$pred_MutBurden
    )
    
    # Predict using stacking ensemble
    pred <- predict_stacked_ensemble(ens, new_pt)
    
    # Risk tier classification based on the predicted risk percentile.
    # To compute percentile, we compare the patient's predicted risk score against the distribution of unified risk scores in the active cohort.
    active_cohort <- if (input$cohort_select == "indian") {
      INDIAN_COHORT
    } else if (input$cohort_select == "uploaded") {
      req(uploaded_data())
    } else {
      GLOBAL_COHORT
    }
    # Compute unified risk for all patients in active cohort
    all_risks <- ens$unified_risk
    
    pct <- round(mean(all_risks < pred$unified_risk, na.rm = TRUE) * 100)
    
    # Risk tier classification
    risk_tier <- if (pct >= 75) {
      list(label = "HIGH RISK",     color = COLOUR_HIGH, icon = "exclamation-triangle-fill", bg = "rgba(239,68,68,0.12)")
    } else if (pct >= 40) {
      list(label = "MODERATE RISK", color = "#f59e0b",   icon = "dash-circle-fill",          bg = "rgba(245,158,11,0.12)")
    } else {
      list(label = "LOW RISK",      color = COLOUR_LOW,  icon = "check-circle-fill",          bg = "rgba(34,197,94,0.12)")
    }
    
    # Driver biomarkers (contributions to the linear expert Cox model predictions)
    # The coefficients represent the weights in the Cox model
    coefs <- coef(ens$full_cox)
    contributions <- c(
      HPV_E6E7         = coefs["HPV_E6E7"]         * input$pred_HPV_E6E7,
      Tobacco_Exposure = coefs["Tobacco_Exposure"] * input$pred_Tobacco_Exposure,
      TP53             = coefs["TP53"]             * input$pred_TP53,
      CD8A             = coefs["CD8A"]             * input$pred_CD8A,
      EGFR             = coefs["EGFR"]             * input$pred_EGFR,
      MKI67            = coefs["MKI67"]            * input$pred_MKI67,
      TumorPurity      = coefs["TumorPurity"]      * input$pred_TumorPurity,
      MutationBurden   = coefs["MutationBurden"]   * input$pred_MutBurden
    )
    # Filter out any NAs
    contributions <- contributions[!is.na(contributions)]
    top_drivers <- sort(abs(contributions), decreasing = TRUE)
    top_drivers_names <- names(top_drivers)[1:min(3, length(top_drivers))]
    # Map back to readable labels
    top_drivers_lbls <- sapply(top_drivers_names, function(nm) {
      if (nm == "MutationBurden") "TMB"
      else if (nm == "Tobacco_Exposure") "Tobacco"
      else if (nm == "HPV_E6E7") "HPV E6/E7"
      else nm
    })
    
    output$prediction_result <- renderUI({
      tags$div(
        # Risk banner
        tags$div(
          class = "d-flex align-items-center gap-3 p-3 rounded mb-3",
          style = sprintf("background: %s; border: 1.5px solid %s; border-radius: 10px;", risk_tier$bg, risk_tier$color),
          tags$div(
            style = sprintf("font-size: 2rem; color: %s;", risk_tier$color),
            bs_icon(risk_tier$icon)
          ),
          tags$div(
            tags$div(
              risk_tier$label,
              style = sprintf("font-weight: 800; font-size: 1.1rem; color: %s; letter-spacing: 0.05em;", risk_tier$color)
            ),
            tags$div(
              sprintf("Risk Percentile: %d%% of cohort has lower predicted risk", pct),
              style = "font-size: 0.8rem; color: var(--text-muted);"
            )
          )
        ),
        
        # Survival probability table
        tags$div(
          class = "mb-3",
          tags$div("Predicted Survival Probability", style = "font-size: 0.8rem; font-weight: 700; color: var(--text-muted); text-transform: uppercase; letter-spacing: 0.08em; margin-bottom: 6px;"),
          tags$div(
            class = "d-flex gap-2",
            tags$div(
              class = "flex-fill text-center p-3 rounded",
              style = "background: var(--panel-bg2); border: 1px solid var(--border);",
              tags$div(sprintf("%.1f%%", pred$surv_1yr), style = "font-size: 1.5rem; font-weight: 800; color: var(--accent);"),
              tags$div("1-Year", style = "font-size: 0.75rem; color: var(--text-muted);")
            ),
            tags$div(
              class = "flex-fill text-center p-3 rounded",
              style = "background: var(--panel-bg2); border: 1px solid var(--border);",
              tags$div(sprintf("%.1f%%", pred$surv_3yr), style = "font-size: 1.5rem; font-weight: 800; color: var(--accent);"),
              tags$div("3-Year", style = "font-size: 0.75rem; color: var(--text-muted);")
            ),
            tags$div(
              class = "flex-fill text-center p-3 rounded",
              style = "background: var(--panel-bg2); border: 1px solid var(--border);",
              tags$div(sprintf("%.1f%%", pred$surv_5yr), style = "font-size: 1.5rem; font-weight: 800; color: var(--accent);"),
              tags$div("5-Year", style = "font-size: 0.75rem; color: var(--text-muted);")
            )
          )
        ),
        
        # Model interpretation
        tags$div(
          class = "p-3 rounded mb-3",
          style = "background: var(--panel-bg2); border: 1px solid var(--border); font-size: 0.82rem; line-height: 1.6;",
          tags$div("🔬 Model Interpretation", style = "font-weight: 700; margin-bottom: 6px;"),
          tags$p(sprintf(
            "This patient's biomarker profile yields a Stacking Meta-learner unified risk score of %.3f. This places them in the %s category. ",
            pred$unified_risk, risk_tier$label
          )),
          tags$p(sprintf(
            "Top prognostic drivers: %s — these biomarkers most strongly influence this patient's predicted outcome.",
            paste(top_drivers_lbls, collapse = ", ")
          )),
          tags$p(
            "⚠️ ",
            tags$em("This is a research-grade statistical prediction trained on clinical patient cohorts. It is not a clinical diagnostic tool.")
          )
        ),
        
        # Model info badge
        tags$div(
          class = "d-flex gap-2 flex-wrap",
          tags$span("Model: Stacking Ensemble (Cox + RSF)", style = "font-size: 0.72rem; color: var(--text-muted); background: var(--panel-bg2); padding: 3px 8px; border-radius: 10px;"),
          tags$span(sprintf("Training: %s, n=%d", 
                            if (input$cohort_select == "indian") {
                              "Indian OSCC-GB"
                            } else if (input$cohort_select == "uploaded") {
                              "Custom Uploaded"
                            } else {
                              "Global HNSC"
                            },
                            nrow(active_cohort)), 
                    style = "font-size: 0.72rem; color: var(--text-muted); background: var(--panel-bg2); padding: 3px 8px; border-radius: 10px;"),
          tags$span(sprintf("C-index: %.3f", ens$c_index_meta), style = "font-size: 0.72rem; color: var(--text-muted); background: var(--panel-bg2); padding: 3px 8px; border-radius: 10px;")
        )
      )
    })
  })
  
  # ==========================================================================
  # DOWNLOADS
  # ==========================================================================
  
  output$download_pdf <- downloadHandler(
    filename = function() {
      name_part <- if (input$analysis_mode == "ensemble") "Ensemble" else input$feature
      sprintf("OncoStratify_%s_%s.pdf", name_part, Sys.Date())
    },
    content = function(file) {
      model <- cox_model()
      coef <- model$coefficients[1]
      hr <- exp(coef)
      ci <- exp(confint(model))
      pval <- logrank_p()
      
      if (input$analysis_mode == "ensemble") {
        ens <- ensemble_results()
        title_str <- sprintf("KM Survival Curve — Stacking Ensemble (%s split)", input$split_method)
        legend_labs <- c("High Risk Group", "Low Risk Group")
        stats_label <- sprintf(
          "Stacking Ensemble (MoE) Stats:\nCox Expert C-index: %.3f\nRSF Expert C-index: %.3f\nMeta-Learner C-index: %.3f\nArbitrator Weight (Cox): %.1f%%\nArbitrator Weight (RSF): %.1f%%\nLog-rank p-value: %.4f",
          ens$c_index_cox, ens$c_index_rsf, ens$c_index_meta,
          ens$trust_cox, ens$trust_rsf, pval
        )
      } else {
        title_str <- sprintf("KM Survival Curve — %s (%s split)", 
                             FEATURE_META[[input$feature]]$label,
                             input$split_method)
        legend_labs <- c("High Expression", "Low Expression")
        stats_label <- sprintf(
          "Cox Hazard Ratio (HR): %.2f\n95%% CI: %.2f – %.2f\nLog-rank p-value: %.4f\nVerdict: %s",
          hr, ci[1], ci[2], pval,
          if (pval < 0.05) "SIGNIFICANT BIOMARKER" else "NON-SIGNIFICANT"
        )
      }
      
      p <- suppressWarnings(suppressMessages(survminer::ggsurvplot(
        km_fit(),
        data = stratified_data(),
        risk.table = input$show_risktab,
        conf.int = input$show_ci,
        pval = FALSE,
        palette = c(COLOUR_HIGH, COLOUR_LOW),
        title = title_str,
        xlab = "Survival Follow-up Time (days)",
        ylab = "Probability of Overall Survival",
        legend.labs = legend_labs,
        censor.shape = "|",
        censor.size = 5.5,
        size = 1.2,
        risk.table.col = "strata",
        ggtheme = LIGHT_PLOT_THEME
      )))
      
      # Programmatically remove colour label to prevent ggplot2 "Ignoring unknown labels: colour : Strata" warnings
      p$plot$labels$colour <- NULL
      if (!is.null(p$table)) {
        p$table$labels$colour <- NULL
      }
      
      # Add stats label annotation
      p$plot <- p$plot +
        ggplot2::annotate(
          "text",
          x = 50,
          y = 0.12,
          label = stats_label,
          hjust = 0,
          color = LIGHT$fg,
          size = 4,
          fontface = "bold.italic",
          lineheight = 1.2
        )
      
      pdf(file, width = 10, height = 7.5, bg = "white")
      suppressWarnings(print(p))
      dev.off()
    }
  )
  
  output$download_csv <- downloadHandler(
    filename = function() {
      name_part <- if (input$analysis_mode == "ensemble") "Ensemble" else input$feature
      sprintf("OncoStratify_%s_%s.csv", name_part, Sys.Date())
    },
    content = function(file) {
      write.csv(stratified_data(), file, row.names = FALSE)
    }
  )
  
  log_info("Server initialized")
}

if (interactive()) cat("✓ Server loaded\n")
