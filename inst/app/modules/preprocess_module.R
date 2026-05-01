# =============================================================================
# preprocess_module.R
# DrData – Data Preprocessing Module
# Depends on: utils_preprocess.R (sourced from global.R or app.R)
# =============================================================================

# ── UI ────────────────────────────────────────────────────────────────────────
preprocessUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      
      # ── Left panel: controls ──────────────────────────────────────────────
      box(title = "Preprocessing Options", width = 4,
          status = "primary", solidHeader = TRUE,
          
          # --- Missing Values ---
          h5("Missing Values"),
          selectInput(ns("missing_method"), "Imputation Method",
                      choices = c(
                        "None"              = "none",
                        "Remove rows"       = "remove",
                        "Mean imputation"   = "mean",
                        "Median imputation" = "median",
                        "Mode imputation"   = "mode"
                      )),
          numericInput(ns("missing_col_thresh"),
                       "Drop columns with missing > (%)",
                       value = 100, min = 0, max = 100, step = 5),
          actionButton(ns("apply_missing"), "Apply Missing Value Handling",
                       class = "btn-danger", width = "100%"),
          hr(),
          
          # --- Duplicates ---
          h5("Duplicated Data"),
          selectInput(ns("dup_scope"), "Check duplicates based on",
                      choices = c("All columns"      = "all",
                                  "Selected columns" = "selected")),
          conditionalPanel(
            condition = paste0("input['", ns("dup_scope"), "'] == 'selected'"),
            selectInput(ns("dup_cols"), "Select columns",
                        choices = NULL, multiple = TRUE)
          ),
          actionButton(ns("apply_dedup"), "Remove Duplicates",
                       class = "btn-danger", width = "100%"),
          hr(),
          
          # --- Drop Columns ---
          h5("Drop Columns"),
          radioButtons(ns("drop_col_mode"), "Select by",
                       choices  = c("Column Name"   = "name",
                                    "Column Number" = "number"),
                       inline   = TRUE),
          conditionalPanel(
            condition = paste0("input['", ns("drop_col_mode"), "'] == 'name'"),
            selectInput(ns("drop_col_names"), "Select columns to drop",
                        choices = NULL, multiple = TRUE)
          ),
          conditionalPanel(
            condition = paste0("input['", ns("drop_col_mode"), "'] == 'number'"),
            textInput(ns("drop_col_indices"),
                      "Column numbers (e.g. 1,3,5 or 2:4)",
                      placeholder = "e.g. 1,3,5 or 2:4")
          ),
          actionButton(ns("apply_drop_cols"), "Drop Columns",
                       class = "btn-danger", width = "100%"),
          hr(),
          
          # --- Drop Rows ---
          h5("Drop Rows"),
          textInput(ns("drop_row_indices"),
                    "Row numbers (e.g. 1,5,10 or 3:7)",
                    placeholder = "e.g. 1,5,10 or 3:7"),
          actionButton(ns("apply_drop_rows"), "Drop Rows",
                       class = "btn-danger", width = "100%"),
          hr(),
          
          # --- Normalization ---
          h5("Normalization"),
          selectInput(ns("scale_method"), "Scale/Normalize",
                      choices = c(
                        "None"          = "none",
                        "Z-score"       = "zscore",
                        "Min-Max"       = "minmax",
                        "Log transform" = "log"
                      )),
          selectInput(ns("scale_vars"), "Apply to variables",
                      choices = NULL, multiple = TRUE),
          hr(),
          
          # --- Encoding ---
          h5("One-Hot Encoding"),
          selectInput(ns("encode_vars"), "Encode categorical columns",
                      choices = NULL, multiple = TRUE),
          checkboxInput(ns("encode_full_rank"),
                        "Full rank (drop one level per factor)",
                        value = TRUE),
          hr(),
          
          # --- Outlier Detection ---
          h5("Outlier Detection"),
          selectInput(ns("outlier_vars"), "Apply to variables",
                      choices = NULL, multiple = TRUE),
          selectInput(ns("outlier_method"), "Method",
                      choices = c("IQR" = "iqr", "Z-score" = "zscore")),
          selectInput(ns("outlier_action"), "Action",
                      choices = c("Flag (add column)" = "flag",
                                  "Remove rows"       = "remove")),
          actionButton(ns("apply_outlier"), "Detect Outliers",
                       class = "btn-warning", width = "100%"),
          hr(),
          
          # --- Feature Filters ---
          h5("Feature Filters"),
          actionButton(ns("apply_nzv"), "Remove Near-Zero Variance",
                       class = "btn-warning", width = "100%"),
          br(), br(),
          numericInput(ns("cor_threshold"), "Correlation threshold",
                       value = 0.90, min = 0.5, max = 1.0, step = 0.05),
          actionButton(ns("apply_cor_filter"), "Remove High-Correlation Features",
                       class = "btn-warning", width = "100%"),
          hr(),
          
          # --- Type Conversion ---
          h5("Variable Type Conversion"),
          selectInput(ns("type_conv_vars"), "Select variables",
                      choices = NULL, multiple = TRUE),
          selectInput(ns("type_conv_to"), "Convert to",
                      choices = c("Factor"    = "factor",
                                  "Numeric"   = "numeric",
                                  "Character" = "character")),
          actionButton(ns("apply_type_conv"), "Convert Type",
                       class = "btn-info", width = "100%"),
          hr(),
          
          # --- Binning ---
          h5("Bin Numeric Variable"),
          selectInput(ns("bin_var"), "Select variable",
                      choices = NULL),
          numericInput(ns("bin_n"), "Number of bins",
                       value = 4, min = 2, max = 20, step = 1),
          actionButton(ns("apply_bin"), "Apply Binning",
                       class = "btn-info", width = "100%"),
          hr(),
          
          # --- Rename Column ---
          h5("Rename Column"),
          selectInput(ns("rename_col"), "Select Column", choices = NULL),
          textInput(ns("new_col_name"), "New Name",
                    placeholder = "Enter new column name"),
          actionButton(ns("apply_rename"), "Rename",
                       class = "btn-info", width = "100%"),
          hr(),
          
          # --- Apply / Reset ---
          actionButton(ns("apply"), "Apply Normalization & Encoding",
                       class = "btn-primary", width = "100%"),
          br(), br(),
          actionButton(ns("reset"), "Reset to Original",
                       class = "btn-warning", width = "100%")
      ),
      
      # ── Right panel: preview ──────────────────────────────────────────────
      box(title = "Data Preview", width = 8,
          status = "info", solidHeader = TRUE,
          
          h5("Missing Values per Column"),
          DTOutput(ns("missing_summary")),
          hr(),
          verbatimTextOutput(ns("dup_summary")),
          hr(),
          verbatimTextOutput(ns("summary")),
          hr(),
          h5("Preprocessing History"),
          verbatimTextOutput(ns("step_log")),
          hr(),
          DTOutput(ns("preview"))
      )
    )
  )
}

# ── Server ────────────────────────────────────────────────────────────────────
preprocessServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    
    # Initialise preprocessing log if not present
    observe({
      if (is.null(rv$preprocess_steps)) rv$preprocess_steps <- character(0)
    })
    
    # Helper: append to log and update rv$working_data in one call
    apply_step <- function(result) {
      rv$working_data     <- result$data
      rv$preprocess_steps <- c(rv$preprocess_steps,
                               paste0("[", format(Sys.time(), "%H:%M:%S"), "] ",
                                      result$log))
    }
    
    # ── Update selectInputs when data changes ────────────────────────────────
    observe({
      req(rv$working_data)
      df       <- rv$working_data
      all_vars <- names(df)
      num_vars <- names(df)[vapply(df, is.numeric, logical(1L))]
      cat_vars <- names(df)[vapply(df, function(x) is.character(x) | is.factor(x), logical(1L))]
      
      updateSelectInput(session, "scale_vars",      choices = num_vars)
      updateSelectInput(session, "encode_vars",     choices = cat_vars)
      updateSelectInput(session, "rename_col",      choices = all_vars)
      updateSelectInput(session, "dup_cols",        choices = all_vars)
      updateSelectInput(session, "drop_col_names",  choices = all_vars)
      updateSelectInput(session, "outlier_vars",    choices = num_vars)
      updateSelectInput(session, "type_conv_vars",  choices = all_vars)
      updateSelectInput(session, "bin_var",         choices = num_vars)
    })
    
    # ── Missing values summary ───────────────────────────────────────────────
    output$missing_summary <- renderDT({
      req(rv$working_data)
      df <- rv$working_data
      miss_df <- data.frame(
        Column  = names(df),
        Missing = vapply(df, function(x) sum(is.na(x)), integer(1L)),
        Percent = round(vapply(df, function(x) mean(is.na(x)) * 100, numeric(1L)), 1),
        Type    = vapply(df, function(x) class(x)[1L], character(1L)),
        stringsAsFactors = FALSE
      )
      miss_df <- miss_df[order(-miss_df$Missing), ]
      datatable(miss_df, rownames = FALSE,
                options = list(pageLength = 8, scrollX = TRUE)) %>%
        formatStyle("Percent",
                    background       = styleColorBar(c(0, 100), "#e74c3c"),
                    backgroundSize   = "100% 90%",
                    backgroundRepeat = "no-repeat",
                    backgroundPosition = "center")
    })
    
    # ── Apply missing value handling ─────────────────────────────────────────
    observeEvent(input$apply_missing, {
      req(rv$working_data)
      tryCatch({
        result <- handle_missing_values(
          df         = rv$working_data,
          method     = input$missing_method,
          col_thresh = input$missing_col_thresh / 100
        )
        apply_step(result)
        showNotification(
          paste0("Done. Rows: ", nrow(result$data),
                 " | Cols: ", ncol(result$data)),
          type = "message"
        )
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
    
    # ── Duplicate summary ────────────────────────────────────────────────────
    output$dup_summary <- renderPrint({
      req(rv$working_data)
      df    <- rv$working_data
      n_dup <- sum(duplicated(df))
      cat("Duplicate rows:", n_dup, "out of", nrow(df), "\n")
    })
    
    # ── Remove duplicates ────────────────────────────────────────────────────
    observeEvent(input$apply_dedup, {
      req(rv$working_data)
      scope <- if (input$dup_scope == "all") "all" else input$dup_cols
      tryCatch({
        result <- remove_duplicates(rv$working_data, scope = scope)
        apply_step(result)
        showNotification(result$log, type = "message")
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
    
    # ── Drop columns ─────────────────────────────────────────────────────────
    observeEvent(input$apply_drop_cols, {
      req(rv$working_data)
      df <- rv$working_data
      
      cols_to_drop <- if (input$drop_col_mode == "name") {
        req(input$drop_col_names)
        input$drop_col_names
      } else {
        req(input$drop_col_indices)
        idx <- parse_indices(input$drop_col_indices, ncol(df))
        if (length(idx) == 0) {
          showNotification("No valid column indices entered.", type = "error")
          return()
        }
        names(df)[idx]
      }
      
      tryCatch({
        result <- drop_columns(df, cols_to_drop)
        apply_step(result)
        updateTextInput(session, "drop_col_indices", value = "")
        showNotification(result$log, type = "message")
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
    
    # ── Drop rows ────────────────────────────────────────────────────────────
    observeEvent(input$apply_drop_rows, {
      req(rv$working_data, input$drop_row_indices)
      idx <- parse_indices(input$drop_row_indices, nrow(rv$working_data))
      if (length(idx) == 0) {
        showNotification("No valid row indices entered.", type = "error")
        return()
      }
      tryCatch({
        result <- drop_rows(rv$working_data, idx)
        apply_step(result)
        updateTextInput(session, "drop_row_indices", value = "")
        showNotification(result$log, type = "message")
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
    
    # ── Rename column ────────────────────────────────────────────────────────
    observeEvent(input$apply_rename, {
      req(rv$working_data, input$rename_col, input$new_col_name)
      new_name <- trimws(input$new_col_name)
      if (nchar(new_name) == 0) {
        showNotification("Please enter a new column name.", type = "error")
        return()
      }
      tryCatch({
        result <- rename_variable(rv$working_data, input$rename_col, new_name)
        apply_step(result)
        updateTextInput(session, "new_col_name", value = "")
        showNotification(result$log, type = "message")
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
    
    # ── Outlier detection ────────────────────────────────────────────────────
    observeEvent(input$apply_outlier, {
      req(rv$working_data, input$outlier_vars)
      tryCatch({
        result <- detect_outliers(
          df     = rv$working_data,
          vars   = input$outlier_vars,
          method = input$outlier_method,
          action = input$outlier_action
        )
        apply_step(result)
        showNotification(result$log, type = "message")
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
    
    # ── Near-zero variance filter ────────────────────────────────────────────
    observeEvent(input$apply_nzv, {
      req(rv$working_data)
      tryCatch({
        result <- remove_near_zero_variance(rv$working_data)
        apply_step(result)
        showNotification(result$log, type = "message")
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
    
    # ── Correlation-based filter ─────────────────────────────────────────────
    observeEvent(input$apply_cor_filter, {
      req(rv$working_data)
      tryCatch({
        result <- remove_correlated_features(
          rv$working_data,
          threshold = input$cor_threshold
        )
        apply_step(result)
        showNotification(result$log, type = "message")
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
    
    # ── Type conversion ──────────────────────────────────────────────────────
    observeEvent(input$apply_type_conv, {
      req(rv$working_data, input$type_conv_vars)
      tryCatch({
        result <- convert_variable_type(
          df   = rv$working_data,
          vars = input$type_conv_vars,
          to   = input$type_conv_to
        )
        apply_step(result)
        showNotification(result$log, type = "message")
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
    
    # ── Binning ──────────────────────────────────────────────────────────────
    observeEvent(input$apply_bin, {
      req(rv$working_data, input$bin_var)
      tryCatch({
        result <- bin_variable(
          df  = rv$working_data,
          var = input$bin_var,
          n   = input$bin_n
        )
        apply_step(result)
        showNotification(result$log, type = "message")
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
    
    # ── Normalization & Encoding (combined apply) ────────────────────────────
    observeEvent(input$apply, {
      req(rv$working_data)
      df <- rv$working_data
      
      # Scaling
      if (!is.null(input$scale_vars) && length(input$scale_vars) > 0 &&
          input$scale_method != "none") {
        tryCatch({
          result <- scale_variables(df,
                                    vars   = input$scale_vars,
                                    method = input$scale_method)
          df <- result$data
          rv$preprocess_steps <- c(rv$preprocess_steps,
                                   paste0("[", format(Sys.time(), "%H:%M:%S"), "] ",
                                          result$log))
        }, error = function(e) {
          showNotification(paste("Scaling error:", e$message), type = "error")
        })
      }
      
      # Encoding
      if (!is.null(input$encode_vars) && length(input$encode_vars) > 0) {
        tryCatch({
          result <- encode_categorical(df,
                                       vars      = input$encode_vars,
                                       full_rank = input$encode_full_rank)
          df <- result$data
          rv$preprocess_steps <- c(rv$preprocess_steps,
                                   paste0("[", format(Sys.time(), "%H:%M:%S"), "] ",
                                          result$log))
        }, error = function(e) {
          showNotification(paste("Encoding error:", e$message), type = "error")
        })
      }
      
      rv$working_data <- df
      showNotification("Normalization & Encoding applied.", type = "message")
    })
    
    # ── Reset ────────────────────────────────────────────────────────────────
    observeEvent(input$reset, {
      req(rv$data)
      rv$working_data     <- rv$data
      rv$preprocess_steps <- character(0)
      showNotification("Data reset to original.", type = "warning")
    })
    
    # ── Summary ──────────────────────────────────────────────────────────────
    output$summary <- renderPrint({
      req(rv$working_data)
      df <- rv$working_data
      cat("Rows:", nrow(df), "| Cols:", ncol(df), "\n\n")
      str(df)
    })
    
    # ── Step log ─────────────────────────────────────────────────────────────
    output$step_log <- renderPrint({
      steps <- rv$preprocess_steps
      if (length(steps) == 0) {
        cat("No preprocessing steps applied yet.\n")
      } else {
        cat(paste(steps, collapse = "\n"), "\n")
      }
    })
    
    # ── Data preview ─────────────────────────────────────────────────────────
    output$preview <- renderDT({
      req(rv$working_data)
      datatable(rv$working_data,
                rownames = FALSE,
                options  = list(pageLength = 10, scrollX = TRUE))
    })
    
  })
}

        