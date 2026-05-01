# ============================================================
# DrData — Deployment Module
# Save / load / predict with trained models
# SECURITY: readRDS() only — never load()
# ============================================================

deploymentUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(title = "Model Deployment", width = 4,
          status = "primary", solidHeader = TRUE,

          h5("Save current model"),
          uiOutput(ns("save_status_ui")),
          downloadButton(ns("save_model"),
                         "Save Model (.rds)",
                         class = "btn-warning btn-block"),
          hr(),

          h5("Load a saved model"),
          fileInput(ns("model_file"), "Upload Model (.rds)",
                    accept = ".rds"),
          actionButton(ns("load_model"), "Load Model",
                       class = "btn-info btn-block"),
          hr(),

          h5("Predict on new data"),
          fileInput(ns("new_data"), "Upload New Data (.csv)",
                    accept = ".csv"),
          numericInput(ns("seed_pred"), "Random seed",
                       value = 42, min = 1, max = 99999),
          actionButton(ns("predict_btn"), "Predict",
                       class = "btn-success btn-block",
                       icon  = icon("play")),
          hr(),
          downloadButton(ns("dl_preds"), "Download Predictions",
                         class = "btn-secondary btn-block")
      ),

      box(title = "Predictions", width = 8,
          status = "info", solidHeader = TRUE,

          uiOutput(ns("model_info_ui")),
          hr(),
          DT::dataTableOutput(ns("pred_table"))
      )
    )
  )
}

# ============================================================
# SERVER
# ============================================================

deploymentServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    # ── Save status ───────────────────────────────────────────────────────
    output$save_status_ui <- renderUI({
      if (is.null(rv$best_model)) {
        tags$div(class = "alert alert-warning", style = "font-size:12px;",
                 icon("exclamation-triangle"),
                 " No model available. Run AutoML first.")
      } else {
        tags$div(class = "alert alert-success", style = "font-size:12px;",
                 icon("check-circle"), " Model ready to save: ",
                 tags$strong(rv$best_algo %||% class(rv$best_model)[1L]))
      }
    })

    # ── Save model ────────────────────────────────────────────────────────
    output$save_model <- downloadHandler(
      filename = function() {
        paste0("DrData_", rv$best_algo %||% "model", "_",
               format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
      },
      content = function(file) {
        req(rv$best_model)
        saveRDS(list(
          model    = rv$best_model,
          algo     = rv$best_algo,
          task     = rv$task,
          features = rv$train_data$features,
          target   = rv$train_data$target
        ), file = file)
        showNotification("Model saved!", type = "message")
      }
    )

    # ── Load model — SAFE: readRDS, never load() ──────────────────────────
    observeEvent(input$load_model, {
      req(input$model_file)
      obj <- tryCatch(
        readRDS(input$model_file$datapath),
        error = function(e) {
          showNotification(paste("Cannot read model:", e$message),
                           type = "error")
          NULL
        }
      )
      if (!is.null(obj)) {
        if (is.list(obj) && "model" %in% names(obj)) {
          rv$best_model <- obj$model
          rv$best_algo  <- obj$algo  %||% "Loaded"
          rv$task       <- obj$task  %||% "unknown"
          if (!is.null(obj$features) && !is.null(obj$target)) {
            rv$train_data <- list(features = obj$features,
                                  target   = obj$target)
          }
        } else {
          rv$best_model <- obj
          rv$best_algo  <- "Loaded"
        }
        showNotification("Model loaded successfully!", type = "message")
      }
    })

    # ── Model info banner ─────────────────────────────────────────────────
    output$model_info_ui <- renderUI({
      if (is.null(rv$best_model)) {
        tags$div(class = "alert alert-warning",
                 icon("info-circle"),
                 " Load or train a model, then upload new data to predict.")
      } else {
        feat <- if (!is.null(rv$train_data$features))
          paste(rv$train_data$features, collapse=", ")
        else "—"
        tagList(
          tags$div(class = "alert alert-success",
                   icon("robot"), " Active model: ",
                   tags$strong(rv$best_algo %||% class(rv$best_model)[1]),
                   " | Task: ", rv$task %||% "unknown"),
          tags$p(tags$strong("Expected features: "), feat,
                 style = "font-size:12px; color:#666;")
        )
      }
    })

    # ── Predictions ───────────────────────────────────────────────────────
    preds_rv <- reactiveVal(NULL)

    observeEvent(input$predict_btn, {
      req(rv$best_model, input$new_data)
      new_data <- tryCatch(
        utils::read.csv(input$new_data$datapath),
        error = function(e) {
          showNotification(paste("Error reading CSV:", e$message),
                           type = "error")
          NULL
        }
      )
      if (is.null(new_data)) return()

      set.seed(input$seed_pred)
      pred <- tryCatch({
        model <- rv$best_model
        if (inherits(model, "gbm")) {
          gbm::predict.gbm(model, new_data, n.trees = model$n.trees %||% 100L)
        } else if (inherits(model, c("glm"))) {
          stats::predict(model, new_data, type = "response")
        } else {
          stats::predict(model, new_data)
        }
      }, error = function(e) {
        showNotification(paste("Prediction error:", e$message),
                         type = "error")
        NULL
      })
      if (!is.null(pred)) {
        preds_rv(data.frame(Predicted = pred))
        showNotification("Predictions generated!", type = "message")
      }
    })

    output$pred_table <- DT::renderDataTable({
      req(preds_rv())
      DT::datatable(preds_rv(),
                    options  = list(scrollX = TRUE, pageLength = 15),
                    rownames = TRUE)
    })

    output$dl_preds <- downloadHandler(
      filename = function() paste0("DrData_predictions_",
                                   format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
      content  = function(file) {
        req(preds_rv())
        utils::write.csv(preds_rv(), file, row.names = FALSE)
      }
    )
  })
}
