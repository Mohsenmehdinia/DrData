# ============================================================
# DrData — Explainability Module
# Feature Importance + Permutation Importance
# Works with any model stored in rv$best_model by AutoML
# ============================================================

explainabilityUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(title = "Explainability Settings", width = 3,
          status = "primary", solidHeader = TRUE,

          uiOutput(ns("model_status_ui")),
          hr(),

          selectInput(ns("method"), "Explanation Method",
                      choices = c(
                        "Feature Importance"    = "importance",
                        "Permutation Importance"= "permutation"
                      )
          ),

          conditionalPanel(
            condition = sprintf("input['%s'] == 'permutation'", ns("method")),
            numericInput(ns("n_perm"), "Number of Permutations",
                         value = 20, min = 5, max = 100)
          ),

          numericInput(ns("top_n"), "Show Top N Features",
                       value = 15, min = 3, max = 50),

          hr(),
          actionButton(ns("run"), "Explain Model",
                       class = "btn-primary btn-block",
                       icon  = icon("brain"))
      ),

      box(title = "Model Explanation", width = 9,
          status = "info", solidHeader = TRUE,

          tabsetPanel(
            tabPanel("Importance Plot",
                     br(),
                     plotlyOutput(ns("importance_plot"), height = "450px")
            ),
            tabPanel("Importance Table",
                     br(),
                     DT::dataTableOutput(ns("importance_table"))
            ),
            tabPanel("Interpretation",
                     br(),
                     uiOutput(ns("interpretation_ui"))
            )
          )
      )
    )
  )
}

# ============================================================
# SERVER
# ============================================================

explainabilityServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    # ── Model status banner ───────────────────────────────────────────────
    output$model_status_ui <- renderUI({
      if (is.null(rv$best_model)) {
        tags$div(class = "alert alert-warning", style = "font-size:12px;",
                 icon("exclamation-triangle"),
                 " Run AutoML first to train a model.")
      } else {
        algo <- rv$best_algo %||% class(rv$best_model)[1L]
        tags$div(class = "alert alert-success", style = "font-size:12px;",
                 icon("check-circle"),
                 " Model ready: ", tags$strong(algo))
      }
    })

    # ── Compute importance ────────────────────────────────────────────────
    importance_data <- eventReactive(input$run, {
      req(rv$best_model, rv$train_data)

      model    <- rv$best_model
      td       <- rv$train_data
      features <- td$features
      target   <- td$target
      test     <- td$test
      x_test   <- test[, features, drop = FALSE]
      y_test   <- test[[target]]

      method <- input$method

      # ── Feature Importance (model-native) ──────────────────────────────
      if (method == "importance") {
        imp <- tryCatch({
          if (inherits(model, "randomForest")) {
            m  <- randomForest::importance(model)
            data.frame(Feature    = rownames(m),
                       Importance = m[, 1L])
          } else if (inherits(model, "gbm")) {
            s <- summary(model, plotit = FALSE)
            data.frame(Feature    = s$var,
                       Importance = s$rel.inf)
          } else if (inherits(model, "rpart")) {
            vi <- model$variable.importance
            if (is.null(vi)) stop("No variable importance for this tree.")
            data.frame(Feature    = names(vi),
                       Importance = as.numeric(vi))
          } else if (inherits(model, c("lm", "glm"))) {
            co <- summary(model)$coefficients
            co <- co[rownames(co) != "(Intercept)", , drop = FALSE]
            data.frame(Feature    = rownames(co),
                       Importance = abs(co[, 1L]))
          } else {
            # Fall back to permutation for unsupported model types
            showNotification(
              "Native importance not available; switching to Permutation.",
              type = "warning")
            NULL
          }
        }, error = function(e) {
          showNotification(paste("Importance error:", e$message),
                           type = "warning")
          NULL
        })

        if (!is.null(imp)) return(imp)
        method <- "permutation"   # fallback
      }

      # ── Permutation Importance ──────────────────────────────────────────
      predict_fn <- function(m, newdata) {
        p <- tryCatch({
          if (inherits(m, "gbm")) {
            gbm::predict.gbm(m, newdata, n.trees = m$n.trees)
          } else if (inherits(m, "glm")) {
            stats::predict(m, newdata, type = "response")
          } else {
            stats::predict(m, newdata)
          }
        }, error = function(e) NULL)
        if (is.null(p)) return(rep(NA_real_, nrow(newdata)))
        as.numeric(p)
      }

      is_class <- inherits(y_test, "factor") || is.character(y_test)
      base_pred <- predict_fn(model, x_test)

      if (is.null(base_pred) || all(is.na(base_pred))) {
        showNotification("Cannot generate predictions from this model.",
                         type = "error")
        return(NULL)
      }

      base_loss <- if (is_class) mean(as.character(base_pred) !=
                                      as.character(y_test), na.rm=TRUE)
                   else           mean((as.numeric(y_test) - base_pred)^2,
                                       na.rm=TRUE)

      n_perm <- input$n_perm %||% 20

      withProgress(message = "Computing permutation importance...", {
        imp <- vapply(features, function(f) {
          losses <- replicate(n_perm, {
            x_perm       <- x_test
            x_perm[[f]]  <- sample(x_perm[[f]])
            p <- predict_fn(model, x_perm)
            if (is_class) mean(as.character(p) != as.character(y_test), na.rm=TRUE)
            else           mean((as.numeric(y_test) - p)^2, na.rm=TRUE)
          })
          mean(losses) - base_loss
        }, numeric(1L))
      })

      data.frame(Feature    = names(imp),
                 Importance = as.numeric(imp))
    })

    # ── Top-N filtered ────────────────────────────────────────────────────
    top_imp <- reactive({
      req(importance_data())
      df  <- importance_data()
      df  <- df[order(-df$Importance), , drop = FALSE]
      n   <- min(input$top_n %||% 15, nrow(df))
      df[seq_len(n), , drop = FALSE]
    })

    # ── Plot ─────────────────────────────────────────────────────────────
    output$importance_plot <- renderPlotly({
      req(top_imp())
      df <- top_imp()
      df$Feature <- factor(df$Feature,
                           levels = df$Feature[order(df$Importance)])

      plot_ly(df, x = ~Importance, y = ~Feature,
              type = "bar", orientation = "h",
              marker = list(color = "#2980b9")) %>%
        layout(
          xaxis = list(title = "Importance"),
          yaxis = list(title = ""),
          title = paste("Feature Importance —",
                        rv$best_algo %||% "Model")
        )
    })

    # ── Table ─────────────────────────────────────────────────────────────
    output$importance_table <- DT::renderDataTable({
      req(importance_data())
      df <- importance_data()[order(-importance_data()$Importance), ]
      df$Importance <- round(df$Importance, 5)
      DT::datatable(df,
                    options  = list(pageLength = 15),
                    rownames = FALSE)
    })

    # ── Interpretation text ───────────────────────────────────────────────
    output$interpretation_ui <- renderUI({
      req(top_imp())
      df   <- top_imp()
      top3 <- head(df$Feature, 3)
      algo <- rv$best_algo %||% "the model"

      tagList(
        tags$div(class = "alert alert-info",
                 tags$strong("How to read this:"),
                 tags$p("Higher importance = stronger influence on predictions."),
                 tags$p(paste0(
                   "The top 3 most influential features for ", algo, " are: ",
                   paste(top3, collapse = ", "), "."
                 ))
        ),
        tags$div(class = "alert alert-warning",
                 tags$strong("Note:"),
                 " Feature importance shows correlation with the outcome, ",
                 "not necessarily causation."
        )
      )
    })
  })
}
