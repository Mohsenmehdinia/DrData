# ============================================================
# DrData — AutoML Module
# Auto-selects best model for regression or classification
# Stores results in rv for Model Comparison + Explainability
# ============================================================

automlUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(title = "AutoML Settings", width = 3,
          status = "primary", solidHeader = TRUE,

          selectInput(ns("target"), "Target Variable", choices = NULL),
          selectizeInput(ns("features"), "Feature Variables",
                         choices = NULL, multiple = TRUE),
          hr(),
          sliderInput(ns("test_size"), "Test Size (%)",
                      min = 10, max = 40, value = 20, step = 5),
          numericInput(ns("seed"), "Random Seed",
                       value = 42, min = 1, max = 99999),
          checkboxInput(ns("scale"), "Scale Numeric Features", TRUE),
          hr(),
          h5("Algorithms to Try"),
          checkboxGroupInput(ns("algos"), NULL,
            choices = c(
              "Logistic / Linear Regression" = "glm",
              "Random Forest"                = "rf",
              "Gradient Boosting"            = "gbm",
              "Decision Tree"                = "tree",
              "SVM"                          = "svm",
              "Neural Network"               = "nnet"
            ),
            selected = c("glm", "rf", "gbm")
          ),
          hr(),
          actionButton(ns("run"), "Run AutoML",
                       class = "btn-success btn-block",
                       icon = icon("robot"))
      ),

      box(title = "AutoML Results", width = 9,
          status = "info", solidHeader = TRUE,

          tabsetPanel(
            tabPanel("Model Ranking",
                     br(),
                     uiOutput(ns("task_badge")),
                     br(),
                     plotlyOutput(ns("ranking_plot"), height = "320px"),
                     br(),
                     DT::dataTableOutput(ns("ranking_table"))
            ),
            tabPanel("Best Model",
                     br(),
                     uiOutput(ns("best_model_banner")),
                     br(),
                     verbatimTextOutput(ns("best_model_text"))
            ),
            tabPanel("Predictions",
                     br(),
                     DT::dataTableOutput(ns("pred_table")),
                     br(),
                     downloadButton(ns("dl_preds"),
                                    "Download Predictions",
                                    class = "btn-warning")
            )
          )
      )
    )
  )
}

# ============================================================
# SERVER
# ============================================================

automlServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    # ── Update variable selectors ────────────────────────────────────────
    observe({
      req(rv$working_data)
      vars <- names(rv$working_data)
      updateSelectInput(session, "target",   choices = vars)
      updateSelectizeInput(session, "features", choices = vars)
    })

    # ── Core AutoML reactive ─────────────────────────────────────────────
    automl_result <- eventReactive(input$run, {
      req(rv$working_data, input$target, input$features, input$algos)

      df       <- rv$working_data
      target   <- input$target
      features <- input$features
      algos    <- input$algos

      # --- Prepare data ---
      cols  <- c(target, features)
      df    <- df[stats::complete.cases(df[, cols]), cols, drop = FALSE]

      # Detect task
      y      <- df[[target]]
      task   <- if (is.numeric(y) && length(unique(y)) > 10) "regression"
                else "classification"

      if (task == "classification") {
        df[[target]] <- as.factor(df[[target]])
        y            <- df[[target]]
      }

      # Train / test split (BEFORE scaling to prevent data leakage)
      set.seed(input$seed)
      n       <- nrow(df)
      test_n  <- floor(n * input$test_size / 100)
      test_i  <- sample(seq_len(n), test_n)
      train   <- df[-test_i, , drop = FALSE]
      test    <- df[ test_i, , drop = FALSE]

      # Scale using TRAIN parameters only — apply same transform to test
      # This prevents data leakage from test into train
      if (isTRUE(input$scale)) {
        num_cols <- vapply(train[, features, drop = FALSE],
                           is.numeric, logical(1L))
        if (any(num_cols)) {
          num_feats <- features[num_cols]
          # Compute mean/sd from train only
          train_means <- vapply(train[, num_feats, drop = FALSE],
                                mean, numeric(1L), na.rm = TRUE)
          train_sds   <- vapply(train[, num_feats, drop = FALSE],
                                sd,   numeric(1L), na.rm = TRUE)
          train_sds[train_sds == 0] <- 1  # avoid division by zero
          # Apply to train
          for (f in num_feats) {
            train[[f]] <- (train[[f]] - train_means[f]) / train_sds[f]
            test[[f]]  <- (test[[f]]  - train_means[f]) / train_sds[f]
          }
        }
      }

      formula <- stats::as.formula(paste(target, "~", paste(features, collapse="+")))

      withProgress(message = "Training models...", value = 0, {
        results <- list()
        n_algos  <- length(algos)

        for (i in seq_along(algos)) {
          algo <- algos[i]
          incProgress(1/n_algos, detail = paste("Training", algo))

          tryCatch({
            if (task == "regression") {
              model <- switch(algo,
                glm  = stats::lm(formula, data = train),
                rf   = randomForest::randomForest(formula, data = train,
                                                  ntree = 100),
                gbm  = gbm::gbm(formula, data = train,
                                distribution = "gaussian",
                                n.trees = 100, verbose = FALSE),
                tree = rpart::rpart(formula, data = train,
                                    method = "anova"),
                svm  = e1071::svm(formula, data = train),
                nnet = nnet::nnet(formula, data = train,
                                  size = 5, linout = TRUE,
                                  trace = FALSE, maxit = 200)
              )
              preds <- if (algo == "gbm") {
                gbm::predict.gbm(model, test, n.trees = 100)
              } else {
                as.numeric(stats::predict(model, test))
              }
              y_true <- test[[target]]
              rmse   <- sqrt(mean((y_true - preds)^2))
              r2     <- 1 - sum((y_true-preds)^2)/sum((y_true-mean(y_true))^2)
              results[[algo]] <- list(
                model   = model,
                metric  = rmse,
                r2      = r2,
                preds   = preds,
                algo    = algo,
                metric_name = "RMSE"
              )
            } else {
              y_train <- train[[target]]
              y_test  <- test[[target]]
              x_train <- train[, features, drop = FALSE]
              x_test  <- test[,  features, drop = FALSE]

              model <- switch(algo,
                glm  = stats::glm(formula, data = train, family = binomial),
                rf   = randomForest::randomForest(
                         x = x_train, y = y_train, ntree = 100),
                gbm  = gbm::gbm(
                         stats::as.formula(paste(target, "~", paste(features, collapse="+"))),
                         data = train,
                         distribution = "bernoulli",
                         n.trees = 100, verbose = FALSE),
                tree = rpart::rpart(formula, data = train, method = "class"),
                svm  = e1071::svm(formula, data = train, probability = TRUE),
                nnet = nnet::nnet(formula, data = train,
                                  size = 5, trace = FALSE, maxit = 200)
              )
              preds <- if (algo == "gbm") {
                p <- gbm::predict.gbm(model, test, n.trees=100, type="response")
                factor(ifelse(p > 0.5, levels(y_test)[2], levels(y_test)[1]),
                       levels = levels(y_test))
              } else if (algo == "glm") {
                p <- stats::predict(model, test, type = "response")
                factor(ifelse(p > 0.5, levels(y_test)[2], levels(y_test)[1]),
                       levels = levels(y_test))
              } else {
                stats::predict(model, x_test)
              }
              acc <- mean(preds == y_test)
              results[[algo]] <- list(
                model   = model,
                metric  = acc,
                preds   = preds,
                algo    = algo,
                metric_name = "Accuracy"
              )
            }
          }, error = function(e) {
            showNotification(paste(algo, "failed:", e$message), type = "warning")
          })
        }
      })

      req(length(results) > 0)

      # Rank results
      metrics_df <- do.call(rbind, lapply(names(results), function(a) {
        r <- results[[a]]
        data.frame(
          Algorithm   = a,
          Score       = round(r$metric, 4),
          Metric      = r$metric_name,
          stringsAsFactors = FALSE
        )
      }))

      # Best = lowest RMSE or highest Accuracy
      best_idx   <- if (task == "regression") which.min(metrics_df$Score)
                    else which.max(metrics_df$Score)
      best_algo  <- metrics_df$Algorithm[best_idx]
      best       <- results[[best_algo]]

      # Store in rv for other modules
      rv$best_model <- best$model
      rv$best_algo  <- best_algo
      rv$task       <- task
      rv$train_data <- list(
        train    = train,
        test     = test,
        features = features,
        target   = target
      )
      run_id <- paste0(best_algo, "_", format(Sys.time(), "%H%M%S"))
      rv$automl_results_list[[run_id]] <- list(
        metrics  = metrics_df,
        task     = task,
        best     = best_algo,
        results  = results,
        train    = train,
        test     = test,
        features = features,
        target   = target
      )

      list(
        metrics    = metrics_df,
        best_algo  = best_algo,
        best       = best,
        task       = task,
        test_y     = test[[target]],
        results    = results
      )
    })

    # ── Task badge ───────────────────────────────────────────────────────
    output$task_badge <- renderUI({
      req(automl_result())
      task <- automl_result()$task
      cls  <- if (task == "regression") "label-info" else "label-success"
      tags$span(class = paste("label", cls),
                style = "font-size:13px; padding:5px 10px;",
                toupper(task))
    })

    # ── Ranking plot ─────────────────────────────────────────────────────
    output$ranking_plot <- renderPlotly({
      req(automl_result())
      df    <- automl_result()$metrics
      task  <- automl_result()$task
      best  <- automl_result()$best_algo
      df$Color <- ifelse(df$Algorithm == best, "Best", "Other")

      if (task == "regression") {
        df <- df[order(df$Score), ]
        df$Algorithm <- factor(df$Algorithm, levels = df$Algorithm)
      } else {
        df <- df[order(-df$Score), ]
        df$Algorithm <- factor(df$Algorithm, levels = df$Algorithm)
      }

      plot_ly(df, x = ~Algorithm, y = ~Score, type = "bar",
              color = ~Color,
              colors = c("Best" = "#27ae60", "Other" = "#3498db"),
              text  = ~round(Score, 4), textposition = "outside") %>%
        layout(
          yaxis   = list(title = unique(df$Metric)),
          xaxis   = list(title = ""),
          showlegend = FALSE
        )
    })

    # ── Ranking table ────────────────────────────────────────────────────
    output$ranking_table <- DT::renderDataTable({
      req(automl_result())
      DT::datatable(automl_result()$metrics,
                    options = list(pageLength = 10),
                    rownames = FALSE)
    })

    # ── Best model banner ────────────────────────────────────────────────
    output$best_model_banner <- renderUI({
      req(automl_result())
      r    <- automl_result()
      task <- r$task
      metric_lbl <- if (task == "regression")
        paste("RMSE:", round(r$best$metric, 4))
      else
        paste("Accuracy:", scales::percent(r$best$metric, 0.01))

      tags$div(class = "alert alert-success",
               icon("star"), " Best model: ",
               tags$strong(r$best_algo),
               " — ", metric_lbl)
    })

    # ── Best model summary ───────────────────────────────────────────────
    output$best_model_text <- renderPrint({
      req(automl_result())
      cat("Best Algorithm:", automl_result()$best_algo, "\n\n")
      print(summary(automl_result()$best$model))
    })

    # ── Predictions table ────────────────────────────────────────────────
    predictions <- reactive({
      req(automl_result())
      data.frame(
        Actual    = automl_result()$test_y,
        Predicted = automl_result()$best$preds
      )
    })

    output$pred_table <- DT::renderDataTable({
      DT::datatable(predictions(),
                    options = list(pageLength = 10, scrollX = TRUE),
                    rownames = TRUE)
    })

    output$dl_preds <- downloadHandler(
      filename = function() paste0("automl_predictions_",
                                   format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
      content  = function(file) utils::write.csv(predictions(), file,
                                                  row.names = FALSE)
    )
  })
}
