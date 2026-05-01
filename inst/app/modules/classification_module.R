# ============================================================
# Classification Module
# ============================================================


classificationUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(title = "Model Settings", width = 3, status = "primary", solidHeader = TRUE,
          selectInput(ns("target"), "Target Variable", choices = NULL),
          selectInput(ns("features"), "Feature Variables", choices = NULL, multiple = TRUE),
          selectInput(ns("algorithm"), "Algorithm",
                      choices = c(
                        "Logistic Regression" = "logistic",
                        "Decision Tree"       = "tree",
                        "Random Forest"       = "rf",
                        "SVM"                 = "svm",
                        "KNN"                 = "knn",
                        "Naive Bayes"         = "nb",
                        "Gradient Boosting"   = "gbm",
                        "Neural Network"      = "nnet"
                      )
          ),
          sliderInput(ns("train_ratio"), "Train Ratio", min = 0.5, max = 0.9, value = 0.8, step = 0.05),
          hr(),
          h5("Hyperparameters"),
          uiOutput(ns("hyperparams")),
          hr(),
          actionButton(ns("run"), "Train Model", class = "btn-success", width = "100%")
      ),
      box(title = "Model Performance", width = 9, status = "info", solidHeader = TRUE,
          tabsetPanel(
            tabPanel("Metrics",
                     br(),
                     fluidRow(
                       valueBoxOutput(ns("accuracy_box"),  width = 3),
                       valueBoxOutput(ns("precision_box"), width = 3),
                       valueBoxOutput(ns("recall_box"),    width = 3),
                       valueBoxOutput(ns("f1_box"),        width = 3)
                     ),
                     hr(),
                     fluidRow(
                       box(title = "Confusion Matrix", width = 6, solidHeader = FALSE,
                           plotlyOutput(ns("conf_matrix"), height = "300px")
                       ),
                       box(title = "ROC Curve", width = 6, solidHeader = FALSE,
                           plotlyOutput(ns("roc_curve"), height = "300px")
                       )
                     )
            ),
            tabPanel("Variable Importance",
                     br(),
                     plotlyOutput(ns("var_imp"), height = "400px"),
                     br(),
                     uiOutput(ns("var_imp_note"))
            ),
            tabPanel("Predictions",
                     br(),
                     DTOutput(ns("predictions"))
            ),
            tabPanel("Model Summary",
                     br(),
                     verbatimTextOutput(ns("model_summary"))
            )
          )
      )
    )
  )
}

classificationServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    
    # ---- Update variable selectors when data changes ----
    observe({
      req(rv$working_data)
      df <- rv$working_data
      updateSelectInput(session, "target",   choices = names(df))
      updateSelectInput(session, "features", choices = names(df), selected = names(df))
    })
    
    # ---- Dynamic hyperparameter UI ----
    output$hyperparams <- renderUI({
      ns <- session$ns
      algo <- input$algorithm
      if (algo == "rf") {
        tagList(
          sliderInput(ns("rf_trees"), "Number of Trees", 50, 500, 100, step = 50),
          sliderInput(ns("rf_mtry"),  "mtry", 1, 10, 3)
        )
      } else if (algo == "tree") {
        tagList(
          sliderInput(ns("tree_depth"), "Max Depth", 1, 20, 5),
          sliderInput(ns("tree_cp"),    "Complexity (cp)", 0, 0.1, 0.01, step = 0.001)
        )
      } else if (algo == "svm") {
        tagList(
          selectInput(ns("svm_kernel"), "Kernel",
                      choices = c("radial", "linear", "polynomial")),
          sliderInput(ns("svm_cost"), "Cost", 0.1, 10, 1, step = 0.1)
        )
      } else if (algo == "knn") {
        sliderInput(ns("knn_k"), "K (neighbors)", 1, 20, 5)
      } else if (algo == "gbm") {
        tagList(
          sliderInput(ns("gbm_trees"),  "Trees",     50, 500, 100, step = 50),
          sliderInput(ns("gbm_depth"),  "Depth",      1,  10,   3),
          sliderInput(ns("gbm_shrink"), "Shrinkage", 0.01, 0.3, 0.1, step = 0.01)
        )
      } else if (algo == "nnet") {
        tagList(
          sliderInput(ns("nnet_size"),  "Hidden Units", 1, 20, 5),
          sliderInput(ns("nnet_decay"), "Decay", 0, 0.1, 0.01, step = 0.005)
        )
      } else {
        p("No hyperparameters for this algorithm.")
      }
    })
    
    # ---- Train model ----
    model_result <- eventReactive(input$run, {
      req(rv$working_data, input$target, input$features)
      
      df     <- rv$working_data
      target <- input$target
      feats  <- setdiff(input$features, target)
      req(length(feats) > 0)
      
      df[[target]] <- as.factor(df[[target]])
      df <- df[, c(feats, target), drop = FALSE]
      df <- na.omit(df)
      
      set.seed(42)
      n     <- nrow(df)
      idx   <- sample(seq_len(n), size = floor(input$train_ratio * n))
      train <- df[idx, ]
      test  <- df[-idx, ]
      
      formula <- as.formula(paste(target, "~ ."))
      algo    <- input$algorithm
      lvls    <- levels(df[[target]])
      
      # ---- Fit model ----
      model <- tryCatch({
        if (algo == "logistic") {
          glm(formula, data = train, family = binomial())
          
        } else if (algo == "tree") {
          rpart::rpart(formula, data = train, method = "class",
                       control = rpart::rpart.control(
                         maxdepth = isolate(input$tree_depth) %||% 5,
                         cp       = isolate(input$tree_cp)    %||% 0.01
                       )
          )
          
        } else if (algo == "rf") {
          randomForest::randomForest(formula, data = train,
                                     ntree = isolate(input$rf_trees) %||% 100,
                                     mtry  = min(isolate(input$rf_mtry) %||% 3, length(feats))
          )
          
        } else if (algo == "svm") {
          e1071::svm(formula, data = train, probability = TRUE,
                     kernel = isolate(input$svm_kernel) %||% "radial",
                     cost   = isolate(input$svm_cost)   %||% 1
          )
          
        } else if (algo == "knn") {
          # store info for prediction step
          list(
            type   = "knn",
            train  = train,
            k      = isolate(input$knn_k) %||% 5,
            target = target,
            feats  = feats
          )
          
        } else if (algo == "nb") {
          e1071::naiveBayes(formula, data = train)
          
        } else if (algo == "gbm") {
          # GBM requires numeric target: 0/1 for binary, 0..K-1 for multiclass
          train_gbm           <- train
          train_gbm[[target]] <- as.integer(train_gbm[[target]]) - 1L
          dist <- if (length(lvls) == 2) "bernoulli" else "multinomial"
          gbm::gbm(
            as.formula(paste(target, "~ .")),
            data              = train_gbm,
            distribution      = dist,
            n.trees           = isolate(input$gbm_trees)  %||% 100,
            interaction.depth = isolate(input$gbm_depth)  %||% 3,
            shrinkage         = isolate(input$gbm_shrink) %||% 0.1,
            verbose           = FALSE
          )
          
        } else if (algo == "nnet") {
          nnet::nnet(formula, data = train,
                     size  = isolate(input$nnet_size)  %||% 5,
                     decay = isolate(input$nnet_decay) %||% 0.01,
                     maxit = 200,
                     trace = FALSE
          )
        }
      }, error = function(e) stop(paste("Model training error:", e$message)))
      
      # ---- Predict ----
      preds <- tryCatch({
        if (algo == "knn") {
          class::knn(
            train = model$train[, model$feats],
            test  = test[, model$feats],
            cl    = model$train[[model$target]],
            k     = model$k
          )
          
        } else if (algo == "logistic") {
          prob <- predict(model, newdata = test, type = "response")
          factor(ifelse(prob > 0.5, lvls[2], lvls[1]), levels = lvls)
          
        } else if (algo == "gbm") {
          n_trees <- isolate(input$gbm_trees) %||% 100
          if (length(lvls) == 2) {
            prob <- gbm::predict.gbm(model, newdata = test,
                                     n.trees = n_trees, type = "response")
            factor(ifelse(prob > 0.5, lvls[2], lvls[1]), levels = lvls)
          } else {
            prob <- gbm::predict.gbm(model, newdata = test,
                                     n.trees = n_trees, type = "response")
            factor(lvls[apply(prob[,,1], 1, which.max)], levels = lvls)
          }
          
        } else if (algo == "tree") {
          p <- predict(model, newdata = test, type = "class")
          factor(p, levels = lvls)
          
        } else if (algo == "nnet") {
          p <- predict(model, newdata = test, type = "class")
          factor(p, levels = lvls)
          
        } else {
          p <- predict(model, newdata = test)
          if (!is.factor(p)) factor(p, levels = lvls) else p
        }
      }, error = function(e) stop(paste("Prediction error:", e$message)))
      
      # ---- Metrics ----
      actual <- test[[target]]
      preds  <- factor(preds, levels = lvls)
      actual <- factor(actual, levels = lvls)
      cm     <- table(Predicted = preds, Actual = actual)
      acc    <- sum(diag(cm)) / sum(cm)
      
      if (length(lvls) == 2) {
        tp   <- cm[2, 2]; fp <- cm[2, 1]; fn <- cm[1, 2]
        prec <- if ((tp + fp) > 0) tp / (tp + fp) else NA
        rec  <- if ((tp + fn) > 0) tp / (tp + fn) else NA
      } else {
        prec <- mean(diag(cm) / rowSums(cm), na.rm = TRUE)
        rec  <- mean(diag(cm) / colSums(cm), na.rm = TRUE)
      }
      f1 <- if (!is.na(prec) && !is.na(rec) && (prec + rec) > 0)
        2 * prec * rec / (prec + rec) else NA
      
      list(
        model  = model,
        algo   = algo,
        cm     = cm,
        acc    = acc,
        prec   = prec,
        rec    = rec,
        f1     = f1,
        preds  = preds,
        actual = actual,
        test   = test,
        target = target,
        feats  = feats,
        lvls   = lvls
      )
    })
    
    # ---- Value boxes ----
    output$accuracy_box <- renderValueBox({
      req(model_result())
      valueBox(paste0(round(model_result()$acc  * 100, 1), "%"),
               "Accuracy",  icon = icon("check"),    color = "green")
    })
    output$precision_box <- renderValueBox({
      req(model_result())
      valueBox(paste0(round(model_result()$prec * 100, 1), "%"),
               "Precision", icon = icon("bullseye"), color = "blue")
    })
    output$recall_box <- renderValueBox({
      req(model_result())
      valueBox(paste0(round(model_result()$rec  * 100, 1), "%"),
               "Recall",    icon = icon("search"),   color = "yellow")
    })
    output$f1_box <- renderValueBox({
      req(model_result())
      valueBox(paste0(round(model_result()$f1   * 100, 1), "%"),
               "F1 Score",  icon = icon("star"),     color = "purple")
    })
    
    # ---- Confusion matrix plot ----
    output$conf_matrix <- renderPlotly({
      req(model_result())
      cm_df <- as.data.frame(model_result()$cm)
      p <- ggplot(cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
        geom_tile(color = "white") +
        geom_text(aes(label = Freq), size = 5, fontface = "bold") +
        scale_fill_gradient(low = "white", high = "#3c8dbc") +
        theme_minimal() +
        labs(title = "Confusion Matrix")
      ggplotly(p)
    })
    
    # ---- ROC curve ----
    output$roc_curve <- renderPlotly({
      req(model_result())
      res  <- model_result()
      algo <- res$algo
      
      tryCatch({
        if (length(res$lvls) != 2) return(plotly::plotly_empty())
        
        prob <- NULL
        if (algo == "logistic") {
          prob <- predict(res$model, newdata = res$test, type = "response")
        } else if (algo == "rf") {
          prob <- randomForest::predict.randomForest(
            res$model, newdata = res$test, type = "prob")[, 2]
        } else if (algo == "nb") {
          prob <- predict(res$model, newdata = res$test, type = "raw")[, 2]
        } else if (algo == "gbm") {
          prob <- gbm::predict.gbm(res$model, newdata = res$test,
                                   n.trees = isolate(input$gbm_trees) %||% 100, type = "response")
        } else if (algo == "svm") {
          pp   <- predict(res$model, newdata = res$test, probability = TRUE)
          prob <- attr(pp, "probabilities")[, 2]
        } else if (algo == "nnet") {
          prob <- predict(res$model, newdata = res$test, type = "raw")[, 1]
        }
        
        if (is.null(prob)) return(plotly::plotly_empty())
        
        roc_obj <- pROC::roc(res$actual, as.numeric(prob), quiet = TRUE)
        roc_df  <- data.frame(
          FPR = 1 - roc_obj$specificities,
          TPR = roc_obj$sensitivities
        )
        p <- ggplot(roc_df, aes(x = FPR, y = TPR)) +
          geom_line(color = "#3c8dbc", linewidth = 1) +
          geom_abline(linetype = "dashed", color = "gray50") +
          labs(
            title = paste("AUC =", round(pROC::auc(roc_obj), 3)),
            x = "False Positive Rate",
            y = "True Positive Rate"
          ) +
          theme_minimal()
        ggplotly(p)
      }, error = function(e) plotly::plotly_empty())
    })
    
    # ---- Variable importance ----
    output$var_imp <- renderPlotly({
      req(model_result())
      res  <- model_result()
      algo <- res$algo
      
      tryCatch({
        imp <- NULL
        
        if (algo == "rf") {
          vi  <- randomForest::importance(res$model)
          imp <- data.frame(
            Variable   = rownames(vi),
            Importance = vi[, 1]
          )
        } else if (algo == "tree") {
          vi <- res$model$variable.importance
          if (!is.null(vi) && length(vi) > 0)
            imp <- data.frame(Variable = names(vi), Importance = as.numeric(vi))
        } else if (algo == "gbm") {
          vi  <- gbm::summary.gbm(res$model, plotit = FALSE)
          imp <- data.frame(Variable = vi$var, Importance = vi$rel.inf)
        } else if (algo == "logistic") {
          co  <- abs(coef(res$model))
          co  <- co[names(co) != "(Intercept)"]
          imp <- data.frame(Variable = names(co), Importance = as.numeric(co))
        } else if (algo == "nnet") {
          vi  <- caret::varImp(res$model)
          imp <- data.frame(Variable = rownames(vi), Importance = vi$Overall)
        }
        # svm, knn, nb: no importance -> show note instead
        
        if (!is.null(imp) && nrow(imp) > 0) {
          imp <- imp[order(imp$Importance, decreasing = TRUE), ]
          p <- ggplot(imp, aes(x = reorder(Variable, Importance), y = Importance)) +
            geom_col(fill = "#3c8dbc") +
            coord_flip() +
            theme_minimal() +
            labs(x = NULL, y = "Importance", title = "Variable Importance")
          ggplotly(p)
        } else {
          plotly::plotly_empty()
        }
      }, error = function(e) plotly::plotly_empty())
    })
    
    output$var_imp_note <- renderUI({
      req(model_result())
      if (model_result()$algo %in% c("svm", "knn", "nb")) {
        tags$p(
          style = "color:#888; font-style:italic; text-align:center;",
          "Variable importance is not available for SVM, KNN, and Naive Bayes."
        )
      }
    })
    
    # ---- Predictions table ----
    output$predictions <- renderDT({
      req(model_result())
      res <- model_result()
      df  <- data.frame(
        Actual    = res$actual,
        Predicted = res$preds,
        Correct   = res$actual == res$preds
      )
      datatable(df, options = list(pageLength = 10), rownames = FALSE)
    })
    
    # ---- Model summary ----
    output$model_summary <- renderPrint({
      req(model_result())
      res <- model_result()
      cat("Algorithm :", res$algo, "\n")
      cat("Accuracy  :", round(res$acc  * 100, 2), "%\n")
      cat("Precision :", round(res$prec * 100, 2), "%\n")
      cat("Recall    :", round(res$rec  * 100, 2), "%\n")
      cat("F1 Score  :", round(res$f1   * 100, 2), "%\n\n")
      cat("Confusion Matrix:\n")
      print(res$cm)
      cat("\nModel Details:\n")
      if (res$algo == "knn") {
        cat("KNN model - k =", res$model$k, "\n")
      } else {
        print(summary(res$model))
      }
    })
    
  })
}
