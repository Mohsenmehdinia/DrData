
# Regression Module


regressionUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(title = "Model Settings", width = 3, status = "primary", solidHeader = TRUE,
          
          h5("Variable Setup"),
          selectInput(ns("target"), "Target Variable", choices = NULL),
          selectInput(ns("features"), "Feature Variables", choices = NULL, multiple = TRUE),
          
          hr(),
          h5("Factor Conversion"),
          selectInput(ns("factor_vars"), "Convert to Factor", choices = NULL, multiple = TRUE),
          uiOutput(ns("base_level_ui")),
          actionButton(ns("apply_factors"), "Apply Factor Conversion",
                       class = "btn-warning", width = "100%"),
          
          hr(),
          h5("Algorithm"),
          selectInput(ns("algorithm"), "Algorithm",
                      choices = c(
                        "Linear Regression" = "linear",
                        "Ridge Regression"  = "ridge",
                        "Lasso Regression"  = "lasso",
                        "Decision Tree"     = "tree",
                        "Random Forest"     = "rf",
                        "SVM Regression"    = "svm",
                        "Gradient Boosting" = "gbm",
                        "Neural Network"    = "nnet"
                      )
          ),
          sliderInput(ns("train_ratio"), "Train Ratio", min = 0.5, max = 0.9, value = 0.8, step = 0.05),
          
          hr(),
          h5("Hyperparameters"),
          uiOutput(ns("hyperparams")),
          
          hr(),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'linear'", ns("algorithm")),
            checkboxInput(ns("use_interaction"), "Include Interaction Terms", value = FALSE),
            conditionalPanel(
              condition = sprintf("input['%s'] == true", ns("use_interaction")),
              selectInput(ns("int_vars_model"), "Select Variables for Interaction",
                          choices = NULL, multiple = TRUE),
              helpText("All pairwise interactions between selected variables will be included.")
            ),
            hr()
          ),
          
          actionButton(ns("run"), "Train Model", class = "btn-success", width = "100%")
      ),
      
      box(title = "Model Performance", width = 9, status = "info", solidHeader = TRUE,
          tabsetPanel(
            tabPanel("Metrics",
                     br(),
                     fluidRow(
                       valueBoxOutput(ns("rmse_box"),   width = 3),
                       valueBoxOutput(ns("mae_box"),    width = 3),
                       valueBoxOutput(ns("r2_box"),     width = 3),
                       valueBoxOutput(ns("adj_r2_box"), width = 3)
                     ),
                     fluidRow(
                       valueBoxOutput(ns("mape_box"), width = 3),
                       valueBoxOutput(ns("aic_box"),  width = 3),
                       valueBoxOutput(ns("bic_box"),  width = 3)
                     ),
                     hr(),
                     fluidRow(
                       box(title = "Actual vs Predicted", width = 6,
                           plotlyOutput(ns("actual_vs_pred"), height = "300px")),
                       box(title = "Residuals Plot", width = 6,
                           plotlyOutput(ns("residuals_plot"), height = "300px"))
                     )
            ),
            tabPanel("Variable Importance",
                     br(),
                     plotlyOutput(ns("var_imp"), height = "400px")
            ),
            tabPanel("Residual Diagnostics",
                     br(),
                     fluidRow(
                       box(title = "Residual Distribution", width = 6,
                           plotlyOutput(ns("resid_hist"), height = "280px")),
                       box(title = "Q-Q Plot", width = 6,
                           plotlyOutput(ns("qq_plot"), height = "280px"))
                     )
            ),
            tabPanel("Advanced Diagnostics",
                     br(),
                     conditionalPanel(
                       condition = sprintf("input['%s'] == 'linear'", ns("algorithm")),
                       h4("Variance Inflation Factor (VIF)"),
                       p("VIF > 10: severe multicollinearity | VIF 5-10: moderate | VIF < 5: OK"),
                       DTOutput(ns("vif_table")),
                       hr(),
                       h4("Heteroscedasticity: Breusch-Pagan Test"),
                       verbatimTextOutput(ns("bp_test")),
                       hr(),
                       h4("Autocorrelation: Durbin-Watson Test"),
                       verbatimTextOutput(ns("dw_test")),
                       hr(),
                       h4("Scale-Location Plot"),
                       plotlyOutput(ns("scale_loc_plot"), height = "280px")
                     ),
                     conditionalPanel(
                       condition = sprintf("input['%s'] != 'linear'", ns("algorithm")),
                       br(),
                       p("Advanced Diagnostics is only available for Linear Regression.")
                     )
            ),
         
            tabPanel("Interaction Effects",
                     br(),
                     conditionalPanel(
                       condition = sprintf("input['%s'] == 'linear'", ns("algorithm")),
                       
                       # ── بخش ۱: جدول interaction terms ──
                       h4("Detected Interaction Terms"),
                       p("Interaction terms found in the fitted model with significance indicators."),
                       DTOutput(ns("int_terms_table")),
                       
                       hr(),
                       
                       # ── بخش ۲: مقایسه مدل ──
                       h4("Model Comparison: Base vs Full"),
                       p("ANOVA-based comparison between main-effects-only model and full model with interactions."),
                       DTOutput(ns("model_comparison_table")),
                       
                       hr(),
                       
                       # ── بخش ۳: Simple Slopes ──
                       h4("Simple Slopes Analysis"),
                       uiOutput(ns("simple_slopes_ui")),
                       DTOutput(ns("simple_slopes_table")),
                       
                       hr(),
                       
                       # ── بخش ۴: Marginal Effects ──
                       h4("Marginal Effects Plot"),
                       uiOutput(ns("marginal_effects_ui")),
                       plotlyOutput(ns("marginal_effects_plot"), height = "350px"),
                       
                       hr(),
                       
                       # ── بخش ۵: Interaction Plots ──
                       h4("Interaction Plots"),
                       fluidRow(
                         column(4,
                                selectInput(ns("int_plot_vars"), "Select Variables (2+)",
                                            choices = NULL, multiple = TRUE),
                                helpText("Select at least 2 variables. All pairwise interactions will be plotted."),
                                actionButton(ns("plot_interaction"), "Plot Interactions", class = "btn-info")
                         ),
                         column(8,
                                uiOutput(ns("interaction_plots_ui"))
                         )
                       ),
                       
                       hr(),
                       
                       # ── بخش ۶: Auto Interpretation ──
                       h4("Automatic Interpretation"),
                       verbatimTextOutput(ns("interaction_interpretation"))
                     ),
                     conditionalPanel(
                       condition = sprintf("input['%s'] != 'linear'", ns("algorithm")),
                       br(),
                       p("Interaction Effects is only available for Linear Regression.")
                     )
            )
            
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
  
}


regressionServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    
    # داده‌ای که factor conversion روش اعمال شده
    reg_data <- reactiveVal(NULL)
    
    observe({
      req(rv$working_data)
      reg_data(rv$working_data)
    })
    
    observe({
      req(reg_data())
      df       <- reg_data()
      num_cols <- names(df)[vapply(df, is.numeric, logical(1L))]
      all_cols <- names(df)
      updateSelectInput(session, "target",          choices = num_cols)
      updateSelectInput(session, "features",        choices = all_cols)
      updateSelectInput(session, "factor_vars",     choices = all_cols, selected = character(0))
      updateSelectInput(session, "int_vars_model",  choices = num_cols, selected = character(0))
      updateSelectInput(session, "int_plot_vars",   choices = num_cols, selected = character(0))
    })
    
    # UI داینامیک برای انتخاب base level هر فاکتور
    output$base_level_ui <- renderUI({
      ns <- session$ns
      req(input$factor_vars)
      df <- reg_data()
      lapply(input$factor_vars, function(v) {
        lvls <- sort(unique(na.omit(as.character(df[[v]]))))
        selectInput(
          ns(paste0("base_", v)),
          paste("Base level for:", v),
          choices  = lvls,
          selected = lvls[1]
        )
      })
    })
    
    # اعمال تبدیل فاکتور با base level مشخص
    observeEvent(input$apply_factors, {
      req(reg_data(), input$factor_vars)
      df <- reg_data()
      
      for (v in input$factor_vars) {
        base_lvl <- input[[paste0("base_", v)]]
        lvls     <- sort(unique(na.omit(as.character(df[[v]]))))
        if (!is.null(base_lvl) && base_lvl %in% lvls) {
          lvls <- c(base_lvl, setdiff(lvls, base_lvl))
        }
        df[[v]] <- factor(df[[v]], levels = lvls)
      }
      
      reg_data(df)
      showNotification(
        paste0("Converted to factor: ", paste(input$factor_vars, collapse = ", "),
               ". lm() will automatically create dummy variables with the selected base levels."),
        type = "message", duration = 6
      )
    })
    
    output$hyperparams <- renderUI({
      ns   <- session$ns
      algo <- input$algorithm
      if (algo %in% c("ridge", "lasso")) {
        sliderInput(ns("lambda"), "Lambda", 0.001, 10, 0.1, step = 0.01)
      } else if (algo == "tree") {
        tagList(
          sliderInput(ns("tree_depth"), "Max Depth", 1, 20, 5),
          sliderInput(ns("tree_cp"), "Complexity (cp)", 0, 0.1, 0.01, step = 0.001)
        )
      } else if (algo == "rf") {
        tagList(
          sliderInput(ns("rf_trees"), "Number of Trees", 50, 500, 100, step = 50),
          sliderInput(ns("rf_mtry"), "mtry", 1, 10, 3)
        )
      } else if (algo == "svm") {
        tagList(
          selectInput(ns("svm_kernel"), "Kernel", choices = c("radial","linear","polynomial")),
          sliderInput(ns("svm_cost"), "Cost", 0.1, 10, 1, step = 0.1),
          sliderInput(ns("svm_eps"), "Epsilon", 0.01, 1, 0.1, step = 0.01)
        )
      } else if (algo == "gbm") {
        tagList(
          sliderInput(ns("gbm_trees"), "Trees", 50, 500, 100, step = 50),
          sliderInput(ns("gbm_depth"), "Depth", 1, 10, 3),
          sliderInput(ns("gbm_shrink"), "Shrinkage", 0.01, 0.3, 0.1, step = 0.01)
        )
      } else if (algo == "nnet") {
        tagList(
          sliderInput(ns("nnet_size"), "Hidden Units", 1, 20, 5),
          sliderInput(ns("nnet_decay"), "Decay", 0, 0.1, 0.01, step = 0.005)
        )
      } else {
        p("No hyperparameters for this algorithm.")
      }
    })
    
    model_result <- eventReactive(input$run, {
      req(reg_data(), input$target, input$features)
      df     <- reg_data()
      target <- input$target
      feats  <- setdiff(input$features, target)
      req(length(feats) > 0)
      
      df <- df[, c(feats, target), drop = FALSE]
      df <- na.omit(df)
      
      # ساخت فرمول با interaction terms
      base_formula_str <- paste(target, "~ .")
      formula_str <- if (
        isTRUE(input$use_interaction) &&
        input$algorithm == "linear" &&
        length(input$int_vars_model) >= 2
      ) {
        valid_vars <- intersect(input$int_vars_model, feats)
        if (length(valid_vars) >= 2) {
          # همه pairwise interactions
          pairs <- combn(valid_vars, 2, simplify = FALSE)
          int_terms <- paste(vapply(pairs, function(p) paste(p, collapse = " * "), character(1L)), collapse = " + ")
          paste(target, "~ . +", int_terms)
        } else {
          base_formula_str
        }
      } else {
        base_formula_str
      }
      
      formula <- as.formula(formula_str)
      
      set.seed(42)
      n     <- nrow(df)
      idx   <- sample(seq_len(n), size = floor(input$train_ratio * n))
      train <- df[idx, ]
      test  <- df[-idx, ]
      algo  <- input$algorithm
      
      model <- tryCatch({
        if (algo == "linear") {
          lm(formula, data = train)
        } else if (algo %in% c("ridge", "lasso")) {
          alpha_val <- if (algo == "lasso") 1 else 0
          x_train   <- model.matrix(as.formula(base_formula_str), train)[, -1]
          y_train   <- train[[target]]
          glmnet(x_train, y_train, alpha = alpha_val,
                 lambda = isolate(input$lambda) %||% 0.1)
        } else if (algo == "tree") {
          rpart(formula, data = train, method = "anova",
                control = rpart.control(
                  maxdepth = isolate(input$tree_depth) %||% 5,
                  cp       = isolate(input$tree_cp) %||% 0.01
                ))
        } else if (algo == "rf") {
          randomForest(formula, data = train,
                       ntree = isolate(input$rf_trees) %||% 100,
                       mtry  = isolate(input$rf_mtry) %||% 3)
        } else if (algo == "svm") {
          svm(formula, data = train,
              kernel  = isolate(input$svm_kernel) %||% "radial",
              cost    = isolate(input$svm_cost) %||% 1,
              epsilon = isolate(input$svm_eps) %||% 0.1)
        } else if (algo == "gbm") {
          gbm(formula, data = train, distribution = "gaussian",
              n.trees           = isolate(input$gbm_trees) %||% 100,
              interaction.depth = isolate(input$gbm_depth) %||% 3,
              shrinkage         = isolate(input$gbm_shrink) %||% 0.1,
              verbose = FALSE)
        } else if (algo == "nnet") {
          nnet(formula, data = train,
               size   = isolate(input$nnet_size) %||% 5,
               decay  = isolate(input$nnet_decay) %||% 0.01,
               linout = TRUE, maxit = 200, trace = FALSE)
        }
      }, error = function(e) stop(paste("Model error:", e$message)))
      
      preds <- tryCatch({
        if (algo %in% c("ridge", "lasso")) {
          x_test <- model.matrix(as.formula(base_formula_str), test)[, -1]
          as.numeric(predict(model, newx = x_test,
                             s = isolate(input$lambda) %||% 0.1))
        } else if (algo == "gbm") {
          predict(model, newdata = test,
                  n.trees = isolate(input$gbm_trees) %||% 100)
        } else {
          as.numeric(predict(model, newdata = test))
        }
      }, error = function(e) stop(paste("Prediction error:", e$message)))
      
      actual    <- test[[target]]
      residuals <- actual - preds
      n_test    <- length(actual)
      n_feats   <- length(feats)
      rmse      <- sqrt(mean(residuals^2))
      mae       <- mean(abs(residuals))
      ss_res    <- sum(residuals^2)
      ss_tot    <- sum((actual - mean(actual))^2)
      r2        <- if (ss_tot == 0) NA_real_ else 1 - ss_res / ss_tot

      # Adj-R² is a training metric (penalises extra features on seen data).
      # We compute it from the training residuals to avoid misleading values.
      if (algo == "linear" && !is.null(model)) {
        n_train  <- nrow(train)
        tr_preds <- as.numeric(stats::predict(model, train))
        tr_res   <- train[[target]] - tr_preds
        ss_res_tr <- sum(tr_res^2)
        ss_tot_tr <- sum((train[[target]] - mean(train[[target]]))^2)
        r2_tr    <- if (ss_tot_tr == 0) NA_real_ else 1 - ss_res_tr / ss_tot_tr
        adj_r2   <- if (!is.na(r2_tr))
          1 - (1 - r2_tr) * (n_train - 1) / (n_train - n_feats - 1)
        else NA_real_
      } else {
        adj_r2 <- NA_real_
      }

      # MAPE: guard against zero actual values to avoid Inf
      nonzero  <- actual != 0 & !is.na(actual)
      mape     <- if (sum(nonzero) > 0)
        mean(abs(residuals[nonzero] / actual[nonzero]) * 100)
      else NA_real_

      aic_val   <- if (algo == "linear") AIC(model) else NA
      bic_val   <- if (algo == "linear") BIC(model) else NA
      
      list(
        model = model, algo = algo, formula = formula,
        preds = preds, actual = actual, residuals = residuals,
        rmse = rmse, mae = mae, r2 = r2, adj_r2 = adj_r2,
        mape = mape, aic = aic_val, bic = bic_val,
        test = test, train = train, target = target, feats = feats
      )
    })
    
    # --- Value Boxes ---
    output$rmse_box <- renderValueBox({
      req(model_result())
      valueBox(round(model_result()$rmse, 4), "RMSE", icon = icon("ruler"), color = "red")
    })
    output$mae_box <- renderValueBox({
      req(model_result())
      valueBox(round(model_result()$mae, 4), "MAE", icon = icon("minus"), color = "yellow")
    })
    output$r2_box <- renderValueBox({
      req(model_result())
      valueBox(round(model_result()$r2, 4), "R²", icon = icon("chart-line"), color = "green")
    })
    output$adj_r2_box <- renderValueBox({
      req(model_result())
      valueBox(round(model_result()$adj_r2, 4), "Adj. R²", icon = icon("chart-bar"), color = "olive")
    })
    output$mape_box <- renderValueBox({
      req(model_result())
      valueBox(paste0(round(model_result()$mape, 2), "%"), "MAPE", icon = icon("percent"), color = "blue")
    })
    output$aic_box <- renderValueBox({
      req(model_result())
      val <- if (!is.na(model_result()$aic)) round(model_result()$aic, 2) else "N/A"
      valueBox(val, "AIC", icon = icon("info-circle"), color = "purple")
    })
    output$bic_box <- renderValueBox({
      req(model_result())
      val <- if (!is.na(model_result()$bic)) round(model_result()$bic, 2) else "N/A"
      valueBox(val, "BIC", icon = icon("info-circle"), color = "maroon")
    })
    
    # --- Plots ---
    output$actual_vs_pred <- renderPlotly({
      req(model_result())
      res <- model_result()
      df2 <- data.frame(Actual = res$actual, Predicted = res$preds)
      p <- ggplot(df2, aes(x = Actual, y = Predicted)) +
        geom_point(alpha = 0.6, color = "#3c8dbc") +
        geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
        theme_minimal() + labs(title = "Actual vs Predicted")
      ggplotly(p)
    })
    
    output$residuals_plot <- renderPlotly({
      req(model_result())
      res <- model_result()
      df2 <- data.frame(Predicted = res$preds, Residuals = res$residuals)
      p <- ggplot(df2, aes(x = Predicted, y = Residuals)) +
        geom_point(alpha = 0.6, color = "#f39c12") +
        geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
        theme_minimal() + labs(title = "Residuals vs Fitted")
      ggplotly(p)
    })
    
    output$var_imp <- renderPlotly({
      req(model_result())
      res  <- model_result()
      algo <- res$algo
      imp  <- tryCatch({
        if (algo == "linear") {
          cf <- summary(res$model)$coefficients[-1, , drop = FALSE]
          data.frame(Variable = rownames(cf), Importance = abs(cf[, "t value"]))
        } else if (algo %in% c("ridge", "lasso")) {
          cf <- as.matrix(coef(res$model))[-1, , drop = FALSE]
          data.frame(Variable = rownames(cf), Importance = abs(cf[, 1]))
        } else if (algo == "tree") {
          vi <- res$model$variable.importance
          data.frame(Variable = names(vi), Importance = as.numeric(vi))
        } else if (algo == "rf") {
          vi <- importance(res$model)
          data.frame(Variable = rownames(vi), Importance = vi[, 1])
        } else if (algo == "gbm") {
          vi <- summary(res$model, plotit = FALSE)
          data.frame(Variable = vi$var, Importance = vi$rel.inf)
        } else { NULL }
      }, error = function(e) NULL)
      
      if (is.null(imp)) {
        plotly_empty() %>% layout(title = "Not available for this algorithm")
      } else {
        imp <- imp[order(imp$Importance, decreasing = TRUE), ]
        p <- ggplot(imp, aes(x = reorder(Variable, Importance), y = Importance)) +
          geom_bar(stat = "identity", fill = "#3c8dbc") +
          coord_flip() + theme_minimal() +
          labs(title = "Variable Importance", x = "Variable", y = "Importance")
        ggplotly(p)
      }
    })
    
    output$resid_hist <- renderPlotly({
      req(model_result())
      df2 <- data.frame(Residuals = model_result()$residuals)
      p <- ggplot(df2, aes(x = Residuals)) +
        geom_histogram(fill = "#3c8dbc", color = "white", bins = 30) +
        theme_minimal() + labs(title = "Residual Distribution")
      ggplotly(p)
    })
    
    output$qq_plot <- renderPlotly({
      req(model_result())
      res <- model_result()$residuals
      qq  <- qqnorm(res, plot.it = FALSE)
      df2 <- data.frame(Theoretical = qq$x, Sample = qq$y)
      p <- ggplot(df2, aes(x = Theoretical, y = Sample)) +
        geom_point(alpha = 0.6, color = "#3c8dbc") +
        geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
        theme_minimal() + labs(title = "Q-Q Plot")
      ggplotly(p)
    })
    
    # --- Advanced Diagnostics ---
    output$vif_table <- renderDT({
      req(model_result())
      res <- model_result()
      req(res$algo == "linear")
      vif_vals <- tryCatch({
        if (length(res$feats) < 2) stop("At least 2 variables are required for VIF.")
        v <- car::vif(res$model)
        if (is.matrix(v)) v <- v[, 1]
        data.frame(
          Variable = names(v),
          VIF      = round(as.numeric(v), 3),
          Status   = ifelse(as.numeric(v) > 10, "High (>10)",
                            ifelse(as.numeric(v) > 5, "Moderate (5-10)", "OK (<5)"))
        )
      }, error = function(e) data.frame(Note = e$message))
      datatable(vif_vals, options = list(pageLength = 15), rownames = FALSE)
    })
    
    output$bp_test <- renderPrint({
      req(model_result())
      res <- model_result()
      req(res$algo == "linear")
      tryCatch({
        cat("Breusch-Pagan Test for Heteroscedasticity:\n")
        print(lmtest::bptest(res$model))
        cat("\nH0: Error variance is constant (homoscedasticity)\n")
        cat("p < 0.05 => heteroscedasticity is present\n")
      }, error = function(e) cat("Error:", e$message))
    })
    
    output$dw_test <- renderPrint({
      req(model_result())
      res <- model_result()
      req(res$algo == "linear")
      tryCatch({
        cat("Durbin-Watson Test for Autocorrelation:\n")
        print(lmtest::dwtest(res$model))
        cat("\nDW close to 2: no autocorrelation\n")
        cat("DW < 1.5: positive autocorrelation | DW > 2.5: negative autocorrelation\n")
      }, error = function(e) cat("Error:", e$message))
    })
    
    output$scale_loc_plot <- renderPlotly({
      req(model_result())
      res <- model_result()
      req(res$algo == "linear")
      fitted_vals <- fitted(res$model)
      std_resid   <- sqrt(abs(rstandard(res$model)))
      df2 <- data.frame(Fitted = fitted_vals, SqrtStdResid = std_resid)
      p <- ggplot(df2, aes(x = Fitted, y = SqrtStdResid)) +
        geom_point(alpha = 0.6, color = "#9b59b6") +
        geom_smooth(method = "loess", se = FALSE, color = "red") +
        theme_minimal() +
        labs(title = "Scale-Location Plot",
             x = "Fitted Values", y = "√|Standardized Residuals|")
      ggplotly(p)
    })
    
    # --- Interaction Effects (همه pairwise) ---
    observeEvent(input$plot_interaction, {
      req(model_result(), input$int_plot_vars)
      res  <- model_result()
      req(res$algo == "linear", length(input$int_plot_vars) >= 2)
      
      vars  <- intersect(input$int_plot_vars, names(res$train))
      pairs <- combn(vars, 2, simplify = FALSE)
      
      # رندر داینامیک چند plot
      output$interaction_plots_ui <- renderUI({
        ns <- session$ns
        plot_outputs <- lapply(seq_along(pairs), function(i) {
          plotlyOutput(ns(paste0("int_plot_", i)), height = "320px")
        })
        do.call(tagList, plot_outputs)
      })
      
      lapply(seq_along(pairs), function(i) {
        local({
          pair  <- pairs[[i]]
          v1    <- pair[1]
          v2    <- pair[2]
          pid   <- paste0("int_plot_", i)
          
          output[[pid]] <- renderPlotly({
            tryCatch({
              df_train  <- res$train
              m2  <- mean(df_train[[v2]], na.rm = TRUE)
              sd2 <- sd(df_train[[v2]],   na.rm = TRUE)
              levels_v2 <- c(m2 - sd2, m2, m2 + sd2)
              labels_v2 <- c("Low (-1SD)", "Mean", "High (+1SD)")
              
              plot_df <- do.call(rbind, lapply(seq_along(levels_v2), function(j) {
                tmp        <- df_train
                tmp[[v2]]  <- levels_v2[j]
                tmp$pred   <- predict(res$model, newdata = tmp)
                tmp$level  <- labels_v2[j]
                tmp[, c(v1, "pred", "level")]
              }))
              
              p <- ggplot(plot_df, aes(x = .data[[v1]], y = .data[["pred"]], color = .data[["level"]])) +
                geom_smooth(method = "lm", se = TRUE) +
                theme_minimal() +
                labs(
                  title = paste("Interaction:", v1, "×", v2),
                  x = v1, y = paste("Predicted", res$target),
                  color = v2
                )
              ggplotly(p)
            }, error = function(e) {
              plotly_empty() %>% layout(title = paste("Error:", e$message))
            })
          })
        })
      })
      
      output$interaction_summary <- renderPrint({
        tryCatch({
          valid_vars <- intersect(input$int_plot_vars, res$feats)
          req(length(valid_vars) >= 2)
          pairs_valid <- combn(valid_vars, 2, simplify = FALSE)
          int_terms   <- paste(vapply(pairs_valid, function(p) paste(p, collapse = " * "), character(1L)),
                               collapse = " + ")
          int_formula <- as.formula(paste(res$target, "~ . +", int_terms))
          int_model   <- lm(int_formula, data = res$train)
          cat("Interaction Terms:", int_terms, "\n\n")
          print(summary(int_model))
          cat("\nAIC:", AIC(int_model), "| BIC:", BIC(int_model), "\n")
        }, error = function(e) cat("Error:", e$message))
      })
    })
    # ── Interaction Effects: reactive outputs ────────────────────────────────────
    
    output$int_terms_table <- renderDT({
      req(model_result())
      res <- model_result()
      req(res$algo == "linear")
      int_df <- detect_interaction_terms(res$model)
      if (is.null(int_df) || nrow(int_df) == 0) {
        return(datatable(
          data.frame(Message = "No interaction terms in current model. Enable interactions in Model Settings."),
          options = list(dom = 't'), rownames = FALSE
        ))
      }
      display_df <- data.frame(
        Term         = int_df$interaction_term,
        Estimate     = round(int_df$estimate,  4),
        `Std. Error` = round(int_df$std_error, 4),
        `t value`    = round(int_df$t_value,   3),
        `p value`    = round(int_df$p_value,   4),
        Sig          = int_df$significance,
        Order        = ifelse(int_df$order == 2, "Two-way", paste0(int_df$order, "-way")),
        check.names  = FALSE
      )
      datatable(display_df, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE) %>%
        formatStyle("p value", backgroundColor = styleInterval(0.05, c("#d5f5e3", "white")))
    })
    
    output$model_comparison_table <- renderDT({
      req(model_result())
      res  <- model_result()
      req(res$algo == "linear")
      base_m <- tryCatch(build_base_model(res$model, res$train), error = function(e) NULL)
      comp   <- tryCatch(compare_interaction_models(base_m, res$model), error = function(e) NULL)
      if (is.null(comp)) {
        return(datatable(
          data.frame(Message = "Model comparison not available (model may have no interaction terms)."),
          options = list(dom = 't'), rownames = FALSE
        ))
      }
      datatable(comp, options = list(dom = 't', scrollX = TRUE), rownames = FALSE) %>%
        formatStyle("Delta_R2", backgroundColor = styleInterval(0, c("white", "#d5f5e3")))
    })
    
    output$simple_slopes_ui <- renderUI({
      ns <- session$ns
      req(model_result())
      res   <- model_result()
      req(res$algo == "linear")
      int_df <- detect_interaction_terms(res$model)
      if (is.null(int_df) || nrow(int_df) == 0) return(p("No interaction terms detected."))
      two_way <- int_df[int_df$order == 2, ]
      if (nrow(two_way) == 0) return(p("No two-way interactions found."))
      fluidRow(
        column(4, selectInput(ns("ss_term"), "Select Interaction Term", choices = two_way$interaction_term)),
        column(4, uiOutput(ns("ss_predictor_ui"))),
        column(4, br(), actionButton(ns("run_simple_slopes"), "Compute Simple Slopes", class = "btn-primary"))
      )
    })
    
    output$ss_predictor_ui <- renderUI({
      ns    <- session$ns
      req(input$ss_term)
      parts <- strsplit(input$ss_term, ":")[[1]]
      selectInput(ns("ss_predictor"), "Predictor (X)", choices = parts, selected = parts[1])
    })
    
    output$simple_slopes_table <- renderDT({
      req(input$run_simple_slopes)
      isolate({
        req(model_result(), input$ss_term, input$ss_predictor)
        res       <- model_result()
        parts     <- strsplit(input$ss_term, ":")[[1]]
        predictor <- input$ss_predictor
        moderator <- setdiff(parts, predictor)
        if (length(moderator) == 0) {
          return(datatable(data.frame(Error = "Could not determine moderator."),
                           options = list(dom = 't'), rownames = FALSE))
        }
        moderator <- moderator[1]
        if (!is.numeric(res$train[[moderator]])) {
          return(datatable(
            data.frame(Note = "Simple slopes analysis requires a continuous moderator."),
            options = list(dom = 't'), rownames = FALSE
          ))
        }
        ss_df <- simple_slopes_analysis(res$model, predictor, moderator, res$train)
        datatable(ss_df, options = list(dom = 't', scrollX = TRUE), rownames = FALSE)
      })
    })
    
    output$marginal_effects_ui <- renderUI({
      ns <- session$ns
      req(model_result())
      res    <- model_result()
      req(res$algo == "linear")
      int_df <- detect_interaction_terms(res$model)
      if (is.null(int_df) || nrow(int_df) == 0) return(NULL)
      two_way <- int_df[int_df$order == 2, ]
      if (nrow(two_way) == 0) return(NULL)
      fluidRow(
        column(4, selectInput(ns("me_term"), "Interaction Term", choices = two_way$interaction_term)),
        column(4, uiOutput(ns("me_predictor_ui"))),
        column(4, br(), actionButton(ns("run_marginal"), "Compute Marginal Effects", class = "btn-warning"))
      )
    })
    
    output$me_predictor_ui <- renderUI({
      ns    <- session$ns
      req(input$me_term)
      parts <- strsplit(input$me_term, ":")[[1]]
      selectInput(ns("me_predictor"), "Predictor (dy/dx)", choices = parts, selected = parts[1])
    })
    
    output$marginal_effects_plot <- renderPlotly({
      req(input$run_marginal)
      isolate({
        req(model_result(), input$me_term, input$me_predictor)
        res       <- model_result()
        parts     <- strsplit(input$me_term, ":")[[1]]
        predictor <- input$me_predictor
        moderator <- setdiff(parts, predictor)[1]
        if (!is.numeric(res$train[[moderator]])) {
          return(plotly_empty() %>% layout(title = "Moderator must be continuous"))
        }
        me_df <- compute_marginal_effects(res$model, predictor, moderator, res$train)
        if ("Note" %in% names(me_df)) {
          return(plotly_empty() %>% layout(title = me_df$Note[1]))
        }
        p <- ggplot(me_df, aes(x = moderator_value, y = marginal_effect)) +
          geom_line(color = "#3498db", linewidth = 1.2) +
          geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2, fill = "#3498db") +
          geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
          theme_minimal(base_size = 13) +
          labs(
            title   = paste("Marginal Effect of", predictor, "as", moderator, "varies"),
            x       = paste("Value of", moderator),
            y       = paste0("dy/d(", predictor, ")"),
            caption = "Shaded area = 95% CI | Red dashed line = zero effect"
          )
        ggplotly(p)
      })
    })
    
    output$interaction_interpretation <- renderPrint({
      req(model_result())
      res    <- model_result()
      req(res$algo == "linear")
      int_df <- detect_interaction_terms(res$model)
      cat(interpret_interaction(int_df, res$train))
    })
    
    # --- Predictions & Summary ---
    output$predictions <- renderDT({
      req(model_result())
      res <- model_result()
      df2 <- data.frame(
        Actual    = round(res$actual, 4),
        Predicted = round(res$preds, 4),
        Residual  = round(res$residuals, 4)
      )
      datatable(df2, options = list(pageLength = 10, scrollX = TRUE))
    })
    
    output$model_summary <- renderPrint({
      req(model_result())
      res <- model_result()
      if (res$algo == "linear") {
        print(summary(res$model))
      } else if (res$algo %in% c("ridge", "lasso")) {
        print(res$model)
        cat("\nLambda used:", isolate(input$lambda) %||% 0.1, "\n")
      } else if (res$algo == "tree") {
        print(summary(res$model))
      } else if (res$algo == "rf") {
        print(res$model)
      } else if (res$algo == "gbm") {
        print(summary(res$model, plotit = FALSE))
      } else if (res$algo == "nnet") {
        print(res$model)
      } else {
        cat("Model summary not available for this algorithm.\n")
      }
    })
    
  }) # end moduleServer
} # end regressionServer
