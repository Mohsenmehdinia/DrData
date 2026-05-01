# ============================================================
# DrData — Model Comparison Module
# Compares all AutoML runs stored in rv$automl_results_list
# ============================================================

modelComparisonUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(title = "Comparison Settings", width = 3,
          status = "primary", solidHeader = TRUE,
          uiOutput(ns("run_selector")),
          hr(),
          actionButton(ns("refresh"), "Refresh",
                       class = "btn-success btn-block",
                       icon  = icon("sync"))
      ),
      box(title = "Model Comparison Dashboard", width = 9,
          status = "info", solidHeader = TRUE,
          tabsetPanel(
            tabPanel("Overview",
                     br(),
                     fluidRow(
                       valueBoxOutput(ns("best_box"),   width = 4),
                       valueBoxOutput(ns("score_box"),  width = 4),
                       valueBoxOutput(ns("count_box"),  width = 4)
                     ),
                     br(),
                     plotlyOutput(ns("bar_plot"), height = "350px"),
                     br(),
                     DT::dataTableOutput(ns("metrics_table"))
            ),
            tabPanel("Heatmap",
                     br(),
                     plotlyOutput(ns("heatmap_plot"), height = "450px")
            ),
            tabPanel("Model Details",
                     br(),
                     verbatimTextOutput(ns("model_detail"))
            )
          )
      )
    )
  )
}

# ============================================================
# SERVER
# ============================================================

modelComparisonServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Run selector ─────────────────────────────────────────────────────
    output$run_selector <- renderUI({
      req(rv$automl_results_list)
      run_names <- names(rv$automl_results_list)
      req(length(run_names) > 0)
      tagList(
        selectInput(ns("selected_run"), "Select AutoML Run",
                    choices = run_names, selected = run_names[1]),
        helpText(paste(length(run_names), "run(s) available"))
      )
    })

    # ── Active run data ───────────────────────────────────────────────────
    active_run <- reactive({
      input$refresh
      req(rv$automl_results_list, input$selected_run)
      rv$automl_results_list[[input$selected_run]]
    })

    combined_metrics <- reactive({
      req(active_run())
      active_run()$metrics
    })

    # ── Value boxes ──────────────────────────────────────────────────────
    output$best_box <- renderValueBox({
      req(combined_metrics())
      df   <- combined_metrics()
      task <- active_run()$task
      best <- if (task == "regression") df$Algorithm[which.min(df$Score)]
              else                       df$Algorithm[which.max(df$Score)]
      valueBox(best, "Best Model", color = "green", icon = icon("trophy"))
    })

    output$score_box <- renderValueBox({
      req(combined_metrics())
      df   <- combined_metrics()
      task <- active_run()$task
      val  <- if (task == "regression") min(df$Score) else max(df$Score)
      lbl  <- if (task == "regression") "Best RMSE" else "Best Accuracy"
      valueBox(round(val, 4), lbl, color = "purple",
               icon = icon("chart-line"))
    })

    output$count_box <- renderValueBox({
      req(combined_metrics())
      valueBox(nrow(combined_metrics()), "Models Compared",
               color = "blue", icon = icon("layer-group"))
    })

    # ── Bar plot ─────────────────────────────────────────────────────────
    output$bar_plot <- renderPlotly({
      req(combined_metrics(), active_run())
      df   <- combined_metrics()
      task <- active_run()$task
      best <- active_run()$best
      df$Highlight <- ifelse(df$Algorithm == best, "Best", "Other")

      if (task == "regression") df <- df[order(df$Score),]
      else                       df <- df[order(-df$Score),]
      df$Algorithm <- factor(df$Algorithm, levels = df$Algorithm)

      plot_ly(df, x = ~Algorithm, y = ~Score,
              type  = "bar",
              color = ~Highlight,
              colors= c("Best"="#27ae60","Other"="#3498db"),
              text  = ~round(Score,4), textposition="outside") %>%
        layout(showlegend = FALSE,
               yaxis = list(title = unique(df$Metric)),
               xaxis = list(title = ""))
    })

    # ── Metrics table ────────────────────────────────────────────────────
    output$metrics_table <- DT::renderDataTable({
      req(combined_metrics())
      DT::datatable(combined_metrics(),
                    options = list(pageLength = 10),
                    rownames = FALSE)
    })

    # ── Heatmap across all runs ──────────────────────────────────────────
    output$heatmap_plot <- renderPlotly({
      req(rv$automl_results_list)
      runs <- rv$automl_results_list
      req(length(runs) >= 1)

      all_algos <- unique(unlist(lapply(runs, function(r) r$metrics$Algorithm)))
      run_names <- names(runs)

      mat <- vapply(run_names, function(rn) {
        df <- runs[[rn]]$metrics
        vapply(all_algos, function(a) {
          idx <- which(df$Algorithm == a)
          if (length(idx) == 0) NA_real_ else df$Score[idx[1]]
        }, numeric(1L))
      }, numeric(length(all_algos)))

      plot_ly(
        x = run_names,
        y = all_algos,
        z = mat,
        type      = "heatmap",
        colorscale = "Viridis",
        text      = round(mat, 4),
        texttemplate = "%{text}"
      ) %>%
        layout(
          xaxis = list(title = "AutoML Run"),
          yaxis = list(title = "Algorithm"),
          title = "Performance Heatmap (all runs)"
        )
    })

    # ── Model detail ─────────────────────────────────────────────────────
    output$model_detail <- renderPrint({
      req(active_run())
      run  <- active_run()
      best <- run$best
      cat("Run:", input$selected_run, "\n")
      cat("Task:", run$task, "\n")
      cat("Best Algorithm:", best, "\n\n")
      model_obj <- run$results[[best]]$model
      print(summary(model_obj))
    })
  })
}
