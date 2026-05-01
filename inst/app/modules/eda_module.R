# ============================================================
# DrData — EDA Module (v3)
# CHANGES:
#   1. colourInput + palette selector for plot colors
#   2. Y Variable shown only when plot needs it
#   3. Box/Violin: single-variable mode (no grouping) + warning
#      when X is numeric in grouped mode
# ============================================================

edaUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(title = "EDA Options", width = 3,
          status = "primary", solidHeader = TRUE,

          # ── Plot type ───────────────────────────────────────────────
          selectInput(ns("plot_type"), "Plot Type",
            choices = c(
              "Histogram"    = "hist",
              "Boxplot"      = "box",
              "Scatter Plot" = "scatter",
              "Bar Chart"    = "bar",
              "Correlation"  = "corr",
              "Density Plot" = "density",
              "Violin Plot"  = "violin",
              "QQ Plot"      = "qq"
            )
          ),

          # ── Box/Violin: mode toggle ──────────────────────────────────
          # Single-variable = just pick Y, no grouping
          # Grouped         = X (categorical) groups Y
          conditionalPanel(
            condition = sprintf(
              "['box','violin'].indexOf(input['%s']) !== -1",
              ns("plot_type")
            ),
            radioButtons(ns("bv_mode"), "Mode",
              choices  = c("Single variable" = "single",
                           "Grouped by X"    = "grouped"),
              selected = "single",
              inline   = TRUE
            )
          ),

          # ── X Variable ───────────────────────────────────────────────
          # Shown for: scatter, bar, hist, density, qq (always)
          # Shown for: box/violin ONLY in grouped mode
          conditionalPanel(
            condition = sprintf(
              paste0(
                "['scatter','bar','hist','density','qq','corr'].indexOf(input['%s']) !== -1 || ",
                "(['box','violin'].indexOf(input['%s']) !== -1 && input['%s'] == 'grouped')"
              ),
              ns("plot_type"), ns("plot_type"), ns("bv_mode")
            ),
            selectInput(ns("x_var"), "X Variable", choices = NULL)
          ),

          # ── Y Variable ───────────────────────────────────────────────
          # For scatter/box-grouped/violin-grouped: full label
          # For box-single/violin-single: label = "Variable"
          conditionalPanel(
            condition = sprintf(
              paste0(
                "['scatter'].indexOf(input['%s']) !== -1 || ",
                "(['box','violin'].indexOf(input['%s']) !== -1)"
              ),
              ns("plot_type"), ns("plot_type")
            ),
            uiOutput(ns("y_var_ui"))
          ),

          # ── Color By (scatter only) ───────────────────────────────────
          conditionalPanel(
            condition = sprintf("input['%s'] == 'scatter'", ns("plot_type")),
            selectInput(ns("color_var"), "Color By", choices = NULL)
          ),

          hr(),

          # ── Fill color (not for corr/scatter) ─────────────────────────
          conditionalPanel(
            condition = sprintf(
              "input['%s'] !== 'corr' && input['%s'] !== 'scatter'",
              ns("plot_type"), ns("plot_type")
            ),
            colourpicker::colourInput(
              ns("fill_color"), "Fill Color", value = "#3c8dbc"
            )
          ),

          # ── Palette (scatter + corr) ───────────────────────────────────
          conditionalPanel(
            condition = sprintf(
              "input['%s'] == 'corr' || input['%s'] == 'scatter'",
              ns("plot_type"), ns("plot_type")
            ),
            selectInput(ns("palette"), "Color Palette",
              choices = c(
                "Blue-Red (diverging)" = "RdBu",
                "Viridis"              = "viridis",
                "Magma"                = "magma",
                "Plasma"               = "plasma",
                "Inferno"              = "inferno",
                "Spectral"             = "Spectral",
                "Green-Purple"         = "PRGn",
                "Blues"                = "Blues",
                "Reds"                 = "Reds"
              ),
              selected = "RdBu"
            )
          ),

          # ── Alpha (all except corr) ───────────────────────────────────
          conditionalPanel(
            condition = sprintf("input['%s'] !== 'corr'", ns("plot_type")),
            sliderInput(ns("alpha"), "Transparency (Alpha)",
                        min = 0.1, max = 1.0, value = 0.8, step = 0.05)
          ),

          hr(),

          # ── Bins (histogram only) ──────────────────────────────────────
          conditionalPanel(
            condition = sprintf("input['%s'] == 'hist'", ns("plot_type")),
            sliderInput(ns("bins"), "Bins (Histogram)",
                        min = 5, max = 100, value = 30)
          ),

          actionButton(ns("plot_btn"), "Plot",
                       class = "btn-primary", width = "100%")
      ),

      box(title = "Plot", width = 9, status = "info", solidHeader = TRUE,
          # Warning banner (numeric X in grouped box/violin)
          uiOutput(ns("x_warning")),
          plotlyOutput(ns("plot"), height = "450px")
      )
    ),

    fluidRow(
      tabBox(width = 12, title = "Analysis",
        tabPanel("Descriptive Statistics", DTOutput(ns("desc_stats"))),
        tabPanel("Normality Tests",
          fluidRow(
            column(4,
              selectInput(ns("norm_var"), "Select Variable", choices = NULL),
              checkboxGroupInput(ns("norm_tests"), "Tests to Run",
                choices = c(
                  "Shapiro-Wilk"       = "sw",
                  "Kolmogorov-Smirnov" = "ks",
                  "Anderson-Darling"   = "ad",
                  "Jarque-Bera"        = "jb"
                ),
                selected = c("sw", "ks")
              ),
              actionButton(ns("norm_btn"), "Run Tests",
                           class = "btn-info", width = "100%")
            ),
            column(8, verbatimTextOutput(ns("norm_result")))
          )
        ),
        tabPanel("Auto EDA",
          fluidRow(column(12,
            actionButton(ns("auto_eda_btn"), "Generate Auto EDA Report",
                         class = "btn-success", width = "100%"), hr()
          )),
          fluidRow(
            column(6, box(title="Dataset Overview", width=NULL,
                          status="primary", solidHeader=TRUE,
                          verbatimTextOutput(ns("auto_overview")))),
            column(6, box(title="Variable Types", width=NULL,
                          status="info", solidHeader=TRUE,
                          DTOutput(ns("auto_vartypes"))))
          ),
          fluidRow(column(12, box(title="Extended Descriptive Statistics",
            width=NULL, status="warning", solidHeader=TRUE,
            DTOutput(ns("auto_extdesc"))))),
          fluidRow(column(12, box(title="Automatic Insights", width=NULL,
            status="danger", solidHeader=TRUE,
            uiOutput(ns("auto_insights")))))
        )
      )
    )
  )
}

# ============================================================
# SERVER
# ============================================================

edaServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    # ── Update selectors ──────────────────────────────────────────────
    observe({
      req(rv$working_data)
      df       <- rv$working_data
      all_vars <- names(df)
      num_vars <- names(df)[vapply(df, is.numeric, logical(1L))]
      updateSelectInput(session, "x_var",     choices = all_vars)
      updateSelectInput(session, "color_var", choices = c("None", all_vars))
      updateSelectInput(session, "norm_var",  choices = num_vars)
    })

    # ── Dynamic Y label: "Variable" in single mode ────────────────────
    output$y_var_ui <- renderUI({
      req(rv$working_data)
      df   <- rv$working_data
      mode <- input$bv_mode %||% "single"
      type <- input$plot_type

      lbl <- if (type %in% c("box", "violin") && mode == "single")
        "Variable (single)" else "Y Variable"

      selectInput(session$ns("y_var"), lbl, choices = names(df))
    })

    # ── Warning: numeric X in grouped box/violin ──────────────────────
    output$x_warning <- renderUI({
      req(rv$working_data)
      type <- input$plot_type
      mode <- input$bv_mode %||% "single"
      if (!type %in% c("box", "violin")) return(NULL)
      if (mode != "grouped") return(NULL)
      req(input$x_var)
      if (is.numeric(rv$working_data[[input$x_var]])) {
        tags$div(
          class = "alert alert-warning",
          style = "margin-bottom:8px;",
          icon("exclamation-triangle"),
          tags$strong(" X is numeric! "),
          "Boxplot/Violin need a categorical X (e.g. gender, department). ",
          "With a continuous X each unique value becomes a separate group. ",
          "Consider using ", tags$strong("Single variable"), " mode, or first ",
          "convert X to a factor in the Preprocessing tab."
        )
      }
    })

    # ── Helpers ───────────────────────────────────────────────────────
    fill_col <- reactive({
      if (is.null(input$fill_color) || input$fill_color == "") "#3c8dbc"
      else input$fill_color
    })

    apply_palette <- function(p, pal, is_cont = TRUE) {
      viridis_p <- c("viridis", "magma", "plasma", "inferno")
      if (pal %in% viridis_p) {
        if (is_cont) p + scale_color_viridis_c(option=pal) +
                         scale_fill_viridis_c(option=pal)
        else         p + scale_color_viridis_d(option=pal) +
                         scale_fill_viridis_d(option=pal)
      } else {
        div_p <- c("RdBu","Spectral","PRGn","PuOr")
        dir   <- if (pal %in% div_p) -1 else 1
        if (is_cont) p + scale_fill_distiller(palette=pal, direction=dir) +
                         scale_color_distiller(palette=pal, direction=dir)
        else         p + scale_fill_brewer(palette=pal) +
                         scale_color_brewer(palette=pal)
      }
    }

    # ── Main plot ─────────────────────────────────────────────────────
    output$plot <- renderPlotly({
      input$plot_btn
      isolate({
        req(rv$working_data)
        df    <- rv$working_data
        type  <- input$plot_type
        col   <- fill_col()
        alpha <- input$alpha %||% 0.8
        mode  <- input$bv_mode %||% "single"

        p <- tryCatch({

          # ── Histogram ──────────────────────────────────────────────
          if (type == "hist") {
            req(input$x_var)
            xv <- input$x_var
            ggplot(df, aes(x = .data[[xv]])) +
              geom_histogram(bins=input$bins, fill=col,
                             color="white", alpha=alpha) +
              theme_minimal() +
              labs(title = paste("Histogram of", xv), x = xv)

          # ── Density ────────────────────────────────────────────────
          } else if (type == "density") {
            req(input$x_var)
            xv <- input$x_var
            ggplot(df, aes(x = .data[[xv]])) +
              geom_density(fill=col, alpha=alpha, color=col) +
              theme_minimal() +
              labs(title = paste("Density of", xv), x = xv)

          # ── Bar ────────────────────────────────────────────────────
          } else if (type == "bar") {
            req(input$x_var)
            xv <- input$x_var
            ggplot(df, aes(x = .data[[xv]])) +
              geom_bar(fill=col, alpha=alpha) +
              theme_minimal() +
              theme(axis.text.x = element_text(angle=45, hjust=1)) +
              labs(title = paste("Bar Chart of", xv), x = xv)

          # ── QQ ──────────────────────────────────────────────────────
          } else if (type == "qq") {
            req(input$x_var)
            xv <- input$x_var
            ggplot(df, aes(sample = .data[[xv]])) +
              stat_qq(color=col, alpha=alpha) +
              stat_qq_line(color="red", linewidth=0.8) +
              theme_minimal() +
              labs(title = paste("Q-Q Plot of", xv))

          # ── Boxplot ─────────────────────────────────────────────────
          } else if (type == "box") {
            req(input$y_var)
            yv <- input$y_var
            if (mode == "single") {
              ggplot(df, aes(x = "", y = .data[[yv]])) +
                geom_boxplot(fill=col, alpha=alpha, width=0.4) +
                theme_minimal() +
                labs(title = paste("Boxplot of", yv), x = "")
            } else {
              req(input$x_var)
              xv <- input$x_var
              ggplot(df, aes(x = .data[[xv]], y = .data[[yv]])) +
                geom_boxplot(fill=col, alpha=alpha) +
                theme_minimal() +
                theme(axis.text.x = element_text(angle=45, hjust=1)) +
                labs(title = paste("Boxplot:", yv, "by", xv),
                     x = xv, y = yv)
            }

          # ── Violin ─────────────────────────────────────────────────
          } else if (type == "violin") {
            req(input$y_var)
            yv <- input$y_var
            if (mode == "single") {
              ggplot(df, aes(x = "", y = .data[[yv]])) +
                geom_violin(fill=col, alpha=alpha) +
                geom_boxplot(width=0.08, fill="white", outlier.shape=NA) +
                theme_minimal() +
                labs(title = paste("Violin of", yv), x = "")
            } else {
              req(input$x_var)
              xv <- input$x_var
              ggplot(df, aes(x = .data[[xv]], y = .data[[yv]])) +
                geom_violin(fill=col, alpha=alpha) +
                geom_boxplot(width=0.08, fill="white", outlier.shape=NA) +
                theme_minimal() +
                theme(axis.text.x = element_text(angle=45, hjust=1)) +
                labs(title = paste("Violin:", yv, "by", xv),
                     x = xv, y = yv)
            }

          # ── Scatter ────────────────────────────────────────────────
          } else if (type == "scatter") {
            req(input$x_var, input$y_var)
            xv <- input$x_var
            yv <- input$y_var
            cv <- if (!is.null(input$color_var) &&
                       input$color_var != "None") input$color_var else NULL
            if (!is.null(cv)) {
              is_cont <- is.numeric(df[[cv]])
              p_base  <- ggplot(df, aes(x = .data[[xv]],
                                        y = .data[[yv]],
                                        color = .data[[cv]])) +
                geom_point(alpha=alpha, size=2) + theme_minimal() +
                labs(title=paste("Scatter:", xv, "vs", yv),
                     x = xv, y = yv)
              apply_palette(p_base, input$palette %||% "viridis", is_cont)
            } else {
              ggplot(df, aes(x = .data[[xv]], y = .data[[yv]])) +
                geom_point(alpha=alpha, color=col, size=2) +
                theme_minimal() +
                labs(title=paste("Scatter:", xv, "vs", yv),
                     x = xv, y = yv)
            }

          # ── Correlation ────────────────────────────────────────────
          } else if (type == "corr") {
            num_df <- df[, vapply(df, is.numeric, logical(1L)), drop=FALSE]
            req(ncol(num_df) >= 2)
            cor_mat <- cor(num_df, use="complete.obs")
            cor_df  <- as.data.frame(as.table(cor_mat))
            names(cor_df) <- c("Var1","Var2","Correlation")
            pal <- input$palette %||% "RdBu"
            vp  <- c("viridis","magma","plasma","inferno")
            div <- c("RdBu","Spectral","PRGn","PuOr")
            p_base <- ggplot(cor_df, aes(x=Var1, y=Var2, fill=Correlation)) +
              geom_tile(color="white") + theme_minimal() +
              theme(axis.text.x=element_text(angle=45,hjust=1)) +
              labs(title="Correlation Heatmap")
            if (pal %in% vp)
              p_base + scale_fill_viridis_c(option=pal, limits=c(-1,1))
            else
              p_base + scale_fill_distiller(palette=pal, limits=c(-1,1),
                                            direction=if(pal %in% div) -1 else 1)
          }

        }, error = function(e) {
          ggplot() +
            annotate("text", x=0.5, y=0.5,
                     label=paste("Error:", e$message), size=5) +
            theme_void()
        })

        ggplotly(p)
      })
    })

    # ── Descriptive Statistics ────────────────────────────────────────
    output$desc_stats <- renderDT({
      req(rv$working_data)
      df     <- rv$working_data
      num_df <- df[, vapply(df, is.numeric, logical(1L)), drop=FALSE]
      req(ncol(num_df) > 0)
      stats <- data.frame(
        Variable = names(num_df),
        Mean     = round(vapply(num_df, mean,   numeric(1L), na.rm=TRUE), 3),
        Median   = round(vapply(num_df, median, numeric(1L), na.rm=TRUE), 3),
        SD       = round(vapply(num_df, sd,     numeric(1L), na.rm=TRUE), 3),
        Min      = round(vapply(num_df, min,    numeric(1L), na.rm=TRUE), 3),
        Max      = round(vapply(num_df, max,    numeric(1L), na.rm=TRUE), 3),
        Missing  = vapply(num_df, function(x) sum(is.na(x)), integer(1L))
      )
      datatable(stats, options=list(pageLength=10), rownames=FALSE)
    })

    # ── Normality Tests ───────────────────────────────────────────────
    observeEvent(input$norm_btn, {
      req(rv$working_data, input$norm_var)
      x     <- na.omit(rv$working_data[[input$norm_var]])
      tests <- input$norm_tests
      output$norm_result <- renderPrint({
        cat("Variable:", input$norm_var, "\nN =", length(x), "\n")
        cat(strrep("\u2500", 40), "\n\n")
        if ("sw" %in% tests) {
          cat("Shapiro-Wilk Test:\n")
          print(shapiro.test(if (length(x)>5000) sample(x,5000) else x))
          cat("\n")
        }
        if ("ks" %in% tests) {
          cat("Kolmogorov-Smirnov Test:\n")
          print(ks.test(x, "pnorm", mean(x), sd(x))); cat("\n")
        }
        if ("ad" %in% tests) {
          cat("Anderson-Darling Test:\n")
          if (requireNamespace("nortest", quietly=TRUE))
            print(nortest::ad.test(x))
          else cat("Install 'nortest' first.\n")
          cat("\n")
        }
        if ("jb" %in% tests) {
          cat("Jarque-Bera Test:\n")
          if (requireNamespace("tseries", quietly=TRUE))
            print(tseries::jarque.bera.test(x))
          else cat("Install 'tseries' first.\n")
          cat("\n")
        }
      })
    })

    # ── Auto EDA ──────────────────────────────────────────────────────
    observeEvent(input$auto_eda_btn, {
      req(rv$working_data)
      df <- rv$working_data

      output$auto_overview <- renderPrint({
        cat("Rows:    ", nrow(df), "\nColumns: ", ncol(df), "\n")
        cat("Total Missing:", sum(is.na(df)), "\n")
        cat("Duplicate Rows:", sum(duplicated(df)), "\n\n")
        cat("Column Types:\n")
        print(vapply(df, function(x) class(x)[1L], character(1L)))
      })

      output$auto_vartypes <- renderDT({
        vt <- data.frame(
          Variable    = names(df),
          Type        = vapply(df, function(x) class(x)[1L], character(1L)),
          N_Unique    = vapply(df, function(x) length(unique(na.omit(x))), integer(1L)),
          N_Missing   = vapply(df, function(x) sum(is.na(x)), integer(1L)),
          Pct_Missing = round(vapply(df, function(x) mean(is.na(x))*100, numeric(1L)), 1)
        )
        datatable(vt, options=list(pageLength=15), rownames=FALSE)
      })

      output$auto_extdesc <- renderDT({
        num_df <- df[, vapply(df, is.numeric, logical(1L)), drop=FALSE]
        req(ncol(num_df) > 0)
        skew_fn <- function(x) {
          x <- na.omit(x); n <- length(x)
          if (n < 3) return(NA_real_)
          m <- mean(x); s <- sd(x)
          if (s == 0) return(NA_real_)
          (sum((x-m)^3)/n)/s^3
        }
        kurt_fn <- function(x) {
          x <- na.omit(x); n <- length(x)
          if (n < 4) return(NA_real_)
          m <- mean(x); s <- sd(x)
          if (s == 0) return(NA_real_)
          (sum((x-m)^4)/n)/s^4 - 3
        }
        ext <- data.frame(
          Variable = names(num_df),
          Mean     = round(vapply(num_df, mean,   numeric(1L), na.rm=TRUE), 3),
          Median   = round(vapply(num_df, median, numeric(1L), na.rm=TRUE), 3),
          SD       = round(vapply(num_df, sd,     numeric(1L), na.rm=TRUE), 3),
          IQR      = round(vapply(num_df, IQR,    numeric(1L), na.rm=TRUE), 3),
          Skewness = round(vapply(num_df, skew_fn, numeric(1L)), 3),
          Kurtosis = round(vapply(num_df, kurt_fn, numeric(1L)), 3),
          Min      = round(vapply(num_df, min,    numeric(1L), na.rm=TRUE), 3),
          Max      = round(vapply(num_df, max,    numeric(1L), na.rm=TRUE), 3),
          Missing  = vapply(num_df, function(x) sum(is.na(x)), integer(1L))
        )
        datatable(ext, options=list(pageLength=15, scrollX=TRUE), rownames=FALSE)
      })

      output$auto_insights <- renderUI({
        insights <- list()
        add <- function(cls, title, msg)
          insights[[length(insights)+1]] <<- tags$div(
            class=paste("alert", cls),
            tags$strong(title), msg)

        miss_pct <- mean(is.na(df)) * 100
        if (miss_pct > 0) add("alert-warning", "Missing Data: ",
          sprintf("%.1f%% of all values are missing.", miss_pct))

        if (sum(duplicated(df)) > 0) add("alert-warning", "Duplicate Rows: ",
          sprintf("%d duplicate rows.", sum(duplicated(df))))

        num_df <- df[, vapply(df, is.numeric, logical(1L)), drop=FALSE]
        if (ncol(num_df) > 0) {
          skew_fn <- function(x) {
            x<-na.omit(x); n<-length(x)
            if(n<3||sd(x)==0) return(NA_real_)
            (sum((x-mean(x))^3)/n)/sd(x)^3
          }
          hs <- names(Filter(function(s) !is.na(s) && abs(s)>1,
                             vapply(num_df, skew_fn, numeric(1L))))
          if (length(hs)) add("alert-info", "High Skewness: ",
            paste("Variables with |skewness| > 1:", paste(hs, collapse=", ")))
        }

        hm <- names(df)[vapply(df, function(x) mean(is.na(x))>0.3, logical(1L))]
        if (length(hm)) add("alert-danger", "High Missing (>30%): ",
          paste(hm, collapse=", "))

        cc <- names(df)[vapply(df, function(x) length(unique(na.omit(x)))==1L, logical(1L))]
        if (length(cc)) add("alert-danger", "Constant Columns: ",
          paste(cc, collapse=", "))

        if (length(insights)==0) add("alert-success", "All clear! ",
          "No major data issues detected.")

        tagList(insights)
      })
    })

  })
}
