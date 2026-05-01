# ============================================================
# DrData — Data Import Module
# FIX: datasets::Titanic instead of data("Titanic")
#      vapply() instead of sapply() for type safety
# ============================================================

dataUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(title = "Import Data", width = 4, status = "primary", solidHeader = TRUE,
        fileInput(ns("file"), "Choose File",
          accept = c(".csv", ".xlsx", ".xls", ".rds", ".txt")
        ),
        radioButtons(ns("sep"), "Separator (CSV / TXT)",
          choices  = c(Comma = ",", Semicolon = ";", Tab = "\t"),
          selected = ","
        ),
        checkboxInput(ns("header"), "Header row", value = TRUE),
        hr(),
        h5("Or load example dataset:"),
        selectInput(ns("example"), "Example Datasets",
          choices = c("None", "iris", "mtcars", "Titanic")
        ),
        actionButton(ns("load_example"), "Load Example", class = "btn-info")
      ),
      box(title = "Data Summary", width = 8, status = "info", solidHeader = TRUE,
        verbatimTextOutput(ns("summary")),
        hr(),
        DTOutput(ns("preview"))
      )
    ),
    fluidRow(
      box(title = "Variable Info", width = 12, status = "warning", solidHeader = TRUE,
        DTOutput(ns("var_info"))
      )
    )
  )
}

dataServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    # ── Load uploaded file ──────────────────────────────────────────────────
    observeEvent(input$file, {
      req(input$file)
      ext <- tools::file_ext(input$file$name)
      df <- tryCatch({
        switch(ext,
          csv  = utils::read.csv(input$file$datapath,
                                 header = input$header, sep = input$sep),
          txt  = utils::read.csv(input$file$datapath,
                                 header = input$header, sep = input$sep),
          xlsx = readxl::read_excel(input$file$datapath),
          xls  = readxl::read_excel(input$file$datapath),
          rds  = readRDS(input$file$datapath),
          stop("Unsupported file format: .", ext)
        )
      }, error = function(e) {
        showNotification(paste("Error loading file:", e$message), type = "error")
        NULL
      })
      if (!is.null(df)) {
        rv$raw_data     <- as.data.frame(df)
        rv$working_data <- as.data.frame(df)
        showNotification("Data loaded successfully!", type = "message")
      }
    })

    # ── Load example dataset ────────────────────────────────────────────────
    # FIX: use datasets:: to avoid polluting .GlobalEnv with data()
    observeEvent(input$load_example, {
      req(input$example != "None")
      df <- switch(input$example,
        iris    = datasets::iris,
        mtcars  = datasets::mtcars,
        Titanic = as.data.frame(datasets::Titanic)
      )
      rv$raw_data     <- df
      rv$working_data <- df
      showNotification(paste(input$example, "loaded!"), type = "message")
    })

    # ── Summary ──────────────────────────────────────────────────────────────
    output$summary <- renderPrint({
      req(rv$working_data)
      df <- rv$working_data
      cat("Rows:", nrow(df), " | Cols:", ncol(df), "\n\n")
      utils::str(df)
    })

    # ── Preview table ─────────────────────────────────────────────────────────
    output$preview <- renderDT({
      req(rv$working_data)
      datatable(head(rv$working_data, 100),
        options = list(scrollX = TRUE, pageLength = 10),
        rownames = FALSE
      )
    })

    # ── Variable info ─────────────────────────────────────────────────────────
    output$var_info <- renderDT({
      req(rv$working_data)
      df <- rv$working_data
      info <- data.frame(
        Variable = names(df),
        Type     = vapply(df, function(x) class(x)[1L], character(1L)),
        Missing  = vapply(df, function(x) sum(is.na(x)),  integer(1L)),
        Unique   = vapply(df, function(x) length(unique(x)), integer(1L)),
        stringsAsFactors = FALSE
      )
      datatable(info, options = list(pageLength = 15), rownames = FALSE)
    })
  })
}
