#د Clustering 

clusteringUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(title = "Clustering Settings", width = 4, status = "primary", solidHeader = TRUE,
          selectInput(ns("data_type"), "Data Type",
                      choices = c("Numeric Only" = "numeric", "Mixed (Categorical + Numeric)" = "mixed")),
          selectInput(ns("features"), "Select Features", choices = NULL, multiple = TRUE),
          selectInput(ns("algorithm"), "Algorithm",
                      choices = c("K-Means" = "kmeans", "Hierarchical" = "hclust", "DBSCAN" = "dbscan")),
          uiOutput(ns("hyperparams")),
          hr(),
          selectInput(ns("scale_method"), "Scale Data",
                      choices = c("None" = "none", "Z-score" = "zscore", "Min-Max" = "minmax")),
          hr(),
          actionButton(ns("run"), "Run Clustering", class = "btn-primary btn-block", icon = icon("play"))
      ),
      box(title = "Cluster Plot", width = 8, status = "info", solidHeader = TRUE,
          plotlyOutput(ns("cluster_plot"), height = "400px"))
    ),
    fluidRow(
      tabBox(width = 12, title = "Results",
             tabPanel("Cluster Summary",
                      fluidRow(
                        valueBoxOutput(ns("n_clusters"), width = 3),
                        valueBoxOutput(ns("silhouette_box"), width = 3),
                        valueBoxOutput(ns("wss_box"), width = 3),
                        valueBoxOutput(ns("n_noise"), width = 3)
                      ),
                      hr(),
                      DTOutput(ns("cluster_table"))
             ),
             tabPanel("Elbow / Dendrogram", plotlyOutput(ns("elbow_plot"), height = "350px")),
             tabPanel("Silhouette Plot",    plotOutput(ns("silhouette_plot"), height = "350px")),
             tabPanel("Cluster Profiles",  plotlyOutput(ns("profile_plot"), height = "400px"))
      )
    )
  )
}

clusteringServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    
    # helper: replace NULL with default
    
    observe({
      req(rv$working_data)
      df <- rv$working_data
      cols <- if (input$data_type == "numeric") names(df)[vapply(df, is.numeric, logical(1L))] else names(df)
      updateSelectInput(session, "features", choices = cols,
                        selected = cols[seq_len(min(3, length(cols)))])
    }) |> bindEvent(input$data_type, rv$working_data, ignoreNULL = FALSE)
    
    output$hyperparams <- renderUI({
      ns <- session$ns
      algo      <- input$algorithm
      data_type <- input$data_type
      
      dist_choices <- if (data_type == "mixed")
        c("Gower" = "gower", "Euclidean" = "euclidean")
      else
        c("Euclidean" = "euclidean")
      
      if (algo == "kmeans") {
        if (data_type == "mixed") {
          tagList(
            helpText("Mixed data: using PAM with Gower distance."),
            sliderInput(ns("k"), "Number of Clusters (k)", 2, 10, 3),
            sliderInput(ns("pam_iter"), "Max Iterations (PAM)", 10, 300, 100)
          )
        } else {
          tagList(
            sliderInput(ns("k"), "Number of Clusters (k)", 2, 10, 3),
            selectInput(ns("kmeans_init"), "Initialization",
                        choices = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen")),
            sliderInput(ns("kmeans_iter"), "Max Iterations", 10, 300, 100)
          )
        }
      } else if (algo == "hclust") {
        tagList(
          sliderInput(ns("k"), "Number of Clusters (k)", 2, 10, 3),
          selectInput(ns("hclust_method"), "Linkage Method",
                      choices = c("complete", "single", "average", "ward.D2", "centroid")),
          selectInput(ns("dist_metric"), "Distance Metric", choices = dist_choices)
        )
      } else {
        tagList(
          numericInput(ns("eps"), "Epsilon (eps)", value = 0.5, min = 0.01, step = 0.05),
          sliderInput(ns("minpts"), "Min Points", 2, 20, 5),
          selectInput(ns("dist_metric"), "Distance Metric", choices = dist_choices)
        )
      }
    })
    
    prep_data <- reactive({
      req(rv$working_data, input$features)
      df <- rv$working_data[, input$features, drop = FALSE]
      df <- na.omit(df)
      if (nrow(df) == 0) stop("No data left after removing NA values.")
      
      num_cols <- names(df)[vapply(df, is.numeric, logical(1L))]
      cat_cols <- names(df)[vapply(df, function(x) is.character(x) | is.factor(x), logical(1L))]
      
      if (input$scale_method == "zscore" && length(num_cols) > 0) {
        df[, num_cols] <- as.data.frame(scale(df[, num_cols]))
      } else if (input$scale_method == "minmax" && length(num_cols) > 0) {
        df[, num_cols] <- lapply(df[, num_cols, drop = FALSE], function(x) {
          mn <- min(x, na.rm = TRUE); mx <- max(x, na.rm = TRUE)
          if (mx == mn) return(x)
          (x - mn) / (mx - mn)
        })
      }
      
      for (col in cat_cols) df[[col]] <- as.factor(df[[col]])
      df
    })
    
    calc_dist <- reactive({
      req(prep_data())
      df        <- prep_data()
      algo      <- input$algorithm
      data_type <- input$data_type
      
      use_gower <- data_type == "mixed" && {
        metric <- if (algo %in% c("hclust", "dbscan")) input$dist_metric %||% "gower" else "gower"
        metric == "gower"
      }
      
      if (use_gower || (data_type == "mixed" && algo == "kmeans")) {
        cluster::daisy(df, metric = "gower")
      } else {
        num_df <- df[, vapply(df, is.numeric, logical(1L)), drop = FALSE]
        if (ncol(num_df) == 0) stop("No numeric columns available for Euclidean distance.")
        dist(num_df)
      }
    })
    
    cluster_result <- eventReactive(input$run, {
      req(prep_data(), calc_dist())
      df       <- prep_data()
      dist_mat <- calc_dist()
      algo     <- input$algorithm
      
      result <- tryCatch({
        if (algo == "kmeans") {
          k <- isolate(input$k) %||% 3
          if (input$data_type == "mixed") {
            mdl <- cluster::pam(dist_mat, k = k,
                                diss    = TRUE,
                                nstart  = 1)
            list(labels = mdl$clustering, model = mdl,
                 wss = mdl$objective[["swap"]], k = k, dist = dist_mat)
          } else {
            mdl <- kmeans(df[, vapply(df, is.numeric, logical(1L)), drop = FALSE],
                          centers   = k,
                          algorithm = isolate(input$kmeans_init) %||% "Hartigan-Wong",
                          iter.max  = isolate(input$kmeans_iter) %||% 100,
                          nstart    = 25)
            list(labels = mdl$cluster, model = mdl,
                 wss = mdl$tot.withinss, k = k, dist = dist_mat)
          }
          
        } else if (algo == "hclust") {
          k      <- isolate(input$k) %||% 3
          method <- isolate(input$hclust_method) %||% "ward.D2"
          mdl    <- hclust(dist_mat, method = method)
          labels <- cutree(mdl, k = k)
          num_df <- df[, vapply(df, is.numeric, logical(1L)), drop = FALSE]
          wss <- sum(vapply(unique(labels), function(cl) {
            sub <- num_df[labels == cl, , drop = FALSE]
            if (nrow(sub) < 2) return(0)
            sum(apply(sub, 2, var) * (nrow(sub) - 1))
          }))
          list(labels = labels, model = mdl, wss = wss, k = k, dist = dist_mat)
          
        } else {
          if (!requireNamespace("dbscan", quietly = TRUE)) stop("Package dbscan is not installed.")
          eps    <- isolate(input$eps)    %||% 0.5
          minpts <- isolate(input$minpts) %||% 5
          mdl    <- dbscan::dbscan(dist_mat, eps = eps, minPts = minpts)
          k      <- length(unique(mdl$cluster[mdl$cluster != 0]))
          list(labels = mdl$cluster, model = mdl, wss = NA, k = k, dist = dist_mat)
        }
      }, error = function(e) stop(paste("Clustering error:", e$message)))
      
      # attach data BEFORE silhouette calculation
      result$data <- df
      
      sil <- tryCatch({
        labs      <- result$labels
        valid     <- labs != 0
        dist_used <- result$dist  # use the SAME distance matrix as clustering

        if (length(unique(labs[valid])) >= 2 && !is.null(dist_used)) {
          # Subset distance matrix to non-noise points
          dist_sub <- if (all(valid)) dist_used
                      else as.dist(as.matrix(dist_used)[valid, valid])
          s <- cluster::silhouette(labs[valid], dist_sub)
          mean(s[, 3])
        } else NA_real_
      }, error = function(e) NA_real_)
      
      result$silhouette <- sil
      result
    })
    
    output$n_clusters <- renderValueBox({
      valueBox(cluster_result()$k, "Clusters", icon = icon("object-group"), color = "blue")
    })
    output$silhouette_box <- renderValueBox({
      val <- cluster_result()$silhouette
      valueBox(ifelse(is.na(val), "N/A", round(val, 3)), "Silhouette",
               icon = icon("star"), color = "green")
    })
    output$wss_box <- renderValueBox({
      val <- cluster_result()$wss
      valueBox(ifelse(is.na(val), "N/A", round(val, 1)), "Total WSS",
               icon = icon("compress"), color = "yellow")
    })
    output$n_noise <- renderValueBox({
      noise <- sum(cluster_result()$labels == 0)
      valueBox(noise, "Noise Points", icon = icon("exclamation-triangle"), color = "red")
    })
    
    output$cluster_plot <- renderPlotly({
      req(cluster_result())
      res    <- cluster_result()
      df     <- res$data
      labels <- as.factor(res$labels)
      num_df <- df[, vapply(df, is.numeric, logical(1L)), drop = FALSE]
      
      if (ncol(num_df) >= 2) {
        pca <- prcomp(num_df, scale. = FALSE)
        df2 <- data.frame(PC1 = pca$x[, 1], PC2 = pca$x[, 2], Cluster = labels)
        p <- ggplot(df2, aes(x = PC1, y = PC2, color = Cluster)) +
          geom_point(alpha = 0.7, size = 2) + theme_minimal() +
          labs(title = "Cluster Plot (PCA)")
        ggplotly(p)
      } else {
        plotly_empty() %>% layout(title = "Need at least 2 numeric features for PCA plot")
      }
    })
    
    output$cluster_table <- renderDT({
      req(cluster_result())
      res        <- cluster_result()
      df2        <- res$data
      df2$Cluster <- res$labels
      datatable(df2, options = list(pageLength = 10, scrollX = TRUE))
    })
    
    output$elbow_plot <- renderPlotly({
      req(cluster_result())
      res  <- cluster_result()
      df   <- res$data
      algo <- input$algorithm
      
      if (algo == "kmeans") {
        num_df   <- df[, vapply(df, is.numeric, logical(1L)), drop = FALSE]
        wss_vals <- vapply(1:10, function(k) {
          kmeans(num_df, centers = k, nstart = 10, iter.max = 100)$tot.withinss
        })
        df2 <- data.frame(k = 1:10, WSS = wss_vals)
        p <- ggplot(df2, aes(x = k, y = WSS)) +
          geom_line(color = "#3c8dbc") + geom_point(color = "#3c8dbc", size = 3) +
          theme_minimal() + labs(title = "Elbow Method", x = "Number of Clusters", y = "Total WSS")
        ggplotly(p)
        
      } else if (algo == "hclust") {
        mdl <- res$model
        df2 <- tail(data.frame(Index = seq_along(mdl$height), Height = mdl$height), 20)
        p <- ggplot(df2, aes(x = Index, y = Height)) +
          geom_line(color = "#3c8dbc") + geom_point(color = "#3c8dbc", size = 2) +
          theme_minimal() + labs(title = "Dendrogram Heights (last 20 merges)")
        ggplotly(p)
        
      } else {
        plotly_empty() %>% layout(title = "Not applicable for DBSCAN")
      }
    })
    
    output$silhouette_plot <- renderPlot({
      req(cluster_result())
      res    <- cluster_result()
      df     <- res$data
      labs   <- res$labels
      valid  <- labs != 0
      num_df <- df[, vapply(df, is.numeric, logical(1L)), drop = FALSE]
      
      if (length(unique(labs[valid])) >= 2 && ncol(num_df) > 0) {
        s <- cluster::silhouette(labs[valid], dist(num_df[valid, , drop = FALSE]))
        plot(s, col = 1:max(labs[valid]), border = NA, main = "Silhouette Plot")
      } else {
        plot.new(); title("Not enough clusters for silhouette")
      }
    })
    
    output$profile_plot <- renderPlotly({
      req(cluster_result())
      res         <- cluster_result()
      df2         <- res$data
      df2$Cluster <- as.factor(res$labels)
      df2         <- df2[df2$Cluster != "0", ]
      num_cols    <- names(df2)[vapply(df2, is.numeric, logical(1L))]
      
      if (length(num_cols) == 0)
        return(plotly_empty() %>% layout(title = "No numeric features for profile plot"))
      
      means <- df2 %>%
        group_by(Cluster) %>%
        summarise(across(all_of(num_cols), \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
        tidyr::pivot_longer(-Cluster, names_to = "Feature", values_to = "Mean")
      
      p <- ggplot(means, aes(x = Feature, y = Mean, fill = Cluster)) +
        geom_bar(stat = "identity", position = "dodge") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = "Cluster Profiles (Mean per Feature)")
      ggplotly(p)
    })
    
  })
}

