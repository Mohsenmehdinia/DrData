# utils_eda.R
# توابع کمکی برای ماژول EDA - جدا از منطق Shiny

# ─── آمار توصیفی ────────────────────────────────────────────────────────────

compute_descriptive_stats <- function(df) {
  num_df <- df[, vapply(df, is.numeric, logical(1L)), drop = FALSE]
  if (ncol(num_df) == 0) return(NULL)
  
  data.frame(
    Variable  = names(num_df),
    N         = vapply(num_df, function(x) sum(!is.na(x)), integer(1L)),
    Mean      = round(vapply(num_df, mean, numeric(1L), na.rm = TRUE), 3),
    Median    = round(vapply(num_df, median, numeric(1L), na.rm = TRUE), 3),
    SD        = round(vapply(num_df, sd, numeric(1L), na.rm = TRUE), 3),
    IQR       = round(vapply(num_df, IQR, numeric(1L), na.rm = TRUE), 3),
    Skewness  = round(vapply(num_df, function(x) {
      x <- na.omit(x); n <- length(x)
      if (n < 3) return(NA)
      m <- mean(x); s <- sd(x)
      if (s == 0) return(NA)
      sum((x - m)^3) / (n * s^3)
    }), 3),
    Kurtosis  = round(vapply(num_df, function(x) {
      x <- na.omit(x); n <- length(x)
      if (n < 4) return(NA)
      m <- mean(x); s <- sd(x)
      if (s == 0) return(NA)
      sum((x - m)^4) / (n * s^4) - 3
    }), 3),
    Min       = round(vapply(num_df, min, numeric(1L), na.rm = TRUE), 3),
    Max       = round(vapply(num_df, max, numeric(1L), na.rm = TRUE), 3),
    Missing   = vapply(num_df, function(x) sum(is.na(x)), integer(1L)),
    Missing_pct = round(vapply(num_df, function(x) mean(is.na(x)) * 100, numeric(1L)), 1),
    stringsAsFactors = FALSE
  )
}

# ─── آزمون‌های نرمالیتی ──────────────────────────────────────────────────────

run_normality_tests <- function(x) {
  x <- na.omit(x)
  results <- list()
  
  # Shapiro-Wilk (max 5000)
  sw_x <- if (length(x) > 5000) sample(x, 5000) else x
  results$shapiro <- tryCatch(shapiro.test(sw_x), error = function(e) NULL)
  
  # Kolmogorov-Smirnov
  results$ks <- tryCatch(
    ks.test(x, "pnorm", mean(x), sd(x)),
    error = function(e) NULL
  )
  
  # Anderson-Darling (نیاز به nortest)
  results$ad <- tryCatch({
    if (requireNamespace("nortest", quietly = TRUE))
      nortest::ad.test(x)
    else NULL
  }, error = function(e) NULL)
  
  # Jarque-Bera (نیاز به tseries)
  results$jb <- tryCatch({
    if (requireNamespace("tseries", quietly = TRUE))
      tseries::jarque.bera.test(x)
    else NULL
  }, error = function(e) NULL)
  
  results
}

format_normality_results <- function(test_list, var_name) {
  cat(sprintf("=== Normality Tests for: %s ===\n\n", var_name))
  
  if (!is.null(test_list$shapiro)) {
    cat("Shapiro-Wilk Test:\n")
    print(test_list$shapiro)
    cat("\n")
  }
  if (!is.null(test_list$ks)) {
    cat("Kolmogorov-Smirnov Test:\n")
    print(test_list$ks)
    cat("\n")
  }
  if (!is.null(test_list$ad)) {
    cat("Anderson-Darling Test:\n")
    print(test_list$ad)
    cat("\n")
  }
  if (!is.null(test_list$jb)) {
    cat("Jarque-Bera Test:\n")
    print(test_list$jb)
    cat("\n")
  }
  
  # نتیجه‌گیری ساده
  p_vals <- c(
    SW = if (!is.null(test_list$shapiro)) test_list$shapiro$p.value else NA,
    KS = if (!is.null(test_list$ks))      test_list$ks$p.value      else NA,
    AD = if (!is.null(test_list$ad))      test_list$ad$p.value      else NA,
    JB = if (!is.null(test_list$jb))      test_list$jb$p.value      else NA
  )
  p_vals <- na.omit(p_vals)
  if (length(p_vals) > 0) {
    n_reject <- sum(p_vals < 0.05)
    cat(sprintf(
      "─── Conclusion: %d of %d tests reject normality (p < 0.05) ───\n",
      n_reject, length(p_vals)
    ))
  }
}

# ─── تحلیل داده‌های گمشده ───────────────────────────────────────────────────

compute_missing_summary <- function(df) {
  data.frame(
    Variable    = names(df),
    Type        = vapply(df, function(x) class(x)[1L], character(1L)),
    Missing     = vapply(df, function(x) sum(is.na(x)), integer(1L)),
    Missing_pct = round(vapply(df, function(x) mean(is.na(x)) * 100, numeric(1L)), 1),
    Complete    = vapply(df, function(x) sum(!is.na(x)), integer(1L)),
    stringsAsFactors = FALSE
  ) |> dplyr::arrange(dplyr::desc(Missing_pct))
}

# ─── ماتریس همبستگی ─────────────────────────────────────────────────────────

compute_correlation_matrix <- function(df, method = "pearson") {
  num_df <- df[, vapply(df, is.numeric, logical(1L)), drop = FALSE]
  if (ncol(num_df) < 2) return(NULL)
  cor(num_df, use = "complete.obs", method = method)
}

# ─── توابع ترسیم ─────────────────────────────────────────────────────────────

apply_plot_labels <- function(p, title = "", x_label = "", y_label = "") {
  p + ggplot2::labs(
    title = if (nchar(trimws(title))   > 0) trimws(title)   else NULL,
    x     = if (nchar(trimws(x_label)) > 0) trimws(x_label) else NULL,
    y     = if (nchar(trimws(y_label)) > 0) trimws(y_label) else NULL
  ) + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
}

build_histogram <- function(df, x, bins = 30, fill_color = "#3c8dbc") {
  if (!is.numeric(df[[x]])) {
    ggplot2::ggplot(df, ggplot2::aes(x = .data[[x]])) +
      ggplot2::geom_bar(fill = fill_color, color = "white") +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  } else {
    ggplot2::ggplot(df, ggplot2::aes(x = .data[[x]])) +
      ggplot2::geom_histogram(bins = bins, fill = fill_color, color = "white") +
      ggplot2::theme_minimal()
  }
}

build_density <- function(df, x, fill_color = "#3c8dbc") {
  stopifnot(is.numeric(df[[x]]))
  ggplot2::ggplot(df, ggplot2::aes(x = .data[[x]])) +
    ggplot2::geom_density(fill = fill_color, alpha = 0.5) +
    ggplot2::theme_minimal()
}

build_boxplot <- function(df, x, y, fill_color = "#3c8dbc") {
  ggplot2::ggplot(df, ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
    ggplot2::geom_boxplot(fill = fill_color) +
    ggplot2::theme_minimal()
}

build_violin <- function(df, x, y, fill_color = "#3c8dbc") {
  ggplot2::ggplot(df, ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
    ggplot2::geom_violin(fill = fill_color, alpha = 0.7) +
    ggplot2::geom_boxplot(width = 0.1) +
    ggplot2::theme_minimal()
}

build_scatter <- function(df, x, y, color_var = NULL, fill_color = "#3c8dbc") {
  if (!is.null(color_var) && color_var != "None") {
    df[[color_var]] <- as.factor(df[[color_var]])
    ggplot2::ggplot(df, ggplot2::aes(x = .data[[x]], y = .data[[y]], color = .data[[color_var]])) +
      ggplot2::geom_point(alpha = 0.7) +
      ggplot2::theme_minimal()
  } else {
    ggplot2::ggplot(df, ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
      ggplot2::geom_point(alpha = 0.7, color = fill_color) +
      ggplot2::theme_minimal()
  }
}

build_bar <- function(df, x, fill_color = "#3c8dbc") {
  ggplot2::ggplot(df, ggplot2::aes(x = .data[[x]])) +
    ggplot2::geom_bar(fill = fill_color) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

build_qq <- function(df, x) {
  stopifnot(is.numeric(df[[x]]))
  ggplot2::ggplot(df, ggplot2::aes(sample = .data[[x]])) +
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line(color = "red") +
    ggplot2::theme_minimal()
}

build_correlation_heatmap <- function(df) {
  cor_mat <- compute_correlation_matrix(df)
  if (is.null(cor_mat)) return(NULL)
  cor_df        <- as.data.frame(as.table(cor_mat))
  names(cor_df) <- c("Var1", "Var2", "Correlation")
  ggplot2::ggplot(cor_df, ggplot2::aes(x = Var1, y = Var2, fill = Correlation)) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(ggplot2::aes(label = round(Correlation, 2)),
                       size = 3, color = "black") +
    ggplot2::scale_fill_gradient2(
      low = "blue", mid = "white", high = "red", midpoint = 0
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

build_missing_plot <- function(df) {
  miss_df <- compute_missing_summary(df)
  miss_df  <- miss_df[miss_df$Missing > 0, ]
  if (nrow(miss_df) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5,
                          label = "No missing values found", size = 6) +
        ggplot2::theme_void()
    )
  }
  miss_df$Variable <- factor(miss_df$Variable,
                             levels = miss_df$Variable[order(miss_df$Missing_pct)])
  ggplot2::ggplot(miss_df, ggplot2::aes(x = Variable, y = Missing_pct)) +
    ggplot2::geom_col(fill = "#e74c3c") +
    ggplot2::geom_text(ggplot2::aes(label = paste0(Missing_pct, "%")),
                       hjust = -0.1, size = 3) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = "Missing (%)") +
    ggplot2::theme_minimal()
}

# ─── Auto-EDA Engine ─────────────────────────────────────────────────────────

run_auto_eda <- function(df) {
  num_vars  <- names(df)[vapply(df, is.numeric, logical(1L))]
  cat_vars  <- names(df)[vapply(df, function(x) is.character(x) | is.factor(x), logical(1L))]
  n_rows    <- nrow(df)
  n_cols    <- ncol(df)
  miss_df   <- compute_missing_summary(df)
  high_miss <- miss_df[miss_df$Missing_pct > 20, "Variable"]
  
  insights <- list()
  
  # ── پروفایل کلی ──
  insights$profile <- list(
    n_rows       = n_rows,
    n_cols       = n_cols,
    n_numeric    = length(num_vars),
    n_categorical = length(cat_vars),
    total_missing = sum(is.na(df)),
    missing_pct   = round(mean(is.na(df)) * 100, 1),
    duplicate_rows = sum(duplicated(df))
  )
  
  # ── مشکلات داده ──
  issues <- character(0)
  if (length(high_miss) > 0)
    issues <- c(issues, sprintf("High missing (>20%%): %s",
                                paste(high_miss, collapse = ", ")))
  if (insights$profile$duplicate_rows > 0)
    issues <- c(issues, sprintf("%d duplicate rows detected",
                                insights$profile$duplicate_rows))
  
  # متغیرهای با واریانس صفر
  zero_var <- num_vars[vapply(df[num_vars], function(x) {
    v <- var(x, na.rm = TRUE); !is.na(v) && v == 0
  })]
  if (length(zero_var) > 0)
    issues <- c(issues, sprintf("Zero variance: %s",
                                paste(zero_var, collapse = ", ")))
  
  # outlier ساده (IQR method)
  outlier_vars <- num_vars[vapply(df[num_vars], function(x) {
    x <- na.omit(x)
    if (length(x) < 10) return(FALSE)
    q  <- quantile(x, c(0.25, 0.75))
    iq <- q[2] - q[1]
    sum(x < q[1] - 1.5 * iq | x > q[2] + 1.5 * iq) / length(x) > 0.05
  })]
  if (length(outlier_vars) > 0)
    issues <- c(issues, sprintf("Potential outliers (>5%% of data): %s",
                                paste(outlier_vars, collapse = ", ")))
  
  insights$issues <- if (length(issues) > 0) issues else "No major issues detected"
  
  # ── توزیع متغیرها ──
  if (length(num_vars) > 0) {
    skew_info <- vapply(df[num_vars], function(x) {
      x <- na.omit(x)
      if (length(x) < 3) return(NA)
      m <- mean(x); s <- sd(x)
      if (s == 0) return(NA)
      sum((x - m)^3) / (length(x) * s^3)
    })
    insights$skewed_vars <- list(
      right = names(skew_info[!is.na(skew_info) & skew_info >  1]),
      left  = names(skew_info[!is.na(skew_info) & skew_info < -1])
    )
  }
  
  # ── همبستگی بالا ──
  if (length(num_vars) >= 2) {
    cor_mat   <- compute_correlation_matrix(df)
    cor_pairs <- which(abs(cor_mat) > 0.8 & upper.tri(cor_mat), arr.ind = TRUE)
    if (nrow(cor_pairs) > 0) {
      insights$high_correlations <- apply(cor_pairs, 1, function(idx) {
        sprintf("%s ~ %s (r = %.2f)",
                rownames(cor_mat)[idx[1]],
                colnames(cor_mat)[idx[2]],
                cor_mat[idx[1], idx[2]])
      })
    } else {
      insights$high_correlations <- "No high correlations (|r| > 0.8) found"
    }
  }
  
  # ── اهمیت متغیرها (variance-based ranking) ──
  if (length(num_vars) > 0) {
    vars_scaled <- vapply(df[num_vars], function(x) {
      x <- na.omit(x)
      if (length(x) < 2) return(NA)
      var(x, na.rm = TRUE) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE) + 1e-10)
    })
    insights$variable_importance <- sort(vars_scaled, decreasing = TRUE)
  }
  
  # ── متغیرهای categorical ──
  if (length(cat_vars) > 0) {
    insights$categorical_summary <- lapply(df[cat_vars], function(x) {
      tbl <- sort(table(x, useNA = "ifany"), decreasing = TRUE)
      list(
        n_unique   = length(unique(na.omit(x))),
        top_values = head(tbl, 5)
      )
    })
  }
  
  insights
}

format_auto_eda_text <- function(insights) {
  lines <- character(0)
  
  p <- insights$profile
  lines <- c(lines,
             "═══════════════════════════════════════",
             "           AUTO EDA REPORT             ",
             "═══════════════════════════════════════",
             sprintf("Rows: %d  |  Columns: %d", p$n_rows, p$n_cols),
             sprintf("Numeric: %d  |  Categorical: %d", p$n_numeric, p$n_categorical),
             sprintf("Missing: %d cells (%.1f%%)  |  Duplicates: %d",
                     p$total_missing, p$missing_pct, p$duplicate_rows),
             "",
             "─── Data Issues ───────────────────────"
  )
  lines <- c(lines, if (is.character(insights$issues))
    insights$issues else paste("•", insights$issues))
  
  if (!is.null(insights$skewed_vars)) {
    lines <- c(lines, "", "─── Distribution ──────────────────────")
    if (length(insights$skewed_vars$right) > 0)
      lines <- c(lines, paste("Right-skewed:", paste(insights$skewed_vars$right, collapse = ", ")))
    if (length(insights$skewed_vars$left) > 0)
      lines <- c(lines, paste("Left-skewed:", paste(insights$skewed_vars$left, collapse = ", ")))
    if (length(insights$skewed_vars$right) == 0 && length(insights$skewed_vars$left) == 0)
      lines <- c(lines, "All numeric variables appear roughly symmetric")
  }
  
  if (!is.null(insights$high_correlations)) {
    lines <- c(lines, "", "─── High Correlations (|r| > 0.8) ────")
    lines <- c(lines, if (is.character(insights$high_correlations))
      insights$high_correlations else paste("•", insights$high_correlations))
  }
  
  if (!is.null(insights$variable_importance)) {
    lines <- c(lines, "", "─── Variable Importance (variance-based)")
    top5  <- head(insights$variable_importance, 5)
    lines <- c(lines, sprintf("  %d. %s (%.4f)",
                              seq_along(top5), names(top5), top5))
  }
  
  paste(lines, collapse = "\n")
}
