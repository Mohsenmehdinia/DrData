# =============================================================================
# utils_preprocess.R
# Reusable preprocessing functions for DrData
# =============================================================================


# -----------------------------------------------------------------------------
# handle_missing_values()
# Applies column threshold removal + imputation strategy
# Returns: list(data = df, log = character)
# -----------------------------------------------------------------------------
handle_missing_values <- function(df, method = "none", col_thresh = 1.0) {
  log_msgs <- character(0)
  
  # 1. Drop columns exceeding missing threshold
  miss_rate <- vapply(df, function(x) mean(is.na(x)), numeric(1L))
  drop_cols <- names(miss_rate)[miss_rate > col_thresh]
  if (length(drop_cols) > 0) {
    df <- df[, !names(df) %in% drop_cols, drop = FALSE]
    log_msgs <- c(log_msgs,
                  paste0("Dropped ", length(drop_cols), " column(s) with >{",
                         round(col_thresh * 100), "}% missing: ",
                         paste(drop_cols, collapse = ", ")))
  }
  
  # 2. Apply imputation
  if (method == "remove") {
    before <- nrow(df)
    df <- na.omit(df)
    log_msgs <- c(log_msgs,
                  paste0("Removed ", before - nrow(df), " row(s) with missing values"))
    
  } else if (method == "mean") {
    df <- df %>%
      mutate(across(where(is.numeric),
                    ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
    log_msgs <- c(log_msgs, "Applied mean imputation to numeric columns")
    
  } else if (method == "median") {
    df <- df %>%
      mutate(across(where(is.numeric),
                    ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))
    log_msgs <- c(log_msgs, "Applied median imputation to numeric columns")
    
  } else if (method == "mode") {
    .mode_val <- function(x) {
      ux <- unique(na.omit(x))
      if (length(ux) == 0) return(NA)
      ux[which.max(tabulate(match(x, ux)))]
    }
    df <- df %>%
      mutate(across(everything(), ~ {
        mv <- .mode_val(.)
        ifelse(is.na(.), mv, .)
      }))
    log_msgs <- c(log_msgs, "Applied mode imputation to all columns")
  }
  
  list(data = df, log = log_msgs)
}

# -----------------------------------------------------------------------------
# remove_duplicates()
# scope: "all" or character vector of column names
# Returns: list(data, log)
# -----------------------------------------------------------------------------
remove_duplicates <- function(df, scope = "all") {
  before <- nrow(df)
  
  if (identical(scope, "all")) {
    df <- df[!duplicated(df), ]
  } else {
    valid_cols <- intersect(scope, names(df))
    if (length(valid_cols) == 0) stop("No valid columns specified for dedup.")
    df <- df[!duplicated(df[, valid_cols, drop = FALSE]), ]
  }
  
  removed <- before - nrow(df)
  list(
    data = df,
    log  = paste0("Removed ", removed, " duplicate row(s) based on ",
                  if (identical(scope, "all")) "all columns"
                  else paste(scope, collapse = ", "))
  )
}

# -----------------------------------------------------------------------------
# drop_columns()
# cols: character vector of column names to drop
# Returns: list(data, log)
# -----------------------------------------------------------------------------
drop_columns <- function(df, cols) {
  valid <- intersect(cols, names(df))
  if (length(valid) == 0) stop("None of the specified columns exist.")
  df <- df[, !names(df) %in% valid, drop = FALSE]
  list(
    data = df,
    log  = paste0("Dropped column(s): ", paste(valid, collapse = ", "))
  )
}

# -----------------------------------------------------------------------------
# drop_rows()
# idx: integer vector of row indices (1-based)
# Returns: list(data, log)
# -----------------------------------------------------------------------------
drop_rows <- function(df, idx) {
  valid_idx <- idx[idx >= 1 & idx <= nrow(df)]
  if (length(valid_idx) == 0) stop("No valid row indices provided.")
  df <- df[-valid_idx, , drop = FALSE]
  rownames(df) <- NULL
  list(
    data = df,
    log  = paste0("Dropped ", length(valid_idx), " row(s) at index: ",
                  paste(valid_idx, collapse = ", "))
  )
}

# -----------------------------------------------------------------------------
# scale_variables()
# method: "zscore" | "minmax" | "log"
# vars: character vector of numeric column names
# Returns: list(data, log)
# -----------------------------------------------------------------------------
scale_variables <- function(df, vars, method = "zscore") {
  valid <- intersect(vars, names(df)[vapply(df, is.numeric, logical(1L))])
  if (length(valid) == 0) stop("No valid numeric columns to scale.")
  
  for (v in valid) {
    df[[v]] <- switch(method,
                      zscore = as.numeric(scale(df[[v]])),
                      minmax = {
                        mn <- min(df[[v]], na.rm = TRUE)
                        mx <- max(df[[v]], na.rm = TRUE)
                        if (mx == mn) df[[v]] else (df[[v]] - mn) / (mx - mn)
                      },
                      log    = log1p(pmax(df[[v]], 0))  # guard against negative values
    )
  }
  
  list(
    data = df,
    log  = paste0("Applied ", method, " scaling to: ", paste(valid, collapse = ", "))
  )
}

# -----------------------------------------------------------------------------
# encode_categorical()
# Uses caret::dummyVars for robust one-hot encoding
# fullRank = TRUE drops one level per factor (avoids multicollinearity)
# Returns: list(data, log)
# -----------------------------------------------------------------------------
encode_categorical <- function(df, vars, full_rank = TRUE) {
  valid <- intersect(vars, names(df))
  if (length(valid) == 0) stop("No valid columns to encode.")
  
  # Ensure target columns are factors
  df[valid] <- lapply(df[valid], as.factor)
  
  formula_str <- paste("~", paste(valid, collapse = " + "))
  dv <- caret::dummyVars(as.formula(formula_str), data = df, fullRank = full_rank)
  encoded <- as.data.frame(predict(dv, newdata = df))
  
  # Clean column names (replace spaces/dots with underscores)
  names(encoded) <- gsub("[^A-Za-z0-9_]", "_", names(encoded))
  
  df <- cbind(df[, !names(df) %in% valid, drop = FALSE], encoded)
  
  list(
    data = df,
    log  = paste0("One-hot encoded (fullRank=", full_rank, "): ",
                  paste(valid, collapse = ", "))
  )
}

# -----------------------------------------------------------------------------
# rename_variable()
# Returns: list(data, log)
# -----------------------------------------------------------------------------
rename_variable <- function(df, old_name, new_name) {
  if (!old_name %in% names(df)) stop(paste("Column not found:", old_name))
  if (new_name %in% names(df))  stop(paste("Column already exists:", new_name))
  names(df)[names(df) == old_name] <- new_name
  list(
    data = df,
    log  = paste0('Renamed "', old_name, '" to "', new_name, '"')
  )
}

# -----------------------------------------------------------------------------
# remove_near_zero_variance()
# Uses caret::nearZeroVar to identify and drop low-info columns
# Returns: list(data, log)
# -----------------------------------------------------------------------------
remove_near_zero_variance <- function(df, freq_cut = 95/5, unique_cut = 10) {
  nzv_idx <- caret::nearZeroVar(df,
                                freqCut   = freq_cut,
                                uniqueCut = unique_cut)
  if (length(nzv_idx) == 0) {
    return(list(data = df, log = "No near-zero variance columns found"))
  }
  dropped <- names(df)[nzv_idx]
  df <- df[, -nzv_idx, drop = FALSE]
  list(
    data = df,
    log  = paste0("Removed near-zero variance column(s): ",
                  paste(dropped, collapse = ", "))
  )
}

# -----------------------------------------------------------------------------
# remove_correlated_features()
# Removes numeric columns with pairwise correlation above threshold
# Returns: list(data, log)
# -----------------------------------------------------------------------------
remove_correlated_features <- function(df, threshold = 0.90) {
  num_df <- df[, vapply(df, is.numeric, logical(1L)), drop = FALSE]
  if (ncol(num_df) < 2) {
    return(list(data = df, log = "Not enough numeric columns for correlation filter"))
  }
  
  cor_mat  <- cor(num_df, use = "pairwise.complete.obs")
  high_cor <- caret::findCorrelation(cor_mat, cutoff = threshold, names = TRUE)
  
  if (length(high_cor) == 0) {
    return(list(data = df, log = paste0("No columns exceed correlation threshold of ", threshold)))
  }
  
  df <- df[, !names(df) %in% high_cor, drop = FALSE]
  list(
    data = df,
    log  = paste0("Removed high-correlation (>", threshold, ") column(s): ",
                  paste(high_cor, collapse = ", "))
  )
}

# -----------------------------------------------------------------------------
# detect_outliers()
# method: "iqr" | "zscore"
# action: "flag" (add column) | "remove"
# Returns: list(data, log)
# -----------------------------------------------------------------------------
detect_outliers <- function(df, vars, method = "iqr", action = "flag") {
  valid <- intersect(vars, names(df)[vapply(df, is.numeric, logical(1L))])
  if (length(valid) == 0) stop("No valid numeric columns for outlier detection.")
  
  is_outlier <- rep(FALSE, nrow(df))
  
  for (v in valid) {
    x <- df[[v]]
    if (method == "iqr") {
      q1  <- quantile(x, 0.25, na.rm = TRUE)
      q3  <- quantile(x, 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      is_outlier <- is_outlier | (!is.na(x) & (x < q1 - 1.5 * iqr | x > q3 + 1.5 * iqr))
    } else if (method == "zscore") {
      z  <- abs(scale(x)[, 1])
      is_outlier <- is_outlier | (!is.na(z) & z > 3)
    }
  }
  
  n_out <- sum(is_outlier)
  
  if (action == "flag") {
    df$.outlier_flag <- is_outlier
    log_msg <- paste0("Flagged ", n_out, " outlier row(s) using ", method,
                      " in: ", paste(valid, collapse = ", "))
  } else {
    df <- df[!is_outlier, , drop = FALSE]
    rownames(df) <- NULL
    log_msg <- paste0("Removed ", n_out, " outlier row(s) using ", method,
                      " in: ", paste(valid, collapse = ", "))
  }
  
  list(data = df, log = log_msg)
}

# -----------------------------------------------------------------------------
# convert_variable_type()
# to_type: "numeric" | "factor" | "character"
# Returns: list(data, log)
# -----------------------------------------------------------------------------
convert_variable_type <- function(df, vars, to_type = "factor") {
  valid <- intersect(vars, names(df))
  if (length(valid) == 0) stop("No valid columns to convert.")
  
  convert_fn <- switch(to_type,
                       numeric   = as.numeric,
                       factor    = as.factor,
                       character = as.character,
                       stop("Unknown type: ", to_type)
  )
  
  df[valid] <- lapply(df[valid], convert_fn)
  list(
    data = df,
    log  = paste0("Converted to ", to_type, ": ", paste(valid, collapse = ", "))
  )
}

# -----------------------------------------------------------------------------
# bin_variable()
# Creates a new factor column with binned intervals
# n_bins: number of equal-width bins (or supply custom breaks)
# Returns: list(data, log)
# -----------------------------------------------------------------------------
bin_variable <- function(df, var, n_bins = 4, breaks = NULL, labels = NULL) {
  if (!var %in% names(df)) stop(paste("Column not found:", var))
  if (!is.numeric(df[[var]])) stop(paste(var, "must be numeric for binning"))
  
  new_col <- paste0(var, "_bin")
  df[[new_col]] <- cut(df[[var]],
                       breaks = if (!is.null(breaks)) breaks else n_bins,
                       labels = labels,
                       include.lowest = TRUE)
  list(
    data = df,
    log  = paste0("Binned '", var, "' into ", n_bins, " bins -> '", new_col, "'")
  )
}

# -----------------------------------------------------------------------------
# parse_indices()  [kept here so utils is self-contained]
# Parses "1,3,5" or "2:4" style strings into integer vectors
# -----------------------------------------------------------------------------
parse_indices <- function(input_str, max_val) {
  parts   <- unlist(strsplit(trimws(input_str), ","))
  indices <- c()
  for (p in parts) {
    p <- trimws(p)
    if (grepl("^\\d+:\\d+$", p)) {
      rng     <- as.integer(unlist(strsplit(p, ":")))
      indices <- c(indices, seq(rng[1], rng[2]))
    } else if (grepl("^\\d+$", p)) {
      indices <- c(indices, as.integer(p))
    }
  }
  unique(indices[indices >= 1 & indices <= max_val])
}
