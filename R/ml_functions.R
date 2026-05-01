#' Split a data frame into training and test sets
#'
#' @param data A \code{data.frame} to split.
#' @param train_ratio Numeric in (0,1); proportion for training. Default 0.8.
#' @param seed Integer random seed. Default 42.
#' @return Named list with \code{train} and \code{test} data frames.
#' @examples
#' splits <- ml_split(mtcars, train_ratio = 0.75, seed = 1)
#' nrow(splits$train)
#' @export
ml_split <- function(data, train_ratio = 0.8, seed = 42) {
  stopifnot(is.data.frame(data))
  stopifnot(is.numeric(train_ratio), train_ratio > 0, train_ratio < 1)
  set.seed(seed)
  n   <- nrow(data)
  idx <- sample(seq_len(n), size = floor(train_ratio * n))
  list(train = data[idx, , drop = FALSE],
       test  = data[-idx, , drop = FALSE])
}

#' Prepare a data frame for machine learning
#'
#' @param data A \code{data.frame}.
#' @param target Single character string: the response column name.
#' @param features Character vector of predictor names. Default: all except target.
#' @return Named list: \code{data}, \code{target}, \code{features}.
#' @examples
#' prep <- ml_prepare_data(mtcars, target = "mpg")
#' names(prep)
#' @export
ml_prepare_data <- function(data, target, features = NULL) {
  stopifnot(is.data.frame(data))
  stopifnot(is.character(target), length(target) == 1L, target %in% names(data))
  if (is.null(features)) features <- setdiff(names(data), target)
  bad <- setdiff(features, names(data))
  if (length(bad) > 0L)
    stop("Columns not found: ", paste(bad, collapse = ", "), call. = FALSE)
  cols  <- c(target, features)
  clean <- data[stats::complete.cases(data[, cols, drop = FALSE]),
                cols, drop = FALSE]
  list(data = clean, target = target, features = features)
}

#' Compute regression performance metrics
#'
#' @param y_true Numeric vector of observed values.
#' @param y_pred Numeric vector of predicted values.
#' @return One-row \code{data.frame} with columns \code{RMSE}, \code{MAE}, \code{R2}.
#' @examples
#' ml_metrics_regression(c(1,2,3,4,5), c(1.1,1.9,3.2,3.8,5.1))
#' @export
ml_metrics_regression <- function(y_true, y_pred) {
  stopifnot(is.numeric(y_true), is.numeric(y_pred))
  stopifnot(length(y_true) == length(y_pred))
  if (anyNA(y_true) || anyNA(y_pred)) {
    warning("NA values ignored.", call. = FALSE)
    ok    <- !is.na(y_true) & !is.na(y_pred)
    y_true <- y_true[ok]; y_pred <- y_pred[ok]
  }
  rmse   <- sqrt(mean((y_true - y_pred)^2))
  mae    <- mean(abs(y_true - y_pred))
  ss_res <- sum((y_true - y_pred)^2)
  ss_tot <- sum((y_true - mean(y_true))^2)
  r2     <- if (ss_tot == 0) NA_real_ else 1 - ss_res / ss_tot
  data.frame(RMSE = rmse, MAE = mae, R2 = r2)
}

#' Build a model formula with optional interaction terms
#'
#' @param target Single character: response variable name.
#' @param features Character vector of predictor names.
#' @param use_interactions Logical; add two-way interactions? Default FALSE.
#' @param interaction_vars Character vector of variables to interact.
#' @return A \code{\link[stats]{formula}} object.
#' @examples
#' build_model_formula("mpg", c("cyl", "hp", "wt"))
#' build_model_formula("mpg", c("cyl","hp","wt"), TRUE, c("cyl","hp"))
#' @export
build_model_formula <- function(target, features,
                                use_interactions = FALSE,
                                interaction_vars = NULL) {
  stopifnot(is.character(target), length(target) == 1L)
  stopifnot(is.character(features), length(features) >= 1L)
  rhs <- paste(features, collapse = " + ")
  if (isTRUE(use_interactions) && length(interaction_vars) >= 2L) {
    pairs <- utils::combn(interaction_vars, 2L, simplify = FALSE)
    terms <- vapply(pairs, function(p) paste(p, collapse = ":"), character(1L))
    rhs   <- paste(c(rhs, terms), collapse = " + ")
  }
  stats::as.formula(paste(target, "~", rhs))
}
