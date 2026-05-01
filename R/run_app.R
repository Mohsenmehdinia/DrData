#' Launch the DrData Shiny Application
#'
#' @description
#' Opens the DrData interactive platform in your default browser or RStudio
#' Viewer. All required packages are checked before launch; missing ones are
#' listed so you can install them with a single \code{install.packages()} call.
#'
#' @param ... Additional arguments passed to \code{\link[shiny]{runApp}},
#'   for example \code{port} or \code{launch.browser}.
#'
#' @return Invisible \code{NULL}. Called for its side effect.
#'
#' @examples
#' \dontrun{
#'   run_app()
#'   run_app(port = 4321, launch.browser = FALSE)
#' }
#'
#' @export
run_app <- function(...) {

  # ── Check all Suggests that the Shiny app needs ─────────────────────────
  required <- c(
    "shinydashboard", "plotly", "DT", "ggplot2", "dplyr", "tidyr",
    "readr", "readxl", "caret", "randomForest", "rpart", "rpart.plot",
    "e1071", "class", "nnet", "colourpicker", "glmnet", "cluster",
    "dbscan", "GGally", "gbm", "pROC", "reshape2", "scales"
  )
  missing <- required[!vapply(required, requireNamespace,
                              logical(1L), quietly = TRUE)]
  if (length(missing) > 0L) {
    stop(
      "The following packages are required to run DrData but are not installed:\n",
      "  ", paste(missing, collapse = ", "), "\n\n",
      "Install them with:\n",
      '  install.packages(c("', paste(missing, collapse = '", "'), '"))\n',
      call. = FALSE
    )
  }

  app_dir <- system.file("app", package = "DrData")
  if (app_dir == "") {
    stop("Cannot find the app directory. Please reinstall DrData.",
         call. = FALSE)
  }
  shiny::runApp(appDir = app_dir, ...)
}
