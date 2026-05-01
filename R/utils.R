# ============================================================
# DrData — internal utilities
# ============================================================

#' @keywords internal
#' @noRd
`%||%` <- function(x, y) if (!is.null(x) && length(x) > 0) x else y
