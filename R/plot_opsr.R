#' Plot Method for OPSR Model Fits
#'
#' Wrapper around [`pairs.opsr.te`].
#'
#' @param x an object of class `"opsr"`.
#' @inheritParams opsr_te
#' @param ... further arguments passed to [`pairs.opsr.te`].
#'
#' @returns Returns `x` invisibly.
#' @seealso [`opsr_te`], [`pairs.opsr.te`]
#'
#' @method plot opsr
#' @export
plot.opsr <- function(x, type, weights = NULL, ...) {
  te <- opsr_te(x, type = type, weights = weights)
  pairs(te, ...)
  invisible(x)
}
