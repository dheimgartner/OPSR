#' Print Method for Exclusion Restriction Test
#'
#' @param x an object of class `"opsr.exclusion.test"`.
#' @param ... further arguments passed to [`print.anova.opsr`] and
#'   [`print.opsr.profile.rho`].
#'
#' @return Prints a two-section report and returns `x` invisibly.
#' @method print opsr.exclusion.test
#' @seealso [`opsr_test_exclusion_restriction`]
#' @export
print.opsr.exclusion.test <- function(x, ...) {
  cat("Exclusion restriction diagnostics\n")
  cat("Instrument(s):", paste(x$instrument, collapse = ", "), "\n\n")

  cat("--- 1. LR test: instrument in selection equation ---\n")
  print(x$lr_test, ...)

  cat("\n--- 2. Profile likelihood for rho ---\n")
  print(x$profile_rho, ...)

  invisible(x)
}
