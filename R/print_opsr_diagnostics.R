#' Print Method for OPSR Convergence Diagnostics
#'
#' @param x an object of class `"opsr.diagnostics"`.
#' @param digits minimum number of significant digits for numeric output.
#' @param print.gradient if `TRUE`, prints the full gradient vector.
#' @param print.eigenvalues if `TRUE`, prints all eigenvalues of the Hessian.
#' @param .print_title flag to disable printing when called from [`print.summary.opsr`].
#' @param ... further arguments passed to or from other methods.
#'
#' @return Prints diagnostics and returns `x` invisibly.
#'
#' @method print opsr.diagnostics
#'
#' @seealso [`opsr_diagnostics`]
#' @export
print.opsr.diagnostics <- function(x, digits = max(3L, getOption("digits") - 3L),
                                   print.gradient = FALSE,
                                   print.eigenvalues = FALSE,
                                   .print.title = TRUE, ...) {
  flag <- function(ok) if (ok) "[OK]" else "[WARNING]"

  grad_ok <- x$max_abs_gradient < 1e-3
  if (.print.title) {
    cat("Convergence diagnostics\n\n")
  }
  cat("Max. absolute gradient: ",
      format(x$max_abs_gradient, digits = digits, scientific = TRUE),
      " ", flag(grad_ok), "\n", sep = "")
  cat("Hessian negative definite: ", x$hessian_neg_def,
      " ", flag(x$hessian_neg_def), "\n", sep = "")
  cat("Condition number (Hessian):",
      format(x$condition_number, digits = digits), "\n")

  if (print.eigenvalues) {
    cat("\nHessian eigenvalues:\n")
    ev_mat <- matrix(x$eigenvalues, nrow = 1)
    colnames(ev_mat) <- seq_along(x$eigenvalues)
    rownames(ev_mat) <- ""
    print(ev_mat, digits = digits)
  }

  if (print.gradient) {
    cat("\nGradient:\n")
    print(x$gradient, digits = digits)
  }

  invisible(x)
}
