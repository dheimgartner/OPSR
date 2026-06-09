#' Convergence Diagnostics for OPSR Models
#'
#' Evaluates the standard second-order optimality conditions at the parameter
#' estimates returned by [`opsr`]: the gradient should be near zero and the
#' Hessian of the log-likelihood should be negative definite (all eigenvalues
#' strictly negative), confirming a local maximum rather than a saddle point
#' or other degenerate solution.
#'
#' @param object an object of class `"opsr"`.
#'
#' @return An object of class `"opsr.diagnostics"`, a list with components:
#' \item{max_abs_gradient}{Maximum absolute value of the gradient at convergence.
#'   Should be close to zero.}
#' \item{gradient}{Full gradient vector at convergence.}
#' \item{hessian_neg_def}{Logical; `TRUE` if the Hessian is negative definite
#'   (all eigenvalues strictly negative), which is the required second-order
#'   condition for a local maximum.}
#' \item{eigenvalues}{Eigenvalues of the Hessian matrix, sorted in decreasing
#'   order. All should be negative at a proper local maximum.}
#' \item{condition_number}{Ratio of the largest to smallest absolute eigenvalue
#'   of the Hessian. Large values indicate near-singularity and numerical
#'   instability.}
#'
#' @seealso [`summary.opsr`], [`opsr`]
#'
#' @export
opsr_diagnostics <- function(object) {
  g <- object$gradient
  max_abs_gradient <- max(abs(g))

  H <- object$hessian
  ev <- eigen(H, symmetric = TRUE, only.values = TRUE)$values
  hessian_neg_def <- all(ev < 0)
  condition_number <- max(abs(ev)) / min(abs(ev))

  out <- list(
    max_abs_gradient = max_abs_gradient,
    gradient = g,
    hessian_neg_def = hessian_neg_def,
    eigenvalues = ev,
    condition_number = condition_number
  )
  class(out) <- "opsr.diagnostics"
  out
}

#' Print Method for OPSR Convergence Diagnostics
#'
#' @param x an object of class `"opsr.diagnostics"`.
#' @param digits minimum number of significant digits for numeric output.
#' @param print.gradient if `TRUE`, prints the full gradient vector.
#' @param print.eigenvalues if `TRUE`, prints all eigenvalues of the Hessian.
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
                                   print.eigenvalues = TRUE, ...) {
  flag <- function(ok) if (ok) "[OK]" else "[WARNING]"

  grad_ok <- x$max_abs_gradient < 1e-3
  cat("Convergence diagnostics (second-order conditions):\n\n")
  cat("  Max. absolute gradient:    ",
      format(x$max_abs_gradient, digits = digits, scientific = TRUE),
      " ", flag(grad_ok), "\n", sep = "")
  cat("  Hessian negative definite: ", x$hessian_neg_def,
      " ", flag(x$hessian_neg_def), "\n", sep = "")
  cat("  Condition number (Hessian):",
      format(x$condition_number, digits = digits, big.mark = ","), "\n")

  if (print.eigenvalues) {
    cat("\n  Hessian eigenvalues:\n")
    ev_mat <- matrix(x$eigenvalues, nrow = 1)
    colnames(ev_mat) <- seq_along(x$eigenvalues)
    rownames(ev_mat) <- ""
    print(ev_mat, digits = digits)
  }

  if (print.gradient) {
    cat("\n  Gradient:\n")
    print(x$gradient, digits = digits)
  }

  invisible(x)
}
