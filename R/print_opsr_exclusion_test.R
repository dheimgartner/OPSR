#' Print Method for Profile Likelihood of Rho
#'
#' @param x an object of class `"opsr.profile.rho"`.
#' @param digits minimum number of significant digits.
#' @param ... further arguments (currently unused).
#'
#' @return Prints a per-regime summary and returns `x` invisibly.
#' @method print opsr.profile.rho
#' @seealso [`opsr_profile_rho`]
#' @export
print.opsr.profile.rho <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  rho_hat  <- attr(x, "rho_hat")
  max_ll   <- attr(x, "max_loglik")
  nReg     <- max(x$regime)
  flat_tol <- 2  # LL range below this (in natural log units) is flagged as flat

  cat("Profile log-likelihood for rho\n\n")

  for (j in seq_len(nReg)) {
    sub      <- x[x$regime == j, ]
    ll_range <- range(sub$profile_loglik, na.rm = TRUE)
    drop     <- max_ll - ll_range[1]

    cat(sprintf("Regime %d  (rho_hat = %s):\n", j,
                format(rho_hat[j], digits = digits)))
    cat(sprintf("  Profile LL range: [%s, %s]  |  drop from MLE: %s\n",
                format(ll_range[1], digits = digits),
                format(ll_range[2], digits = digits),
                format(drop, digits = digits)))

    if (diff(ll_range) < flat_tol) {
      cat("  [WARNING] Flat profile -- rho may be poorly identified\n")
    }
  }

  cat("\nUse plot() to visualise the profile.\n")
  invisible(x)
}


#' Plot Profile Likelihood for Rho
#'
#' Draws one panel per regime showing the profile log-likelihood as a function
#' of \eqn{\rho}, with a vertical dashed line at the MLE estimate.
#'
#' @param x an object of class `"opsr.profile.rho"`.
#' @param ... further graphical arguments passed to [`graphics::plot`].
#'
#' @return Returns `x` invisibly.
#' @method plot opsr.profile.rho
#' @seealso [`opsr_profile_rho`]
#' @export
plot.opsr.profile.rho <- function(x, ...) {
  rho_hat <- attr(x, "rho_hat")
  nReg    <- max(x$regime)

  op <- graphics::par(mfrow = c(1, nReg), mar = c(4, 4, 3, 1))
  on.exit(graphics::par(op))

  for (j in seq_len(nReg)) {
    sub <- x[x$regime == j, ]
    graphics::plot(sub$rho, sub$profile_loglik, type = "l",
                   xlab = expression(rho),
                   ylab = "Profile log-likelihood",
                   main = paste0("Regime ", j), ...)
    graphics::abline(v = rho_hat[j], lty = 2, col = "gray40")
  }
  invisible(x)
}


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
