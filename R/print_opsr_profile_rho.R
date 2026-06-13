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
    sub <- x[x$regime == j, ]

    cat(sprintf("Regime %d  (rho_hat = %s):\n", j,
                format(rho_hat[j], digits = digits)))

    if (all(is.na(sub$profile_loglik))) {
      cat("  [WARNING] All profile evaluations failed -- no profile available\n")
      next
    }

    ll_range <- range(sub$profile_loglik, na.rm = TRUE)
    drop     <- max_ll - ll_range[1]

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
