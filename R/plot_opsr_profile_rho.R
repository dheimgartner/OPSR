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
