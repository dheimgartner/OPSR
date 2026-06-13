#' Profile Likelihood for the Rho Parameters
#'
#' For each regime \eqn{j}, evaluates the profile log-likelihood by fixing
#' \eqn{\rho_j} on a grid of values and re-maximising over all remaining
#' parameters.  A flat or bimodal profile indicates that the data do not
#' strongly identify \eqn{\rho_j}, implying that the Heckman correction may
#' rely on functional-form assumptions (normality) rather than a genuine
#' exclusion restriction.
#'
#' @param object an object of class `"opsr"`.
#' @param grid numeric vector of \eqn{\rho} values in \eqn{(-1, 1)} at which
#'   to evaluate the profile.  Defaults to 21 equally-spaced points.
#' @param printLevel integer passed to [`opsr`] for each profile evaluation.
#'   Defaults to `0` (silent).
#' @param ... further arguments passed to [`opsr`] for each profile evaluation.
#'
#' @return A `data.frame` of class `"opsr.profile.rho"` with columns
#'   `regime` (integer), `rho` (fixed value), and `profile_loglik`
#'   (maximised log-likelihood with \eqn{\rho_j} fixed).  Failed evaluations
#'   are recorded as `NA`.  Attributes `rho_hat` and `max_loglik` store the
#'   MLE estimates and the unrestricted maximum for reference.
#'
#' @seealso [`opsr_test_exclusion_restriction`], [`print.opsr.profile.rho`],
#'   [`plot.opsr.profile.rho`]
#' @export
opsr_profile_rho <- function(object,
                              grid = seq(-0.99, 0.99, length.out = 21),
                              printLevel = 0,
                              ...) {
  nReg    <- object$nReg
  nm      <- names(object$estimate)
  rho_nms <- paste0("rho", seq_len(nReg))

  dat <- opsr_get_all_vars(object)
  f   <- object$formula
  environment(f) <- environment()

  profile_one <- function(j, rho_val) {
    start          <- object$estimate
    start[rho_nms[j]] <- rho_val
    fixed          <- setNames(nm %in% rho_nms[j], nm)

    x <- utils::capture.output(
      fit_j <- opsr(f, dat, start = start, fixed = fixed,
                    printLevel = printLevel, ...)
    )
    fit_j$maximum
  }

  message("Computing profile likelihood for rho (", nReg, " regime(s) x ",
          length(grid), " grid points) ...")

  results <- lapply(seq_len(nReg), function(j) {
    ll_vals <- vapply(grid, function(rho_val) {
      tryCatch(profile_one(j, rho_val), error = function(e) NA_real_)
    }, numeric(1))
    data.frame(regime = j, rho = grid, profile_loglik = ll_vals)
  })

  out <- do.call(rbind, results)
  rownames(out) <- NULL
  attr(out, "rho_hat")   <- stats::setNames(object$estimate[rho_nms], rho_nms)
  attr(out, "max_loglik") <- object$maximum
  class(out) <- c("opsr.profile.rho", "data.frame")
  out
}
