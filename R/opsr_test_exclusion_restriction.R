#' LR Test for Instrument Exclusion from Selection Equation
#'
#' Re-fits a restricted model with one or more selection-equation variables
#' fixed at zero and compares it to the full model via [`anova.opsr`].
#' A small LR statistic (or large p-value) means the instrument contributes
#' little information to the selection equation — a sign of weak identification.
#'
#' @param object an object of class `"opsr"`.
#' @param instrument character vector naming the selection-equation variable(s)
#'   to exclude.  Must match variable names as they appear after the `s_`
#'   prefix in `coef(object)` (i.e., without the prefix).
#' @param printLevel integer passed to [`opsr`] when refitting the restricted
#'   model.  Defaults to `0` (silent).
#' @param ... further arguments passed to [`opsr`] when refitting.
#'
#' @return An object of class `"anova.opsr"` as returned by [`anova.opsr`],
#'   comparing the restricted model (instrument fixed at zero) to the full
#'   model.
#'
#' @seealso [`anova.opsr`], [`opsr_test_exclusion_restriction`]
#' @export
opsr_lr_instrument <- function(object, instrument, printLevel = 0, ...) {
  start <- object$estimate
  nm    <- names(start)

  inst_nm <- paste0("s_", instrument)
  missing_inst <- setdiff(inst_nm, nm)
  if (length(missing_inst) > 0) {
    avail <- gsub("^s_", "", nm[grepl("^s_", nm)])
    stop("instrument variable(s) not found in selection equation: ",
         paste(gsub("^s_", "", missing_inst), collapse = ", "),
         "\nAvailable selection variables: ", paste(avail, collapse = ", "))
  }

  start[inst_nm] <- 0
  fixed <- setNames(nm %in% inst_nm, nm)

  dat <- opsr_get_all_vars(object)
  f   <- object$formula
  environment(f) <- environment()

  x <- utils::capture.output(
    fit_restricted <- opsr(f, dat, start = start, fixed = fixed,
                           printLevel = printLevel, ...)
  )

  result <- anova.opsr(fit_restricted, object)
  result$restriction <- list(row = 1L, instrument = instrument)
  result
}


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


#' Test Exclusion Restriction in OPSR Models
#'
#' Combines two diagnostics for assessing whether the exclusion restriction
#' (instrumental variable) underpinning an [`opsr`] model is credible:
#'
#' 1. **LR test** ([`opsr_lr_instrument`]): drops the instrument from the
#'    selection equation by fixing its coefficient at zero and compares the
#'    restricted to the full model.  A small LR statistic signals a weak
#'    instrument.
#'
#' 2. **Profile likelihood** ([`opsr_profile_rho`]): fixes each \eqn{\rho_j}
#'    on a grid and re-maximises over all other parameters.  A flat profile
#'    indicates that the data do not identify \eqn{\rho_j} independently of
#'    functional-form assumptions.
#'
#' @param object an object of class `"opsr"`.
#' @param instrument character vector naming the excluded selection-equation
#'   variable(s).  See [`opsr_lr_instrument`] for details.
#' @param grid numeric vector of \eqn{\rho} values for the profile likelihood.
#'   See [`opsr_profile_rho`] for details.
#' @param ... further arguments passed to [`opsr`] when refitting models.
#'
#' @return An object of class `"opsr.exclusion.test"`, a list with components:
#' \item{lr_test}{An `"anova.opsr"` object from [`opsr_lr_instrument`].}
#' \item{profile_rho}{An `"opsr.profile.rho"` data frame from [`opsr_profile_rho`].}
#' \item{instrument}{The instrument name(s) supplied by the user.}
#'
#' @seealso [`opsr_lr_instrument`], [`opsr_profile_rho`],
#'   [`print.opsr.exclusion.test`], [`plot.opsr.profile.rho`]
#' @example R/examples/ex-opsr_test_exclusion_restriction.R
#' @export
opsr_test_exclusion_restriction <- function(object, instrument,
                                             grid = seq(-0.99, 0.99, length.out = 21),
                                             ...) {
  lr      <- opsr_lr_instrument(object, instrument, ...)
  profile <- opsr_profile_rho(object, grid = grid, ...)

  out <- list(
    lr_test     = lr,
    profile_rho = profile,
    instrument  = instrument
  )
  class(out) <- "opsr.exclusion.test"
  out
}
