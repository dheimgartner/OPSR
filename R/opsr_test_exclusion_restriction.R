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
