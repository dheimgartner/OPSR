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
