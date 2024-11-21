#' Null Model for OPSR Model fits
#'
#' Intercept-only model with no error correlation.
#'
#' @param object an object of class `"opsr"`.
#' @param ... further arguments passed to [`opsr`].
#'
#' @return An object of class `"opsr.null" "opsr"`.
#'
#' @example R/examples/ex-opsr_null_model.R
#' @export
opsr_null_model <- function(object, ...) {
  # pattern <- "^kappa|^sigma|^rho|(Intercept)"  !! not identified
  pattern <- "^kappa|^sigma|(Intercept)"  # separate models

  start <- object$estimate
  nm <- names(start)
  start[!grepl(pattern, nm)] <- 0
  nInter <- sum(grepl("(Intercept)", nm))
  if (nInter != object$nReg) {
    stop("Intercept needs to be included for all regimes!")
  }
  fixed <- sapply(nm, function(x) !grepl(pattern, x))

  browser()
  debugonce(opsr)
  dat <- model.frame(object)




  fit_null <- opsr(object$formula, dat, start = start, fixed = fixed, ...)
  class(fit_null) <- c("opsr.null", class(object))
  fit_null
}

#' @export
summary.opsr.null <- function(object, ...) {
  ms <- NextMethod("summary", object)  # opsr
  ms$coef_table[object$fixed, "Estimate"] <- NA_real_
  ms$formula <- ~Nullmodel
  class(ms) <- c("summary.opsr.null", class(ms))
  ms
}

#' @export
print.summary.opsr.null <- function(x, ...) {
  NextMethod("print", x)
  invisible(x)
}
