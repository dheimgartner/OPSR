## returns class opsr.null (wald tests not meaningful in summary.opsr)
opsr_null_model <- function(object) {
  pattern <- "^kappa|^sigma|^rho|(Intercept)"
  pattern <- "^kappa|^sigma|(Intercept)"

  warning("Which pattern should we use? With or without rho? If without =>",
          " Intercepts match conditional mean... and no identification issues.",
          " I think it should be consistent with Wald test in 'summary.opsr'")

  start <- object$estimate
  nm <- names(start)
  start[!grepl(pattern, nm)] <- 0
  nInter <- sum(grepl("(Intercept)", nm))
  if (nInter != object$nReg) {
    stop("Intercept needs to be included for all regimes!")
  }
  fixed <- sapply(nm, function(x) !grepl(pattern, x))
  dat <- model.frame(object)
  fit_null <- opsr(object$formula, dat, start = start, fixed = fixed)
  class(fit_null) <- c("opsr.null", "maxLik", "maxim")
  fit_null
}


#' @export
summary.opsr.null <- function(object, ...) {
  ms <- NextMethod("summary", object)
  class(ms) <- c("summary.opsr.null", class(ms))
  ms
}


#' @export
print.summary.opsr.null <- function(x, ...) {
  ps <- capture.output(NextMethod("print", x))
  ps[2] <- "Ordinal probit switching regression (null model)"
  cat(ps, sep = "\n")
  invisible(x)
}
