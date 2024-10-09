#' @export
print.summary.opsr <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  ## output formatting for wald tests
  cat_wald <- function(wald, note) {
    r <- function(x) round(x, digits = digits)
    cat("Wald chi2 (", note, "): ", r(wald$chisq), " on ", wald$df, " DF, ",
        "p-value: < ", r(wald$pval), "\n", sep = "")
  }

  with(x, {
    cat("--------------------------------------------\n")
    cat("Ordinal probit switching regression\n")
    cat(maxim_type, ", ", iterations, " iterations\n", sep = "")
    cat("Return code ", return_code, ": ", message, "\n", sep = "")
    cat("Runtime: ", format(unclass(runtime), digits = 3), " ", attr(runtime, "units"), "\n", sep = "")
    cat("Log-Likelihood:", GOF$LL_final, "\n")
    cat("AIC:", GOF$AIC, "\n")
    cat("BIC:", GOF$BIC, "\n")
    cat("Number of regimes:", n_regimes, "\n")
    cat("Number of observations: "); cat(n_obs[1], "("); cat(n_obs[-1], sep = ", "); cat(")\n")

    cat("Estimated parameters:", n_params, "\n")
    cat("Estimates:\n")
    stats::printCoefmat(coef_table, digits = digits)
    cat("\n")
    cat_wald(wald_test$null, "null")
    cat_wald(wald_test$rho, "rho")
    cat("--------------------------------------------\n")
  })
  invisible(x)
}
