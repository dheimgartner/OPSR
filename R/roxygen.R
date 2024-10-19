## Sources from DESCRIPTION
#' @name OPSR-package
"_PACKAGE"

## I think this exportPattern exports all functions (maybe remove it...)

#' @useDynLib OPSR, .registration=TRUE
#' @importFrom Rcpp evalCpp setRcppClass
#' @exportPattern "^[[:alpha:]]+"
NULL

## Generics outside OPSR
#' @importFrom stats AIC BIC coef coefficients fitted model.frame model.matrix
#'   model.weights predict residuals update nobs
NULL

#' @importFrom methods setMethod
NULL

## RcppArmadillo listed in imports but never explicitly used
silence_r_cmd_check <- function() {
  RcppArmadillo::armadillo_get_number_of_omp_threads()  # just some random function
}
