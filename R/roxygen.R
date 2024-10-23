## Sources from DESCRIPTION
#' @name OPSR-package
"_PACKAGE"

## Generics outside OPSR
#' @importFrom stats AIC BIC coef coefficients fitted model.frame model.matrix
#'   model.weights predict residuals update nobs
NULL

#' @importFrom methods setMethod
NULL

#' @importFrom texreg extract
NULL

## RcppArmadillo listed in imports but never explicitly used
silence_r_cmd_check <- function() {
  RcppArmadillo::armadillo_get_number_of_omp_threads()  # just some random function
}

## C++
#' @useDynLib OPSR, .registration=TRUE
#' @importFrom Rcpp evalCpp setRcppClass
NULL

#' Interface to C++ Log-Likelihood Implementation
#'
#' This is the main computation engine wrapped by [`opsr.fit`].
#'
#' @param theta named coefficient vector as parsed from formula interface [`opsr`].
#' @param W list of matrices with explanatory variables for selection process for each regime.
#' @param X list of matrices with expalanatory varialbes for outcome process for each regime.
#' @param Y list of vectors with continuous outcomes for each regime.
#' @param weights vector of weights. See also [`opsr`].
#' @param nReg integer number of regimes.
#' @param nThreads number of threads to be used by `OpenMP` (should be max. `nReg`).
#'
#' @return Numeric vector of (weighted) log-likelihood contributions.
#' @usage loglik_cpp(theta, W, X, Y, weights, nReg, nThreads)
#' @name loglik_cpp
#'
#' @seealso [`opsr.fit`], [`loglik_R`]
#' @export
NULL

#' Prepares Coefficients for Likelihood Function
#'
#' Extracts the coefficients for each regime
#'
#' @param theta named coefficient vector as parsed from formula interface [`opsr`].
#' @param nReg integer number of regimes.
#'
#' @return Named list of length `nReg`
#' @usage opsr_prepare_coefs(theta, nReg)
#' @name opsr_prepare_coefs
#'
#' @example R/examples/ex-opsr_prepare_coefs.R
#' @export
NULL

#' Check Whether OpenMP is Available
#'
#' @return boolean
#' @usage opsr_check_omp()
#' @name opsr_check_omp
#'
#' @export
NULL

#' Check Maximum Number of Threads Available
#'
#' @return integer
#' @usage opsr_max_threads()
#' @name opsr_max_threads
#'
#' @seealso [`opsr_check_omp`]
#' @export
NULL

## Data
## see here for documentation example: https://github.com/tidyverse/nycflights13/blob/main/R/flights.R

#' Telework data
#'
#' @source tbc
#'
#' @format Data frame with columns
#' \describe{
#' \item{variable1}{Description.}
#' \item{variable2}{Description.}
#' }
"telework_data"
