#' Fitter function for ordinal probit switching regression model
#'
#' This is the basic computing engine called by [`opsr`] used to fit ordinal
#' probit switching regression models. Should usually *not* be used directly.
#' Log-likelihood function is implemented in C++ which yields a considerable
#' speed-up. However, gradient and Hessian is currently numerically determined.
#'
#' @param Ws
#' @param Xs
#' @param Ys
#' @param start
#' @param weights a vector of weights to be used in the fitting process. Has to
#'   conform with order (`w <- weights[order(Z)]`, where Z is the ordinal
#'   outcome).
#' @param method
#' @param iterlim
#' @param printLevel
#' @param ...
#'
#' @return object of class `"maxLik" "maxim"`.
#' @export
#'
#' @seealso ['loglik'], Formula 6 (translates almost verbatim).
opsr.fit <- function(Ws, Xs, Ys, start, weights,
                     method, iterlim, printLevel, ...) {
  nReg <- length(Xs)
  nObs <- length(Reduce(c, Ys))

  ll2 <- function(theta) loglik(theta, Ws, Xs, Ys, weights, nReg, nObs)

  mL <- maxLik::maxLik(ll2, start = start, method = method, iterlim = iterlim,
                       printLevel = printLevel, ...)

  mL
}
