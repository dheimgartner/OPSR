#' Fitter function for ordinal probit switching regression model
#'
#' This is the basic computing engine called by [`opsr`] used to fit ordinal
#' probit switching regression models. Should usually *not* be used directly.
#' Could be implemented in C++ for speed-up.
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
#' @seealso Formula 6 (translates almost verbatim).
opsr.fit <- function(Ws, Xs, Ys, start, weights,
                     method, iterlim, printLevel, ...) {
  nReg <- length(Xs)

  ## formula 6 in paper
  ## boundary: -1 for min(Z) and 1 for max(Z)
  loglik_j <- function(W_j, X_j, y_j, gamma, kappa_j_1, kappa_j, beta_j, sigma_j, rho_j, boundary) {
    ll_j <- numeric(length = length(y_j))
    for (i in seq_along(y_j)) {
      res <- y_j[i] - X_j[i, ] %*% beta_j
      low <- kappa_j_1 - W_j[i, ] %*% gamma
      high <- kappa_j - W_j[i, ] %*% gamma
      part1 <- 1 / sigma_j * dnorm(res / sigma_j)
      part2 <- if (boundary == 1) 1 else pnorm((sigma_j * high - rho_j * res) / (sigma_j * sqrt(1 - rho_j^2)))
      part3 <- if (boundary == -1) 0 else pnorm((sigma_j * low - rho_j * res) / (sigma_j * sqrt(1 - rho_j^2)))
      ll_j[i] <- log(part1) + log(part2 - part3)
    }
    ll_j
  }

  min_z <- 1  # checked in opsr(): outcome must be ordered starting from 1 in increasing fashion
  max_z <- nReg

  loglik <- function(theta) {
    ## do as little as possible in here (gets called many times)
    theta_ <- opsr_prepare_coefs(theta, nReg)
    ll <- vector(mode = "list", length = nReg)
    for (i in seq_len(nReg)) {
      theta_j <- theta_[[i]]
      boundary <- if (i == min_z) -1 else if (i == max_z) 1 else 0
      ll[[i]] <- loglik_j(Ws[[i]], Xs[[i]], Ys[[i]], theta_j[["gamma"]],
                          theta_j[["kappa_j_1"]], theta_j[["kappa_j"]],
                          theta_j[["beta_j"]], theta_j[["sigma_j"]],
                          theta_j[["rho_j"]], boundary)
    }
    ll <- unlist(ll)  # vector of likelihood contributions (at obs level)
    ll <- ll %*% weights  # weights was ordered in opsr() (sum)
    ll
  }

  fit <- maxLik::maxLik(loglik, start = start, method = method, iterlim = iterlim,
                        printLevel = printLevel, ...)

  fit
}
