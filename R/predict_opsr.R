## see ?predict and ?predict.lm
## implement se.fit with delta method

#' @export
predict.opsr <- function(object, newdata, j, se.fit = FALSE, ...) {
  browser()

  ## goal: prepare inputs for this function from args above
  predict_opsr <- function(X_j, W, beta_j, rho_j, sigma_j, kappa_j_1, kappa_j, gamma) {
    Xb <- X_j %*% beta_j
    Wg <- W %*% gamma
    kappa_j_Wg <- kappa_j - Wg
    kappa_j_1_Wg <- kappa_j_1 - Wg
    nom <- dnorm(kappa_j_Wg) - dnorm(kappa_j_1_Wg)
    denom <- pnorm(kappa_j_Wg) - pnorm(kappa_j_1_Wg)
    imr <- nom / denom  # (negative) inverse mills ratio
    Xb - (rho_j / sigma_j) * imr
  }

  ## PROCEED DIM MISSMATCH ##
  ## in last line of predict_opsr => W_j? => potentially adjust opsr_model_matrices

  newdata <- if (missing(newdata) || is.null(newdata)) NULL
  X <- opsr_model_matrices(object, j, newdata)

  coefs_j <- opsr_prepare_coefs(coefficients(object), nReg = object$nReg)[[j]]
  ## is kappa_j_1 -Inf and kappa_j Inf a problem => don't think so
  p <- predict_opsr(X$X_j, X$W, coefs_j[["beta_j"]], coefs_j[["rho_j"]],
                    coefs_j[["sigma_j"]], coefs_j[["kappa_j_1"]], coefs_j[["kappa_j"]],
                    coefs_j[["gamma"]])

}
