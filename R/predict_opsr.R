## see ?predict and ?predict.lm
## implement se.fit with delta method
## maybe rename j and j_star (which is the counterfactual level)

#' @export
predict.opsr <- function(object, newdata, j, j_star = NULL, se.fit = FALSE, ...) {
  predict_opsr <- function(X_j, W_j, beta_j, rho_j, sigma_j, kappa_j_1, kappa_j, gamma) {
    Xb <- X_j %*% beta_j
    Wg <- W_j %*% gamma
    kappa_j_Wg <- kappa_j - Wg
    kappa_j_1_Wg <- kappa_j_1 - Wg
    nom <- dnorm(kappa_j_Wg) - dnorm(kappa_j_1_Wg)
    denom <- pnorm(kappa_j_Wg) - pnorm(kappa_j_1_Wg)
    imr <- nom / denom  # (negative) inverse mills ratio
    Xb - (rho_j / sigma_j) * imr
  }

  ## prepare inputs for function from args
  newdata <- if (missing(newdata) || is.null(newdata)) NULL
  X <- opsr_model_matrices(object, j, newdata)
  X_j <- X$X_j
  W_j <- X$W_j
  coefs_j <- opsr_prepare_coefs(coefficients(object), nReg = object$nReg)[[j]]

  if (!is.null(j_star)) {
    X_star <- opsr_model_matrices(object, j_star, newdata, z = j)
    coefs_j_star <- opsr_prepare_coefs(coefficients(object), nReg = object$nReg)[[j_star]]
    ## overwrite
    X_j <- X_star$X_j
    W_j <- X_star$W_j
    coefs_j[["beta_j"]] <- coefs_j_star[["beta_j"]]
    coefs_j[["rho_j"]] <- coefs_j_star[["rho_j"]]
    coefs_j[["sigma_j"]] <- coefs_j_star[["sigma_j"]]
  }

  ## is kappa_j_1 -Inf and kappa_j Inf a problem => don't think so
  p <- predict_opsr(X$X_j, X$W_j, coefs_j[["beta_j"]], coefs_j[["rho_j"]],
                    coefs_j[["sigma_j"]], coefs_j[["kappa_j_1"]], coefs_j[["kappa_j"]],
                    coefs_j[["gamma"]])

  ## maybe return object (opsr_predict)
  p
}
