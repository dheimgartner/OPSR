#' Predict method for OPSR model fits
#'
#' Computes the conditional expectation \eqn{E[y_j | Z = j]} or
#' counterfactual conditional expectation \eqn{E[y_{j'} | Z = j]}.
#'
#' @section Outlook:
#' Could be extended with an argument `se.fit`, computing the standard errors
#' of the predictions with the delta method.
#'
#' @param object an object of class `"opsr"`.
#' @param newdata an optional data frame in which to look for variables used in
#'   `object$formula`. If omitted, `environment(object$formula)` is used. See
#'   also [`model.matrix.opsr`].
#' @param group predict outcome of this group.
#' @param counterfact counterfactual group.
#'
#' @export
predict.opsr <- function(object, newdata, group, counterfact = NULL, ...) {
  predict_opsr <- function(X_j, W_j, beta_j, rho_j, sigma_j, kappa_j_1, kappa_j, gamma) {
    Xb <- X_j %*% beta_j
    Wg <- W_j %*% gamma
    kappa_j_Wg <- kappa_j - Wg
    kappa_j_1_Wg <- kappa_j_1 - Wg
    nom <- dnorm(kappa_j_Wg) - dnorm(kappa_j_1_Wg)
    denom <- pnorm(kappa_j_Wg) - pnorm(kappa_j_1_Wg)
    imr <- nom / denom  # (negative) inverse mills ratio
    as.vector(Xb - (rho_j / sigma_j) * imr)
  }

  ## prepare inputs for function from args
  mm <- model.matrix(object, data = newdata)
  X <- mm$X[[group]]
  W <- mm$W[[group]]
  coefs_j <- opsr_prepare_coefs(coefficients(object), nReg = object$nReg)[[group]]

  if (!is.null(counterfact)) {
    mm <- model.matrix(object, data = newdata, filter = group)
    coefs_j_<- opsr_prepare_coefs(coefficients(object), nReg = object$nReg)[[counterfact]]
    ## overwrite
    X <- mm$X[[counterfact]]
    W <- mm$W[[counterfact]]
    coefs_j[["beta_j"]] <- coefs_j_[["beta_j"]]
    coefs_j[["rho_j"]] <- coefs_j_[["rho_j"]]
    coefs_j[["sigma_j"]] <- coefs_j_[["sigma_j"]]
  }

  ## is kappa_j_1 -Inf and kappa_j Inf a problem => don't think so
  p <- predict_opsr(X, W, coefs_j[["beta_j"]], coefs_j[["rho_j"]],
                    coefs_j[["sigma_j"]], coefs_j[["kappa_j_1"]], coefs_j[["kappa_j"]],
                    coefs_j[["gamma"]])

  p
}
