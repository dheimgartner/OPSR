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
#'   also [`opsr_model_matrices`].
#' @param yo predict outcome of this group.
#' @param ys predict outcome `yo` given counterfactual selection `ys`.
#'
#' @export
predict.opsr <- function(object, newdata, yo, ys = NULL) {
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
  newdata <- if (missing(newdata)) NULL
  X <- opsr_model_matrices(object, yo, newdata)
  X_j <- X$X_j
  W_j <- X$W_j
  coefs_j <- opsr_prepare_coefs(coefficients(object), nReg = object$nReg)[[yo]]

  if (!is.null(ys)) {
    X_star <- opsr_model_matrices(object, ys, newdata, z = yo)
    coefs_j_star <- opsr_prepare_coefs(coefficients(object), nReg = object$nReg)[[ys]]
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

  p
}
