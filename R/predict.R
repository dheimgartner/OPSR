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
#' @return a vector of length `nrow(newdata)` (or data used during estimation).
#' @export
#'
#' @details
#' Elements are `NA_real_` if the `group` does not correspond to the observed
#' regime (selection outcome). This ensures consistent output length.
predict.opsr <- function(object, newdata, group, counterfact = NULL,
                         type = c("response", "unlog-response", "prob", "mills"),
                         ...) {
  type <- match.arg(type)
  predict_opsr <- function(X_j, W_j, beta_j, rho_j, sigma_j, kappa_j_1, kappa_j, gamma,
                           type = c("response", "unlog-response", "prob", "mills")) {
    type <- match.arg(type)
    Xb <- X_j %*% beta_j  # xbif(j)
    Wg <- W_j %*% gamma  # xbsel
    kappa_j_Wg <- kappa_j - Wg
    kappa_j_1_Wg <- kappa_j_1 - Wg
    nom <- dnorm(kappa_j_Wg) - dnorm(kappa_j_1_Wg)
    denom <- pnorm(kappa_j_Wg) - pnorm(kappa_j_1_Wg)
    imr <- nom / denom  # (negative) inverse mills ratio millsif(j)

    out <-
      switch(type,
             "response" = as.vector(
               Xb - (rho_j * sigma_j) * imr
             ),
             "unlog-response" = as.vector(
               exp(Xb + (sigma_j**2) / 2) *
                 (pnorm(kappa_j_Wg - rho_j * sigma_j) - pnorm(kappa_j_1_Wg - rho_j * sigma_j)) /
                 (pnorm(kappa_j_Wg) - pnorm(kappa_j_1_Wg)) - 1
             ),
             "prob" = as.vector(pnorm(kappa_j_Wg) - pnorm(kappa_j_1_Wg)),
             "mills" = as.vector(imr)
      )

    out
  }

  ## so X_j and W_j are always the factual data matrices
  mm <- model.matrix(object, data = newdata)
  X <- mm$X[[group]]
  W <- mm$W[[group]]
  coefs_j <- opsr_prepare_coefs(coefficients(object), nReg = object$nReg)[[group]]

  ## for the counterfactuals we pass beta_j', rho_j' and sigma_j'
  ## if type == prob we even pass kappa_j', kappa_j_1'
  if (!is.null(counterfact)) {
    mm <- model.matrix(object, data = newdata, filter = group)
    coefs_j_<- opsr_prepare_coefs(coefficients(object), nReg = object$nReg)[[counterfact]]
    X <- mm$X[[counterfact]]
    W <- mm$W[[counterfact]]
    coefs_j[["beta_j"]] <- coefs_j_[["beta_j"]]
    coefs_j[["rho_j"]] <- coefs_j_[["rho_j"]]
    coefs_j[["sigma_j"]] <- coefs_j_[["sigma_j"]]
    if (type == "prob") {
      coefs_j[["kappa_j"]] <- coefs_j_[["kappa_j"]]
      coefs_j[["kappa_j_1"]] <- coefs_j_[["kappa_j_1"]]
    }
  }

  ## is kappa_j_1 -Inf and kappa_j Inf a problem => don't think so
  p <- predict_opsr(X, W, coefs_j[["beta_j"]], coefs_j[["rho_j"]],
                    coefs_j[["sigma_j"]], coefs_j[["kappa_j_1"]], coefs_j[["kappa_j"]],
                    coefs_j[["gamma"]], type = type)

  ## ensure to match length of data
  z <- get_z(object, newdata)
  out <- rep(NA_real_, length(z))
  out[z == group] <- p

  out
}
