opsr_delta_method <- function(X_j, W_j, beta_j, rho_j, sigma_j, kappa_j_1, kappa_j, gamma, cov = NULL) {
  browser()
  X_j <- X_j[1, ]
  W_j <- W_j[1, ]
  tmp <- c("o2_X1", "o2_X2", "rho2", "sigma2", "kappa1", "kappa2", "s_X1", "s_X2")
  cov <- vcov(fit_bfgs)[tmp, tmp]

  form <- sprintf(
    "~ (%f * x1 + %f * x2) - x3 * x4 * ( dnorm(x5 - (%f * x7 + %f * x8)) - dnorm(x6 - (%f * x7 + %f * x8)) ) / ( pnorm(x5 - (%f * x7 + %f * x8)) - pnorm(x6 - (%f * x7 + %f * x8)) )",
    X_j[1], X_j[2], W_j[1], W_j[2], W_j[1], W_j[2], W_j[1], W_j[2], W_j[1], W_j[2]
  )

  x1 <- beta_j[1]
  x2 <- beta_j[2]
  x3 <- rho_j
  x4 <- sigma_j
  x5 <- kappa_j
  x6 <- kappa_j_1
  x7 <- gamma[1]
  x8 <- gamma[2]
  nn <- paste0("x", 1:8)
  rownames(cov) <- nn
  colnames(cov) <- nn
  msm::deltamethod(as.formula(form), c(x1, x2, x3, x4, x5, x6, x7, x8), cov)
}
