## list of length nReg
## each list element contains right gamma, kappa_j_1, kappa_j, beta_j, sigma_j, rho_j
opsr_prepare_theta <- function(theta, nReg) {
  coef_names <- names(theta)
  gamma <- theta[grepl("^s_", coef_names)]
  kappa <- c(-Inf, theta[grepl("^kappa", coef_names)], Inf)
  beta <- theta[grepl("^o[0-9]_", coef_names)]
  sigma <- theta[grepl("^sigma", coef_names)]
  rho <- theta[grepl("^rho", coef_names)]
  theta_ <- lapply(seq_len(nReg), function(i) {
    beta_j <- unname(beta[grepl(paste0("^o", i, "_"), names(beta))])
    kappa_j <- unname(kappa[c(i, i + 1)])
    sigma_j <- unname(sigma[i])
    rho_j <- unname(rho[i])
    list(gamma = unname(gamma), kappa_j_1 = kappa_j[1], kappa_j = kappa_j[2],
         beta_j = beta_j, sigma_j = sigma_j, rho_j = rho_j)
  })
  theta_
}
