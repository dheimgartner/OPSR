#' Heckman two-step estimation
#'
#' Tow-step estimation procedure to generate reasonable starting values.
#'
#' @section Note:
#' Since the Heckman two-step estimator includes an estimate in the second step
#' regression, the resulting OLS standard errors and heteroskedasticity-robust
#' standard errors are incorrect.
#'
#' @export
opsr_2step <- function(W, Xs, Z, Ys) {
  nReg <- length(Xs)

  ## step 1
  fit_selection <- suppressWarnings(
    MASS::polr(factor(Z) ~ W, method = "probit")
  )

  kappa <- unname(fit_selection$zeta)
  names(kappa) <- paste0("kappa", 1:length(kappa))
  kappa_ <- c(-Inf, kappa, Inf)  # add boundaries for step 2

  gamma <- unname(fit_selection$coefficients)
  names(gamma) <- paste0("s_", colnames(W))

  ## step 2
  step2 <- function(x, lambda, y, kappa1, kappa2, z) {
    fit <- suppressWarnings(
      stats::lm(y ~ -1 + x + lambda)  # intercept already included in x
    )
    params <- coefficients(fit)
    beta <- unname(params[!(names(params) %in% "lambda")])
    C <- params[["lambda"]]
    RSS <- sum(residuals(fit)^2)
    n <- length(residuals(fit))

    ## sigma
    tmp1 <- ifelse(kappa1 == -Inf, 0, kappa1 - z)
    tmp2 <- ifelse(kappa2 == Inf, 0, kappa2 - z)
    nom <- tmp1 * dnorm(kappa1 - z) - tmp2 * dnorm(kappa2 - z)
    denom <- pnorm(kappa2 - z) - pnorm(kappa1 - z)
    sigma <- RSS / n - (C^2 / n) * sum(nom / denom - lambda^2)

    ## rho
    rho <- C / sigma

    list(beta = beta, sigma = sigma, rho = rho)
  }

  W_gamma <- W %*% gamma

  ## lambda (inverse mills ratio)
  lambda_hat_j <- function(j) {
    z <- W_gamma[Z == j]
    nom <- dnorm(kappa_[j] - z) - dnorm(kappa_[j + 1] - z)
    denom <- pnorm(kappa_[j + 1] - z) - pnorm(kappa_[j] - z)
    nom / denom
  }

  lambdas <- lapply(seq_len(nReg), function(j) lambda_hat_j(j))

  ## apply
  params_o <- lapply(seq_len(nReg), function(j) {
    x <- Xs[[j]]
    lambda <- lambdas[[j]]
    y <- Ys[[j]]
    kappa1 <- kappa_[j]
    kappa2 <- kappa_[j + 1]
    z <- W_gamma[Z == j]
    step2(x, lambda, y, kappa1, kappa2, z)
  })

  ## prepare output vector
  beta <- unlist(lapply(seq_len(nReg), function(j) {
    beta_j <- params_o[[j]]$beta
    names(beta_j) <- paste0("o", j, "_", colnames(Xs[[j]]))
    beta_j
  }))

  sigma <- unlist(lapply(seq_len(nReg), function(j) {
    params_o[[j]]$sigma
  }))
  names(sigma) <- paste0("sigma", 1:nReg)

  rho <- unlist(lapply(seq_len(nReg), function(j) {
    params_o[[j]]$rho
  }))
  names(rho) <- paste0("rho", 1:nReg)

  c(kappa, gamma, beta, sigma, rho)
}
