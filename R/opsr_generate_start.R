#' 2-step regression to generate reasonable starting values
#'
#' Currently, separate ordinal probit regression and linear regression and
#' setting sigma and rho to 0.5. Should be improved!
#'
#' @param W
#' @param Xs
#' @param Z
#' @param Ys
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' ## tick: this is done by opsr() internally
#' sim_dat <- opsr_simulate()
#' dat <- sim_dat$data
#' W <- as.matrix(dat[, c("xs1", "xs2")])
#' Z <- dat$ys
#' Y <- dat$yo
#' nReg <- length(unique(Z))
#' Xs <- lapply(seq_len(nReg), function(i) {
#'   X <- W
#'   X[Z == i, ]
#' })
#' Ys <- lapply(seq_len(nReg), function(i) {
#'   Y[Z == i]
#' })
#' ## tock
#'
#' opsr_generate_start(W, Xs, Z, Ys)
#' }
opsr_generate_start <- function(W, Xs, Z, Ys) {
  warning("2-step regression to generate reasonable starting values should",
          " be improved.")

  nReg <- length(Xs)

  fit_selection <- suppressWarnings(
    MASS::polr(factor(Z) ~ W, method = "probit")
  )

  fits_outcome <- lapply(seq_len(nReg), function(i) {
    X <- Xs[[i]]
    Y <- Ys[[i]]
    suppressWarnings(
      fit <- stats::lm(Y ~ -1 + X)  # intercept already included in X
    )
    fit
  })

  kappa <- unname(fit_selection$zeta)
  names(kappa) <- paste0("kappa", 1:length(kappa))

  gamma <- unname(fit_selection$coefficients)
  names(gamma) <- paste0("s_", colnames(W))

  betas <- lapply(seq_along(fits_outcome), function(i) {
    fo <- fits_outcome[[i]]
    nn <- paste0("o", i, "_", colnames(Xs[[i]]))
    beta <- unname(fo$coefficients)
    names(beta) <- nn
    beta
  })
  betas <- unlist(betas)

  sigma <- rep(0.5, nReg)
  names(sigma) <- paste0("sigma", 1:length(sigma))

  rho <- rep(0.5, nReg)
  names(rho) <- paste0("rho", 1:length(rho))

  c(kappa, gamma, betas, sigma, rho)
}
