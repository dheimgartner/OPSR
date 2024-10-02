## thinking about how I would implement formula 6

devtools::load_all()

library(maxLik)

rm(list = ls())

sim_dat <- opsr_simulate()
true_params <- sim_dat$params
dat <- sim_dat$data

## data
W <- X <- as.matrix(dat[c("X1", "X2")])
Z <- dat[, "Z"]
Y <- dat[, "Y"]
X1 <- X
X2 <- X
X3 <- X

## start
start <- c(
  kappa1 = -2,
  kappa2 = 1,
  gamma1 = 1,
  gamma2 = 1.5,
  beta11 = 1,
  beta12 = 1.1,
  beta21 = -1,
  beta22 = 1.5,
  beta31 = 1.6,
  beta32 = -2,
  sigma1 = 1,
  sigma2 = 1,
  sigma3 = 1,
  rho1 = 0.1,
  rho2 = 0.1,
  rho3 = 0.1
)

## would it be easier to think of loglik_j and then sum later
## I think this could make sense with Formula...
## model.frame accepts subset!
## however, involves more function calls (loglik_j could be implemented in C++)

## here we assume that there is only one model frame X containing all the variables
## (i.e., combined X1, X2, X3 and W)
loglik <- function(theta, X, Z, Y) {
  min_z <- min(Z)  # is this safe?
  max_z <- max(Z)
  ll_j <- function(X_j, y_j, W, z, gamma, kappa_j, kappa_j_1, beta_j, sigma_j, rho_j) {
    res <- y_j - X_j %*% beta_j
    low <- kappa_j_1 - W %*% gamma
    high <- kappa_j - W %*% gamma
    part1 <- 1 / sigma_j * dnorm(res / sigma_j)
    part2 <- if (z == max_z) 1 else pnorm((sigma_j * high - rho_j * res) / (sigma_j * sqrt(1 - rho_j^2)))
    part3 <- if (z == min_z) 0 else pnorm((sigma_j * low - rho_j * res) / (sigma_j * sqrt(1 - rho_j^2)))
    log(part1) + log(part2 - part3)
  }

  kappa1 <- theta[["kappa1"]]
  kappa2 <- theta[["kappa2"]]
  kappa <- c(kappa1, kappa2)
  gamma1 <- theta[["gamma1"]]
  gamma2 <- theta[["gamma2"]]
  gamma <- c(gamma1, gamma2)
  beta11 <- theta[["beta11"]]
  beta12 <- theta[["beta12"]]
  beta1 <- c(beta11, beta12)
  beta21 <- theta[["beta21"]]
  beta22 <- theta[["beta22"]]
  beta2 <- c(beta21, beta22)
  beta31 <- theta[["beta31"]]
  beta32 <- theta[["beta32"]]
  beta3 <- c(beta31, beta32)
  beta <- list(beta1, beta2, beta3)
  sigma1 <- theta[["sigma1"]]
  sigma2 <- theta[["sigma2"]]
  sigma3 <- theta[["sigma3"]]
  sigma <- c(sigma1, sigma2, sigma3)
  rho1 <- theta[["rho1"]]
  rho2 <- theta[["rho2"]]
  rho3 <- theta[["rho3"]]
  rho <- c(rho1, rho2, rho3)

  final_sum <- 0

  ## for each observation
  for (i in seq_along(Z)) {
    ## prepare X_j, y_j, W, z, gamma, kappa_j, kappa_j_1, sigma_j, rho_j
    j <- Z[i]
    X_j <- X[i, ]
    y_j <- Y[i]
    W <- X_j  # (here; otherwise parsed model.matrix)
    z <- Z[i]
    # gamma <- gamma
    kappa_j <- kappa[j]
    kappa_j_1 <- kappa[j-1]
    sigma_j <- sigma[j]
    rho_j <- rho[j]
    beta_j <- beta[[j]]  # is list obj

    final_sum <- final_sum + ll_j(X_j, y_j, W, z, gamma, kappa_j, kappa_j_1, beta_j, sigma_j, rho_j)
  }

  final_sum
}

# debugonce(loglik)
loglik(start, X, Z, Y)

system.time(
  fit <- maxLik(loglik, start = start, method = "NM", iterlim = 50000, printLevel = 2,
                X = X, Z = Z, Y = Y)
)
summary(fit)
