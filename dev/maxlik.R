library(maxLik)

rm(list = ls())

## example
x <- rnorm(100) # data. true mu = 0, sigma = 1
loglik <- function(theta, debug = FALSE) {
  if (debug) browser()
  mu <- theta[[1]]
  sigma <- theta[[2]]

  sum(dnorm(x, mean = mu, sd = sigma, log = TRUE))
}
start <- c(mu = 1, sigma = 2)
m <- maxLik(loglik, start = start)

## give start value somewhat off
summary(m)

fit <- optim(start, loglik, debug = FALSE)
fit$convergence
m$estimate

## here 1 OP and 3 regression specifications
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
par <- c(
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

loglik <- function(par, W, Z, Y, X1, X2, X3) {
  ## unpack par
  ## will depend on model specification...
  kappa1 <- par[["kappa1"]]
  kappa2 <- par[["kappa2"]]
  gamma1 <- par[["gamma1"]]
  gamma2 <- par[["gamma2"]]
  gamma <- c(gamma1, gamma2)
  beta11 <- par[["beta11"]]
  beta12 <- par[["beta12"]]
  beta1 <- c(beta11, beta12)
  beta21 <- par[["beta21"]]
  beta22 <- par[["beta22"]]
  beta2 <- c(beta21, beta22)
  beta31 <- par[["beta31"]]
  beta32 <- par[["beta32"]]
  beta3 <- c(beta31, beta32)
  sigma1 <- par[["sigma1"]]
  sigma2 <- par[["sigma2"]]
  sigma3 <- par[["sigma3"]]
  rho1 <- par[["rho1"]]
  rho2 <- par[["rho2"]]
  rho3 <- par[["rho3"]]

  final_sum <- 0

  for (i in 1:nrow(W)) {
    part1 <- 0
    part2 <- 0
    part3 <- 0

    if (Z[i] == 1) {
      res <- Y[i] - beta1 %*% X1[i, ]
      low <- -Inf
      high <- kappa1 - gamma %*% W[i, ]
      part1 <- 1 / sigma1 * dnorm(res / sigma1)
      part2 <- pnorm((sigma1 * high - rho1 * res) / (sigma1 * sqrt(1 - rho1^2)))
      part3 <- 0

    } else if (Z[i] == 2) {
      res <- Y[i] - beta2 %*% X2[i, ]
      low <- kappa1 - gamma %*% W[i, ]  # high from Z == 1
      high <- kappa2 - gamma %*% W[i, ]
      part1 <- 1 / sigma2 * dnorm(res / sigma2)
      part2 <- pnorm((sigma2 * high - rho2 * res) / (sigma2 * sqrt(1 - rho2^2)))
      part3 <- pnorm((sigma2 * low - rho2 * res) / (sigma2 * sqrt(1 - rho2^2)))

    } else if (Z[i] == 3) {
      res <- Y[i] - beta3 %*% X3[i, ]
      low <- kappa2 - gamma %*% W[i, ]  # high from Z == 2
      high <- Inf
      part1 <- 1 / sigma3 * dnorm(res / sigma3)
      part2 <- 1
      part3 <- pnorm((sigma3 * low - rho3 * res) / (sigma3 * sqrt(1 - rho3^2)))

    } else {
      stop("Response variable in selection model out of bound")
    }

    final_sum <- final_sum + log(part1) + log(part2 - part3)
  }

  -final_sum
}

loglik(par, W, Z, Y, X1, X2, X3)
st1 <- system.time(
  mle <- optim(par, loglik, W = W, Z = Z, Y = Y, X1 = X1, X2 = X2, X3 = X3,
               hessian = TRUE, control = list(maxit=50000))
)
mle$convergence
class(mle)

coef <- mle$par
stdev <- sqrt(diag(solve(mle$hessian)))
t_stats <- coef / stdev
p <- 2 * pnorm(-abs(t_stats))
sig <- car::recode(p, "lo:0.001='***'; 0.001:0.01='**'; 0.01:0.05='*';  0.05:0.1='.'; 0.1:hi=' '")
cbind(names(par), coef, stdev, t_stats, p, sig)
true_params

## maxLik maximizes! not minimizes
ll <- function(par) -loglik(par, W, Z, Y, X1, X2, X3)
ll(par)
st2 <- system.time(
  fit <- maxLik(ll, start = par, method = "NM", iterlim = 50000, printLevel = 2)
)
summary(fit)
stdev <- sqrt(diag(solve(fit$hessian)))

## (you could pass additional args to likelihood function via named args as in optim)
ll_pass_arg <- function(par, hello) {
  cat(hello, "\n")
  -loglik(par, W, Z, Y, X1, X2, X3)
}
fit <- maxLik(ll_pass_arg, start = par, method = "NM", iterlim = 50000, printLevel = 0, hello = "foo")


## compare runtime to opsr()
devtools::load_all()
start <- par
example <- c(kappa1 = -1.916, kappa2 = 0.969, s_X1 = 1.006, s_X2 = 1.315, o1_X1 = 1.482, o1_X2 = 1.525, o2_X1 = -1.19, o2_X2 = 1.358, o3_X1 = 1.459, o3_X2 = -1.586, sigma1 = 1, sigma2 = 1, sigma3 = 1, rho1 = 0, rho2 = 0, rho3 = 0)
names(start) <- names(example)
st3 <- system.time(
  fit_opsr <- opsr(Z | Y ~ -1 + X1 + X2, data = dat, start = start)
)
