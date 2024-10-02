## first try with simulated data

devtools::load_all()

rm(list = ls())

## here 1 OP and 3 regression specifications
sim_dat <- opsr_simulate()
true_params <- sim_dat$params
dat <- sim_dat$data

## specify the selection model

W <- X <- as.matrix(dat[c("X1", "X2")])
## dependent variable of the selection model
## Z == 1: NTW
## Z == 2: NUTW
## Z == 3: UTW
Z <- dat[, "Z"]
K <- c(-2, 1)  # initial values for kappa
G <- c(1, 1.5)  # initial values for gamma

Y <- dat[, "Y"]  # outcome model dependent variable

## outcome model specification for NTWer
X_NTW <- X
B_NTW <- c(1, 1.1)  # initial values for beta1

## outcome model specification for NUTWers
X_NUTW <- X
B_NUTW <- c(-1, 1.5)

## outcome model specification for UTWers
X_UTW <- X
B_UTW <- c(1.6, -2)

S <- c(1, 1, 1)
R <- c(0.1, 0.1, 0.1)

parameter <- c(K, G, B_NTW, B_NUTW, B_UTW, S, R)
par_name <- c("kappa1", "kappa2", "gamma1", "gamma2",
              "beta11", "beta12",
              "beta21", "beta22",
              "beta31", "beta32",
              "sig1", "sig2", "sig3",
              "rho1", "rho2", "rho3")

p_K <- length(K)
p_G <- length(G)
p_B_NTW <- length(B_NTW)
p_B_NUTW <- length(B_NUTW)
p_B_UTW <- length(B_UTW)
p_S <- length(S)
p_R <- length(R)

## (negative) log likelihood function
nLL_simple <- function(nLL.par, nLL.Z, nLL.W, nLL.Y, nLL.X_NTW, nLL.X_NUTW, nLL.X_UTW) {
  threshold_12 <- nLL.par[1]
  threshold_23 <- nLL.par[2]
  gamma <- matrix(nLL.par[3: (p_K + p_G)], ncol =  p_G)
  beta_NTW <- matrix(nLL.par[(p_K + p_G + 1) : (p_K + p_G + p_B_NTW) ], ncol =  p_B_NTW)
  beta_NUTW <- matrix(nLL.par[ (p_K + p_G + p_B_NTW + 1) : (p_K + p_G + p_B_NTW + p_B_NUTW)], ncol = p_B_NUTW)
  beta_UTW <- matrix(nLL.par[(p_K + p_G + p_B_NTW + p_B_NUTW + 1): (p_K + p_G + p_B_NTW + p_B_NUTW + p_B_UTW)], ncol = p_B_UTW)
  sigma1 <- nLL.par[(p_K + p_G + p_B_NTW + p_B_NUTW + p_B_UTW + 1)]
  sigma2 <- nLL.par[(p_K + p_G + p_B_NTW + p_B_NUTW + p_B_UTW + 2)]
  sigma3 <- nLL.par[(p_K + p_G + p_B_NTW + p_B_NUTW + p_B_UTW + 3)]
  rho1 <- nLL.par[(p_K + p_G + p_B_NTW + p_B_NUTW + p_B_UTW + p_S + 1)]
  rho2 <- nLL.par[(p_K + p_G + p_B_NTW + p_B_NUTW + p_B_UTW + p_S + 2)]
  rho3 <- nLL.par[(p_K + p_G + p_B_NTW + p_B_NUTW + p_B_UTW + p_S + 3)]

  final_sum <- 0

  for(i in 1:nrow(nLL.W)){
    part1 <- 0
    part2 <- 0
    part3 <- 0

    if (nLL.Z[i]==1) {
      res <- nLL.Y[i] -  beta_NTW %*% nLL.X_NTW[i,]
      low <- -Inf
      high <- threshold_12 - gamma %*% nLL.W[i,]
      part1 <- 1/sigma1 * dnorm(res/sigma1)
      part2 <- pnorm((sigma1 * high - rho1 * res) / (sigma1*sqrt(1-rho1**2)))
      part3 <- 0

    } else if (nLL.Z[i]==2) {
      res <- nLL.Y[i] -  beta_NUTW %*% nLL.X_NUTW[i,]
      low <- threshold_12 - gamma %*% nLL.W[i,]
      high <- threshold_23 - gamma %*% nLL.W[i,]
      part1 <- 1/sigma2 * dnorm(res/sigma2)
      part2 <- pnorm((sigma2 * high - rho2 * res) / (sigma2*sqrt(1-rho2**2)))
      part3 <- pnorm((sigma2 * low - rho2 * res) / (sigma2*sqrt(1-rho2**2)))

    } else {
      res <- nLL.Y[i] -  beta_UTW %*% nLL.X_UTW[i,]
      low <- threshold_23 - gamma %*% nLL.W[i,]
      high <- Inf
      part1 <- 1/sigma3 * dnorm(res/sigma3)
      part2 <- 1
      part3 <- pnorm((sigma3 * low - rho3 * res) / (sigma3*sqrt(1-rho3**2)))

    }
    final_sum <- final_sum + log(part1) + log(part2 - part3)
  }

  # part1
  # part2 - part3

  -final_sum
}

debugonce(nLL_simple)
nLL_simple(nLL.par = parameter, nLL.Z = Z, nLL.W = W,
           nLL.Y = Y, nLL.X_NTW = X_NTW, nLL.X_NUTW = X_NUTW, nLL.X_UTW = X_UTW)

?optim
MLE <- optim(parameter, nLL_simple, nLL.Z = Z, nLL.W = W,
             nLL.Y = Y, nLL.X_NTW = X_NTW, nLL.X_NUTW = X_NUTW, nLL.X_UTW = X_UTW,
             hessian = T, # method = "Nelder-Mead",
             control = list(maxit=50000))
MLE$convergence




## extract
(coef <- MLE$par)
# (stdev <- sqrt(1/diag(MLE$hessian)))
(stdev <- sqrt(diag(solve(MLE$hessian))))
t_stats <- coef/stdev
p <-  2*pnorm(-abs(t_stats))
sig <- car::recode(p, "lo:0.001='***'; 0.001:0.01='**'; 0.01:0.05='*';  0.05:0.1='.'; 0.1:hi=' '")
# write.csv(cbind(par_name, coef, stdev, t_stats, p, sig), file = 'Table/OPSR_MLE.csv')
cbind(par_name, coef, stdev, t_stats, p, sig)
true_params

coef_hessian <- as.data.frame(MLE$hessian)
names(coef_hessian) <- par_name
# write.csv(coef_hessian, file = 'Table/OPSR_MLE_hessian.csv', row.names = FALSE)
coef_hessian
