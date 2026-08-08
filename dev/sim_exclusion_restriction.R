###############################################################################
# Simulation study: strength of exclusion restriction and OPSR parameter
# recovery.  Inspired by Chiburis & Lokshin (2007), Stata J 7(2): 167–182.
#
# Design
# ------
# DGP: 3-regime OPSR identical to opsr_simulate() defaults.  The "instrument"
# is xs2, which enters the selection equation with coefficient gamma2 but is
# excluded from all outcome equations.  We vary gamma2 across
#   {0, 0.25, 0.5, 1.0, 1.5}
# to represent absent → strong exclusion restriction.
#
# For each condition we run R = 15 replications at N = 1000.
# We track bias / RMSE of rho_j (the hardest-to-identify parameters) and the
# Hessian condition number as a numerical sign of near-non-identification.
#
# A second section demonstrates opsr_test_exclusion_restriction() and
# opsr_diagnostics() on one representative dataset per extreme condition.
###############################################################################

library(OPSR)
library(mvtnorm)

## =========================================================
## 0.  Ground-truth parameters (match opsr_simulate() defaults)
## =========================================================
SIGMA <- matrix(c(
  1.0, 0.2, 0.3, 0.4,
  0.2, 1.1, 0.4, 0.5,
  0.3, 0.4, 1.2, 0.6,
  0.4, 0.5, 0.6, 1.3), ncol = 4)

KAPPA  <- c(-2, 1)
GAMMA1 <- 1            # coefficient on xs1 (not the instrument)
BETA1  <- c(1, 2, 1.1)
BETA2  <- c(1, -1, 1.5)
BETA3  <- c(1, 1.6, -2)

# rho_j = Cov(epsilon, eta_j) / (sd_epsilon * sd_eta_j), sd_epsilon = 1
TRUE_RHO   <- SIGMA[1, 2:4] / sqrt(diag(SIGMA)[2:4])
TRUE_SIGMA <- sqrt(diag(SIGMA)[2:4])

cat("True parameters\n")
cat(sprintf("  rho1 = %.4f  sigma1 = %.4f\n", TRUE_RHO[1], TRUE_SIGMA[1]))
cat(sprintf("  rho2 = %.4f  sigma2 = %.4f\n", TRUE_RHO[2], TRUE_SIGMA[2]))
cat(sprintf("  rho3 = %.4f  sigma3 = %.4f\n", TRUE_RHO[3], TRUE_SIGMA[3]))
cat(sprintf("  beta1 = (%s)\n", paste(BETA1, collapse = ", ")))
cat(sprintf("  beta2 = (%s)\n", paste(BETA2, collapse = ", ")))
cat(sprintf("  beta3 = (%s)\n", paste(BETA3, collapse = ", ")))

## =========================================================
## 1.  DGP
##     xs1 + xs2 -> selection, only xo1 + xo2 -> outcomes
##     gamma2 controls instrument strength
## =========================================================
dgp <- function(nobs, gamma2) {
  W <- matrix(rnorm(nobs * 2), nobs, 2)
  X <- matrix(rnorm(nobs * 2), nobs, 2)
  colnames(W) <- c("xs1", "xs2")
  colnames(X) <- c("xo1", "xo2")

  err  <- rmvnorm(nobs, mean = rep(0, 4), sigma = SIGMA)
  e    <- err[, 1]
  eta1 <- err[, 2]; eta2 <- err[, 3]; eta3 <- err[, 4]

  cali_z  <- W[, "xs1"] * GAMMA1 + W[, "xs2"] * gamma2 + e
  z       <- findInterval(cali_z, c(-Inf, KAPPA, Inf))

  X_ <- cbind(1, X)
  y1 <- X_ %*% BETA1 + eta1
  y2 <- X_ %*% BETA2 + eta2
  y3 <- X_ %*% BETA3 + eta3

  data.frame(
    ys  = z,
    yo  = ifelse(z == 1, y1, ifelse(z == 2, y2, y3)),
    xs1 = W[, 1], xs2 = W[, 2],
    xo1 = X[, 1], xo2 = X[, 2]
  )
}

## =========================================================
## 2.  Single replication
## =========================================================
FORM <- ys | yo ~ xs1 + xs2 | xo1 + xo2

one_rep <- function(nobs, gamma2) {
  dat <- dgp(nobs, gamma2)

  # Reject degenerate draws (regime too small for estimation)
  if (length(unique(dat$ys)) < 3 || min(table(dat$ys)) < 5) return(NULL)

  fit <- tryCatch(
    suppressWarnings(opsr(FORM, dat, printLevel = 0)),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)

  cf <- coef(fit)
  dg <- opsr_diagnostics(fit)

  list(
    gamma2_hat = cf["s_xs2"],
    rho1       = cf["rho1"],
    rho2       = cf["rho2"],
    rho3       = cf["rho3"],
    sigma1     = cf["sigma1"],
    sigma2     = cf["sigma2"],
    sigma3     = cf["sigma3"],
    o1_xo1     = cf["o1_xo1"],
    o1_xo2     = cf["o1_xo2"],
    o2_xo1     = cf["o2_xo1"],
    o2_xo2     = cf["o2_xo2"],
    o3_xo1     = cf["o3_xo1"],
    o3_xo2     = cf["o3_xo2"],
    hess_negdef = dg$hessian_neg_def,
    cond_number = dg$condition_number,
    max_grad    = dg$max_abs_gradient
  )
}

## =========================================================
## 3.  Run simulation
## =========================================================
set.seed(2024)

GAMMA2_VALS <- c(0, 0.25, 0.5, 1.0, 1.5)
NOBS        <- 1000
R           <- 15

cat(sprintf("\n=== Monte Carlo: gamma2 in {%s}, N=%d, R=%d ===\n",
            paste(GAMMA2_VALS, collapse = ", "), NOBS, R))

t0 <- proc.time()
sim_results <- lapply(GAMMA2_VALS, function(g2) {
  cat(sprintf("\n  gamma2 = %.2f ", g2))
  reps <- vector("list", R)
  for (r in seq_len(R)) {
    reps[[r]] <- one_rep(NOBS, g2)
    cat(".")
  }
  n_ok <- sum(!sapply(reps, is.null))
  cat(sprintf(" [%d/%d ok]", n_ok, R))
  list(gamma2 = g2, reps = Filter(Negate(is.null), reps))
})
names(sim_results) <- as.character(GAMMA2_VALS)
t1 <- proc.time()
cat(sprintf("\n\nTotal simulation time: %.1f s\n", (t1 - t0)["elapsed"]))

## =========================================================
## 4.  Summarize bias and RMSE
## =========================================================
extract_vals <- function(res_list, param) {
  sapply(res_list$reps, function(r) r[[param]])
}

bias_rmse <- function(vals, true_val) {
  vals <- vals[is.finite(vals)]
  if (length(vals) == 0) return(c(n = 0, mean = NA, bias = NA, rmse = NA, sd = NA))
  c(
    n    = length(vals),
    mean = mean(vals),
    bias = mean(vals) - true_val,
    rmse = sqrt(mean((vals - true_val)^2)),
    sd   = sd(vals)
  )
}

param_truth <- list(
  rho1   = TRUE_RHO[1],
  rho2   = TRUE_RHO[2],
  rho3   = TRUE_RHO[3],
  sigma1 = TRUE_SIGMA[1],
  sigma2 = TRUE_SIGMA[2],
  sigma3 = TRUE_SIGMA[3],
  o1_xo1 = BETA1[2],
  o1_xo2 = BETA1[3],
  o2_xo1 = BETA2[2],
  o2_xo2 = BETA2[3],
  o3_xo1 = BETA3[2],
  o3_xo2 = BETA3[3]
)

# Build summary table
rows <- list()
for (g2_chr in names(sim_results)) {
  res   <- sim_results[[g2_chr]]
  g2    <- res$gamma2
  g2_tr <- g2  # true value of gamma2 on xs2

  # gamma2_hat has true value = g2
  vals <- extract_vals(res, "gamma2_hat")
  br   <- bias_rmse(vals, g2_tr)
  rows[[length(rows) + 1]] <- data.frame(
    gamma2 = g2, param = "s_xs2", true = g2_tr,
    n = br["n"], mean = br["mean"], bias = br["bias"],
    rmse = br["rmse"], sd = br["sd"]
  )

  for (p in names(param_truth)) {
    vals <- extract_vals(res, p)
    br   <- bias_rmse(vals, param_truth[[p]])
    rows[[length(rows) + 1]] <- data.frame(
      gamma2 = g2, param = p, true = param_truth[[p]],
      n = br["n"], mean = br["mean"], bias = br["bias"],
      rmse = br["rmse"], sd = br["sd"]
    )
  }
}
tab <- do.call(rbind, rows)
rownames(tab) <- NULL

cat("\n=== BIAS AND RMSE BY INSTRUMENT STRENGTH ===\n")

# Focus on rho (identification-sensitive) and one outcome beta
for (p in c("rho1", "rho2", "rho3", "sigma1", "sigma2", "sigma3",
            "o1_xo1", "o1_xo2", "s_xs2")) {
  sub <- tab[tab$param == p, ]
  cat(sprintf("\n  Parameter: %s (true = %.4f)\n", p, sub$true[1]))
  cat(sprintf("  %-8s  %6s  %7s  %7s  %7s\n", "gamma2", "n_ok", "mean", "bias", "rmse"))
  for (i in seq_len(nrow(sub))) {
    cat(sprintf("  %-8.2f  %6d  %7.4f  %7.4f  %7.4f\n",
                sub$gamma2[i], sub$n[i], sub$mean[i], sub$bias[i], sub$rmse[i]))
  }
}

## =========================================================
## 5.  Hessian diagnostics summary
## =========================================================
cat("\n=== HESSIAN DIAGNOSTICS BY INSTRUMENT STRENGTH ===\n")
cat(sprintf("%-8s  %-10s  %-12s  %-12s  %-10s\n",
            "gamma2", "n_ok", "pct_hess_ok", "med_cond_num", "med_grad"))
cat(strrep("-", 60), "\n")

for (res in sim_results) {
  g2          <- res$gamma2
  hess_ok     <- sapply(res$reps, function(r) r$hess_negdef)
  cond_nums   <- sapply(res$reps, function(r) r$cond_number)
  max_grads   <- sapply(res$reps, function(r) r$max_grad)
  n           <- length(res$reps)
  pct_ok      <- if (n > 0) round(100 * mean(hess_ok, na.rm = TRUE)) else NA
  med_cond    <- if (n > 0) median(cond_nums, na.rm = TRUE) else NA
  med_grad    <- if (n > 0) median(max_grads, na.rm = TRUE) else NA
  cat(sprintf("%-8.2f  %-10d  %-12d  %-12.1f  %-10.6f\n",
              g2, n, pct_ok, med_cond, med_grad))
}

## =========================================================
## 6.  Exclusion restriction diagnostic illustration
##     Run opsr_test_exclusion_restriction() on one dataset
##     per selected condition
## =========================================================
cat("\n\n=== EXCLUSION RESTRICTION DIAGNOSTIC ILLUSTRATIONS ===\n")
cat("(one dataset each for gamma2 = 0, 0.5, 1.5)\n")

diag_conditions <- c(0, 0.5, 1.5)

for (g2 in diag_conditions) {
  label <- switch(
    as.character(g2),
    "0"   = "ABSENT instrument",
    "0.5" = "MODERATE instrument",
    "1.5" = "STRONG instrument"
  )
  cat(sprintf("\n---------- gamma2 = %.1f  [%s] ----------\n", g2, label))

  set.seed(42 + g2 * 100)
  dat <- dgp(1000, g2)
  cat("Regime counts: "); print(table(dat$ys))

  fit <- suppressWarnings(opsr(FORM, dat, printLevel = 0))

  cat("\n-- opsr_diagnostics() --\n")
  dg <- opsr_diagnostics(fit)
  print(dg)

  cat("\n-- opsr_test_exclusion_restriction() (grid = 11 points) --\n")
  test <- opsr_test_exclusion_restriction(
    fit, instrument = "xs2",
    grid = seq(-0.99, 0.99, length.out = 11),
    printLevel = 0
  )
  print(test)

  # Profile: summarize flatness by range of profile log-lik per regime
  prof <- test$profile_rho
  for (j in 1:3) {
    sub_j  <- prof[prof$regime == j, ]
    ll_rng <- diff(range(sub_j$profile_loglik, na.rm = TRUE))
    cat(sprintf("  Profile LL range (regime %d): %.3f  %s\n",
                j, ll_rng,
                if (ll_rng < 2) "[FLAT - weak identification]"
                else "[curved - good identification]"))
  }
}

cat("\n=== Done ===\n")
