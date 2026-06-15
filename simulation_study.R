###############################################################################
# Replication of Chiburis & Lokshin (2007) Monte Carlo Simulations
# "Maximum likelihood and two-step estimation of an ordered-probit
# selection model", Stata Journal 7(2): 167–182  (Section 6)
#
# PAPER DGP (Sections 6.1–6.2):
#   x1, x2 ~ N(0,1) independent
#   u, eps  ~ bivariate standard normal, Cor(u, eps) = rho
#   z*i = alpha1*x1 + alpha2*x2 + u;  kappa = (-1, 1) -> ys in {1,2,3}
#   yi  = beta*x1 + eps   [same beta across regimes by design]
#   w = [x1, x2],  x = [x1]   (x2 is the excluded instrument)
#
# We adapt to OPSR's switching-regression setup (y observed in ALL
# regimes) which corresponds to Run 3 ("multiple equations") in the paper.
# Chiburis notes this has little effect on results.  Regime 1 (ys=1,
# cutoffs -inf to -1) corresponds to paper's j*=0 (exterior); regime 2
# (ys=2, cutoffs -1 to 1) corresponds to j*=1 (interior, harder to
# identify).
#
# Runs implemented (Sections 6.2–6.8):
#  Run 1 – Baseline: N in {50,100,200,300,500,1000}, R=100
#  Run 2 – High correlation (rho=0.99)
#  Run 3 – Multiple equations  [= our default, reported from Run 1]
#  Run 4 – Non-normal shocks   (squared-normal, chi-sq(1)-1 centered)
#  Run 5 – No exclusion restriction  (w = x = [x1])
#  Run 6 – Exclusion restriction NOT satisfied (x2 also in outcome)
#  Run 7 – Weak instrument     (alpha2 = 0, x2 in w but irrelevant)
#
# Each run is followed by opsr_test_exclusion_restriction() on one
# representative dataset.
###############################################################################

library(OPSR)
library(mvtnorm)

set.seed(2007)

## =========================================================
## 0.  Constants matching paper Section 6.1–6.2
## =========================================================
TRUE_BETA  <- 1
TRUE_RHO   <- 0.5
TRUE_SIGMA <- 1
ALPHA1     <- 1
ALPHA2     <- 1
KAPPA      <- c(-1, 1)

# Covariance matrix for (u, eps1, eps2, eps3).
# Cor(u, eps_j) = rho for all j; cross-regime correlations = 0 (unidentifiable).
make_sigma <- function(rho) {
  cov_ue <- rho  # rho * sigma_u * sigma_eps = rho * 1 * 1
  matrix(c(
    1,      cov_ue, cov_ue, cov_ue,
    cov_ue, 1,      0,      0,
    cov_ue, 0,      1,      0,
    cov_ue, 0,      0,      1
  ), ncol = 4)
}

## =========================================================
## 1.  DGP function
## =========================================================
# alpha2         – coefficient on x2 in selection (0 = weak/absent instrument)
# rho            – error correlation
# beta_x2_out    – if != 0, x2 appears in outcome (violates excl. restr.)
# nonnormal      – if TRUE, use squared-normal (chi-sq) errors
dgp <- function(nobs,
                alpha2      = ALPHA2,
                rho         = TRUE_RHO,
                beta_x2_out = 0,
                nonnormal   = FALSE) {
  x1 <- rnorm(nobs)
  x2 <- rnorm(nobs)

  Sigma <- make_sigma(rho)
  raw   <- rmvnorm(nobs, mean = rep(0, 4), sigma = Sigma)

  if (nonnormal) {
    # Square then centre: E[Z^2 - 1] = 0, non-normal marginals
    err <- raw^2 - 1
  } else {
    err <- raw
  }
  u <- err[, 1]; e1 <- err[, 2]; e2 <- err[, 3]; e3 <- err[, 4]

  z_star <- ALPHA1 * x1 + alpha2 * x2 + u
  ys     <- findInterval(z_star, c(-Inf, KAPPA, Inf))

  # Outcome: same beta=1 on x1 for all regimes; optionally x2 too
  y_base <- TRUE_BETA * x1 + beta_x2_out * x2
  y1 <- y_base + e1
  y2 <- y_base + e2
  y3 <- y_base + e3
  yo <- ifelse(ys == 1, y1, ifelse(ys == 2, y2, y3))

  data.frame(ys = ys, yo = yo, xs1 = x1, xs2 = x2)
}

## =========================================================
## 2.  Single-replication helper
## =========================================================
fit_one <- function(dat, formula, true_beta = TRUE_BETA) {
  if (length(unique(dat$ys)) < 3 || min(table(dat$ys)) < 5) return(NULL)
  fit <- tryCatch(
    suppressWarnings(opsr(formula, dat, printLevel = 0)),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)

  cf <- coef(fit)
  vc <- tryCatch(vcov(fit), error = function(e) NULL)

  out <- list()
  for (j in 1:3) {
    nm_b <- paste0("o", j, "_xs1")
    nm_r <- paste0("rho", j)
    b    <- cf[nm_b]
    out[[paste0("beta", j)]] <- b
    out[[paste0("rho", j)]]  <- cf[nm_r]
    if (!is.null(vc) && nm_b %in% rownames(vc)) {
      se <- sqrt(vc[nm_b, nm_b])
      lo <- b - 1.96 * se; hi <- b + 1.96 * se
      out[[paste0("cov", j)]] <- (lo <= true_beta) & (true_beta <= hi)
    } else {
      out[[paste0("cov", j)]] <- NA
    }
  }
  out
}

## =========================================================
## 3.  Summarise replications
## =========================================================
summarize_reps <- function(reps, true_beta = TRUE_BETA) {
  reps <- Filter(Negate(is.null), reps)
  n    <- length(reps)
  if (n == 0) return(NULL)
  out  <- data.frame(n_ok = n)
  for (j in 1:3) {
    betas <- sapply(reps, function(r) r[[paste0("beta", j)]])
    rhos  <- sapply(reps, function(r) r[[paste0("rho",  j)]])
    covs  <- sapply(reps, function(r) r[[paste0("cov",  j)]])
    betas <- betas[is.finite(betas)]
    out[[paste0("b", j, "_mean")]] <- mean(betas, na.rm = TRUE)
    out[[paste0("b", j, "_sd")]]   <- sd(betas,   na.rm = TRUE)
    out[[paste0("b", j, "_bias")]] <- mean(betas, na.rm = TRUE) - true_beta
    out[[paste0("c", j, "_pct")]]  <- mean(covs,  na.rm = TRUE) * 100
    out[[paste0("r", j, "_mean")]] <- mean(rhos,  na.rm = TRUE)
  }
  out
}

## =========================================================
## 4.  Pretty-print one row
## =========================================================
print_row <- function(label, s) {
  cat(sprintf("  %-38s", label))
  for (j in 1:3) {
    cat(sprintf("  %6.4f(%6.4f)[%4.1f%%]",
                s[[paste0("b", j, "_mean")]],
                s[[paste0("b", j, "_sd")]],
                s[[paste0("c", j, "_pct")]]))
  }
  cat("\n")
}

## =========================================================
## 5.  RUN 1 – Baseline (Table 1 analogue)
##     rho=0.5, alpha1=alpha2=beta=1, w=[x1,x2], x=[x1]
##     N in {50,100,200,300,500,1000}, R=100
## =========================================================
cat("=== RUN 1: BASELINE ===\n")
cat("  True beta=1, rho=0.5, alpha1=alpha2=1; w=[x1,x2], x=[x1]\n\n")
cat("  (columns: regime 1 exterior | regime 2 interior | regime 3 exterior)\n")
cat(sprintf("  %-38s  %22s  %22s  %22s\n",
            "Condition", "Reg1: mean(sd)[cov%]",
            "Reg2: mean(sd)[cov%]", "Reg3: mean(sd)[cov%]"))
cat("  ", strrep("-", 110), "\n")

FORM1 <- ys | yo ~ xs1 + xs2 | xs1
N_VALS <- c(50, 100, 200, 300, 500, 1000)
R <- 100

run1_results <- lapply(N_VALS, function(n) {
  reps <- lapply(seq_len(R), function(i) fit_one(dgp(n), FORM1))
  summarize_reps(reps)
})
names(run1_results) <- as.character(N_VALS)

for (nm in names(run1_results)) {
  print_row(sprintf("FIML  N=%s", nm), run1_results[[nm]])
}

## =========================================================
## 6.  RUN 2 – High correlation (rho=0.99)
## =========================================================
cat("\n=== RUN 2: HIGH CORRELATION (rho=0.99) ===\n")
cat(sprintf("  %-38s  %22s  %22s  %22s\n",
            "Condition", "Reg1: mean(sd)[cov%]",
            "Reg2: mean(sd)[cov%]", "Reg3: mean(sd)[cov%]"))
reps2 <- lapply(seq_len(R), function(i) fit_one(dgp(1000, rho = 0.99), FORM1))
run2  <- summarize_reps(reps2)
print_row("FIML  N=1000, rho=0.99", run2)

## =========================================================
## 7.  RUN 3 – Multiple equations [= our default]
## =========================================================
cat("\n=== RUN 3: MULTIPLE EQUATIONS ===\n")
cat("  OPSR always estimates y in all regimes (switching regression).\n")
cat("  Run 1 at N=1000 already represents this case.\n")
print_row("FIML  N=1000 [from Run 1]", run1_results[["1000"]])

## =========================================================
## 8.  RUN 4 – Non-normal shocks (chi-squared, centred)
## =========================================================
cat("\n=== RUN 4: NON-NORMAL SHOCKS (Z^2 - 1, chi-sq centred) ===\n")
cat("  FIML assumes normality; OPSR may be biased, esp. exterior regimes.\n")
cat(sprintf("  %-38s  %22s  %22s  %22s\n",
            "Condition", "Reg1: mean(sd)[cov%]",
            "Reg2: mean(sd)[cov%]", "Reg3: mean(sd)[cov%]"))
reps4 <- lapply(seq_len(R), function(i) fit_one(dgp(1000, nonnormal = TRUE), FORM1))
run4  <- summarize_reps(reps4)
print_row("FIML  N=1000, nonnormal", run4)

## =========================================================
## 9.  RUN 5 – No exclusion restriction (w = x = [x1])
##     alpha2=0 effectively; model formula drops x2 entirely
## =========================================================
cat("\n=== RUN 5: NO EXCLUSION RESTRICTION (w=x=[x1]) ===\n")
cat("  Identification relies solely on nonlinearity of lambda.\n")
cat("  Interior regime (Reg 2) nearly unidentified per Fig 1 of paper.\n")
cat(sprintf("  %-38s  %22s  %22s  %22s\n",
            "Condition", "Reg1: mean(sd)[cov%]",
            "Reg2: mean(sd)[cov%]", "Reg3: mean(sd)[cov%]"))
FORM5 <- ys | yo ~ xs1 | xs1
reps5 <- lapply(seq_len(R), function(i) fit_one(dgp(1000, alpha2 = 0), FORM5))
run5  <- summarize_reps(reps5)
print_row("FIML  N=1000, no instrument", run5)

## =========================================================
## 10.  RUN 6 – Exclusion restriction NOT satisfied
##      DGP: y = beta*x1 + 1*x2 + eps  (x2 in outcome)
##      Researcher's model: ys|yo ~ xs1+xs2 | xs1  (wrong!)
##      True beta (on x1) = 1; estimates will be severely biased
## =========================================================
cat("\n=== RUN 6: EXCLUSION RESTRICTION NOT SATISFIED (x2 in outcome) ===\n")
cat("  DGP: y = 1*x1 + 1*x2 + eps; researcher omits x2 from outcome.\n")
cat("  Tight but wrong estimates expected (cf. paper Table 6: beta~0.13).\n")
cat(sprintf("  %-38s  %22s  %22s  %22s\n",
            "Condition", "Reg1: mean(sd)[cov%]",
            "Reg2: mean(sd)[cov%]", "Reg3: mean(sd)[cov%]"))
reps6 <- lapply(seq_len(R), function(i) fit_one(dgp(1000, beta_x2_out = 1), FORM1))
run6  <- summarize_reps(reps6)
print_row("FIML  N=1000, excl violated", run6)

## =========================================================
## 11.  RUN 7 – Weak instrument (alpha2=0 in DGP, x2 in model)
##      x2 claimed as instrument but has no actual effect on selection
## =========================================================
cat("\n=== RUN 7: WEAK INSTRUMENT (alpha2=0, x2 in w but ineffective) ===\n")
cat("  Similar to Run 5: identification from nonlinearity only.\n")
cat(sprintf("  %-38s  %22s  %22s  %22s\n",
            "Condition", "Reg1: mean(sd)[cov%]",
            "Reg2: mean(sd)[cov%]", "Reg3: mean(sd)[cov%]"))
reps7 <- lapply(seq_len(R), function(i) fit_one(dgp(1000, alpha2 = 0), FORM1))
run7  <- summarize_reps(reps7)
print_row("FIML  N=1000, alpha2=0", run7)

## =========================================================
## 12.  COMPREHENSIVE SUMMARY TABLE
## =========================================================
cat("\n\n=== COMPREHENSIVE SUMMARY (N=1000, R=100) ===\n")
cat("  True beta=1 for all regimes and all runs.\n")
cat("  Mean(SD) and 95%CI coverage for beta estimate in each regime.\n\n")
cat(sprintf("  %-40s  %-20s  %-20s  %-20s\n",
            "Run", "Regime 1 (exterior)", "Regime 2 (interior)", "Regime 3 (exterior)"))
cat("  ", strrep("-", 106), "\n")

summary_runs <- list(
  "Run 1 – Baseline (N=1000)"              = run1_results[["1000"]],
  "Run 2 – High correlation (rho=0.99)"    = run2,
  "Run 3 – Multiple eq. [=Run1 N=1000]"   = run1_results[["1000"]],
  "Run 4 – Non-normal shocks"              = run4,
  "Run 5 – No excl. restriction (w=x)"    = run5,
  "Run 6 – Excl. restr. violated"         = run6,
  "Run 7 – Weak instrument (alpha2=0)"    = run7
)
for (nm in names(summary_runs)) {
  print_row(nm, summary_runs[[nm]])
}

## =========================================================
## 13.  opsr_test_exclusion_restriction() FOR EACH RUN
## =========================================================
cat("\n\n=== opsr_test_exclusion_restriction() ILLUSTRATIONS ===\n")
cat("  One representative dataset (N=1000) per run.\n")
cat("  Grid: 11 points in (-0.99, 0.99).\n\n")

diag_cases <- list(
  list(
    label       = "Run 1: Baseline (valid instrument, alpha2=1)",
    alpha2      = ALPHA2, rho = TRUE_RHO, beta_x2 = 0,
    nonnormal   = FALSE,  form = FORM1,  instrument = "xs2"
  ),
  list(
    label       = "Run 2: High correlation (rho=0.99, valid instrument)",
    alpha2      = ALPHA2, rho = 0.99,    beta_x2 = 0,
    nonnormal   = FALSE,  form = FORM1,  instrument = "xs2"
  ),
  list(
    label       = "Run 4: Non-normal shocks (valid instrument)",
    alpha2      = ALPHA2, rho = TRUE_RHO, beta_x2 = 0,
    nonnormal   = TRUE,   form = FORM1,  instrument = "xs2"
  ),
  list(
    label       = "Run 5: No excl. restriction (w=x=[x1], no instrument)",
    alpha2      = 0,      rho = TRUE_RHO, beta_x2 = 0,
    nonnormal   = FALSE,  form = FORM5,  instrument = NULL
  ),
  list(
    label       = "Run 6: Excl. restriction not satisfied (x2 in outcome)",
    alpha2      = ALPHA2, rho = TRUE_RHO, beta_x2 = 1,
    nonnormal   = FALSE,  form = FORM1,  instrument = "xs2"
  ),
  list(
    label       = "Run 7: Weak instrument (alpha2=0, x2 claimed instrument)",
    alpha2      = 0,      rho = TRUE_RHO, beta_x2 = 0,
    nonnormal   = FALSE,  form = FORM1,  instrument = "xs2"
  )
)

for (cas in diag_cases) {
  cat(sprintf("\n---------- %s ----------\n", cas$label))
  dat <- dgp(1000, alpha2 = cas$alpha2, rho = cas$rho,
             beta_x2_out = cas$beta_x2, nonnormal = cas$nonnormal)
  cat("Regime counts:"); print(table(dat$ys))

  fit <- suppressWarnings(opsr(cas$form, dat, printLevel = 0))

  dg <- opsr_diagnostics(fit)
  cat(sprintf("opsr_diagnostics(): hess_negdef=%s  cond_number=%.1f  max_grad=%.2e\n",
              dg$hessian_neg_def, dg$condition_number, dg$max_abs_gradient))

  if (!is.null(cas$instrument)) {
    test <- opsr_test_exclusion_restriction(
      fit, instrument = cas$instrument,
      grid = seq(-0.99, 0.99, length.out = 11),
      printLevel = 0
    )
    print(test)

    # Summarise profile curvature per regime
    prof <- test$profile_rho
    cat("Profile LL drop from MLE (curvature indicator):\n")
    for (j in 1:3) {
      sub <- prof[prof$regime == j, ]
      cat(sprintf("  Regime %d: range=%.2f  drop from MLE=%.2f  %s\n",
                  j,
                  diff(range(sub$profile_loglik, na.rm = TRUE)),
                  max(sub$profile_loglik, na.rm=TRUE) - min(sub$profile_loglik, na.rm=TRUE),
                  if (diff(range(sub$profile_loglik, na.rm=TRUE)) < 5)
                    "[FLAT - weak/absent identification]"
                  else "[curved - identification present]"))
    }
  } else {
    cat("No instrument to test (w=x=[x1]).\n")
    cat("Running opsr_profile_rho() to assess rho identifiability:\n")
    prof <- opsr_profile_rho(fit, grid = seq(-0.99, 0.99, length.out = 11),
                             printLevel = 0)
    print(prof)
    cat("Profile LL curvature:\n")
    for (j in 1:3) {
      sub <- prof[prof$regime == j, ]
      cat(sprintf("  Regime %d: range=%.2f  %s\n", j,
                  diff(range(sub$profile_loglik, na.rm=TRUE)),
                  if (diff(range(sub$profile_loglik, na.rm=TRUE)) < 5)
                    "[FLAT]" else "[curved]"))
    }
  }
}

cat("\n=== Simulation study complete ===\n")
