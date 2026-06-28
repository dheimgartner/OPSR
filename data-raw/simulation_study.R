## Replication of Chiburis & Lokshin (2007) Monte Carlo simulations
## Stata Journal 7(2): 167-182, Section 6
##
## Produces `simulation_study`, a list with two data frames:
##   $mc   -- Monte Carlo results (bias, SD, 95% CI coverage) per run /
##             estimator / regime
##   $diag -- opsr_test_exclusion_restriction() diagnostics per run
##
## Both FIML (opsr default) and the Heckman two-step are reported.
## Two-step SEs come from the step-2 OLS, which are known to be incorrect
## (generated-regressor + heteroskedasticity); they are included here for
## comparison exactly as in the paper.

library(OPSR)
library(mvtnorm)
library(MASS)

set.seed(2007)

## =========================================================
## 0.  DGP constants
## =========================================================
TRUE_BETA <- 1
TRUE_RHO  <- 0.5
ALPHA1    <- 1
ALPHA2    <- 1
KAPPA     <- c(-1, 1)

make_sigma <- function(rho) {
  c <- rho
  matrix(c(1, c, c, c,
           c, 1, 0, 0,
           c, 0, 1, 0,
           c, 0, 0, 1), ncol = 4)
}

dgp <- function(nobs, alpha2 = ALPHA2, rho = TRUE_RHO,
                beta_x2_out = 0, nonnormal = FALSE) {
  x1 <- rnorm(nobs); x2 <- rnorm(nobs)
  raw <- rmvnorm(nobs, mean = rep(0, 4), sigma = make_sigma(rho))
  err <- if (nonnormal) raw^2 - 1 else raw
  u <- err[, 1]; e1 <- err[, 2]; e2 <- err[, 3]; e3 <- err[, 4]

  z_star <- ALPHA1 * x1 + alpha2 * x2 + u
  ys     <- findInterval(z_star, c(-Inf, KAPPA, Inf))
  yb     <- TRUE_BETA * x1 + beta_x2_out * x2
  yo     <- ifelse(ys == 1, yb + e1, ifelse(ys == 2, yb + e2, yb + e3))
  data.frame(ys = ys, yo = yo, xs1 = x1, xs2 = x2)
}

## =========================================================
## 1.  FIML helper -- returns per-regime (beta, cover)
## =========================================================
fit_fiml <- function(dat, formula, true_beta = TRUE_BETA) {
  if (length(unique(dat$ys)) < 3 || min(table(dat$ys)) < 5) return(NULL)
  fit <- tryCatch(suppressWarnings(opsr(formula, dat, printLevel = 0)),
                  error = function(e) NULL)
  if (is.null(fit)) return(NULL)

  cf <- coef(fit)
  vc <- tryCatch(vcov(fit), error = function(e) NULL)

  lapply(1:3, function(j) {
    nm <- paste0("o", j, "_xs1")
    b  <- cf[nm]
    se <- if (!is.null(vc) && nm %in% rownames(vc)) sqrt(vc[nm, nm]) else NA
    cover <- if (!is.na(se)) {
      (b - 1.96 * se <= true_beta) & (true_beta <= b + 1.96 * se)
    } else NA
    list(beta = b, se = se, cover = cover)
  })
}

## =========================================================
## 2.  Two-step helper -- replicates opsr_2step() logic and
##     captures step-2 lm() SEs (known to be incorrect)
## =========================================================
fit_twostep <- function(dat, sel_vars, true_beta = TRUE_BETA) {
  if (length(unique(dat$ys)) < 3 || min(table(dat$ys)) < 5) return(NULL)

  ## Step 1: ordered probit
  sel_form <- as.formula(paste("factor(ys) ~", paste(sel_vars, collapse = "+")))
  fit1 <- tryCatch(
    suppressWarnings(MASS::polr(sel_form, data = dat, method = "probit")),
    error = function(e) NULL)
  if (is.null(fit1)) return(NULL)

  kappa_ <- c(-Inf, fit1$zeta, Inf)
  gamma  <- setNames(fit1$coefficients, sel_vars)

  ## IMR per observation
  W   <- as.matrix(dat[, sel_vars, drop = FALSE])
  Wg  <- as.numeric(W %*% gamma)
  z   <- dat$ys
  lo  <- kappa_[z]; hi <- kappa_[z + 1]
  IMR <- -(dnorm(hi - Wg) - dnorm(lo - Wg)) /
         (pnorm(hi - Wg) - pnorm(lo - Wg))

  ## Step 2: separate lm per regime
  lapply(1:3, function(j) {
    idx <- dat$ys == j
    lm_dat <- data.frame(yo = dat$yo[idx], xs1 = dat$xs1[idx], imr = IMR[idx])
    fit2 <- tryCatch(
      suppressWarnings(lm(yo ~ xs1 + imr, data = lm_dat)),
      error = function(e) NULL)
    if (is.null(fit2)) return(list(beta = NA, se = NA, cover = NA))

    cf <- coef(fit2)
    vc <- tryCatch(vcov(fit2), error = function(e) NULL)
    b  <- cf["xs1"]
    se <- if (!is.null(vc) && "xs1" %in% rownames(vc)) sqrt(vc["xs1", "xs1"]) else NA
    ## drop infeasible: |rho_hat| > 1 (Two-step* in paper)
    rho_hat <- cf["imr"] / sqrt(sum(residuals(fit2)^2) / (sum(idx) - 3))
    if (!is.finite(b) || abs(b) > 50) return(list(beta = NA, se = NA, cover = NA))
    cover <- if (!is.na(se) && is.finite(se)) {
      (b - 1.96 * se <= true_beta) & (true_beta <= b + 1.96 * se)
    } else NA
    list(beta = b, se = se, cover = cover)
  })
}

## =========================================================
## 3.  Summarise R replications into one row per
##     (run, n_obs, estimator, regime)
## =========================================================
summarize_cond <- function(reps_fiml, reps_2step, run, n_obs,
                           true_beta = TRUE_BETA) {
  rows <- list()
  for (est_name in c("FIML", "Two-step")) {
    reps <- if (est_name == "FIML") reps_fiml else reps_2step
    reps <- Filter(Negate(is.null), reps)
    for (j in 1:3) {
      betas  <- sapply(reps, function(r) r[[j]]$beta)
      covers <- sapply(reps, function(r) r[[j]]$cover)
      betas  <- betas[is.finite(betas)]
      rows[[length(rows) + 1]] <- data.frame(
        run        = run,
        n_obs      = n_obs,
        estimator  = est_name,
        regime     = j,
        true_beta  = true_beta,
        n_ok       = length(betas),
        mean_beta  = mean(betas, na.rm = TRUE),
        sd_beta    = sd(betas,   na.rm = TRUE),
        bias       = mean(betas, na.rm = TRUE) - true_beta,
        coverage   = mean(covers, na.rm = TRUE) * 100
      )
    }
  }
  do.call(rbind, rows)
}

## =========================================================
## 4.  Run one MC condition
## =========================================================
run_mc <- function(run, n_vals, R, formula, sel_vars,
                   alpha2 = ALPHA2, rho = TRUE_RHO,
                   beta_x2_out = 0, nonnormal = FALSE,
                   true_beta = TRUE_BETA) {
  do.call(rbind, lapply(n_vals, function(n) {
    cat(sprintf("  %s  N=%-5d", run, n))
    reps_fiml  <- vector("list", R)
    reps_2step <- vector("list", R)
    for (i in seq_len(R)) {
      dat <- dgp(n, alpha2 = alpha2, rho = rho,
                 beta_x2_out = beta_x2_out, nonnormal = nonnormal)
      reps_fiml[[i]]  <- fit_fiml(dat, formula, true_beta)
      reps_2step[[i]] <- fit_twostep(dat, sel_vars, true_beta)
    }
    cat(sprintf("  FIML ok: %d  2step ok: %d\n",
                sum(!sapply(reps_fiml,  is.null)),
                sum(!sapply(reps_2step, is.null))))
    summarize_cond(reps_fiml, reps_2step, run, n, true_beta)
  }))
}

## =========================================================
## 5.  MC conditions
## =========================================================
R      <- 100
FORM1  <- ys | yo ~ xs1 + xs2 | xs1
FORM5  <- ys | yo ~ xs1       | xs1
SEL12  <- c("xs1", "xs2")
SEL1   <- "xs1"

cat("=== Run 1: Baseline ===\n")
mc1 <- run_mc("1_baseline", c(50, 100, 200, 300, 500, 1000), R,
              FORM1, SEL12)

cat("=== Run 2: High correlation (rho=0.99) ===\n")
mc2 <- run_mc("2_high_rho", 1000, R, FORM1, SEL12, rho = 0.99)

cat("=== Run 4: Non-normal shocks ===\n")
mc4 <- run_mc("4_nonnormal", 1000, R, FORM1, SEL12, nonnormal = TRUE)

cat("=== Run 5: No exclusion restriction ===\n")
mc5 <- run_mc("5_no_excl", 1000, R, FORM5, SEL1, alpha2 = 0)

cat("=== Run 6: Exclusion restriction not satisfied ===\n")
mc6 <- run_mc("6_excl_violated", 1000, R, FORM1, SEL12, beta_x2_out = 1)

cat("=== Run 7: Weak instrument (alpha2=0) ===\n")
mc7 <- run_mc("7_weak_inst", 1000, R, FORM1, SEL12, alpha2 = 0)

mc <- rbind(mc1, mc2, mc4, mc5, mc6, mc7)
mc$run       <- factor(mc$run, levels = unique(mc$run))
mc$estimator <- factor(mc$estimator, levels = c("FIML", "Two-step"))

## =========================================================
## 6.  Diagnostics: opsr_test_exclusion_restriction()
##     One representative dataset per run (N=1000)
## =========================================================
cat("\n=== Diagnostics (one dataset per run) ===\n")

diag_cases <- list(
  list(run="1_baseline",      alpha2=ALPHA2, rho=TRUE_RHO, bx2=0, nn=FALSE,
       form=FORM1, instrument="xs2"),
  list(run="2_high_rho",      alpha2=ALPHA2, rho=0.99,     bx2=0, nn=FALSE,
       form=FORM1, instrument="xs2"),
  list(run="4_nonnormal",     alpha2=ALPHA2, rho=TRUE_RHO, bx2=0, nn=TRUE,
       form=FORM1, instrument="xs2"),
  list(run="5_no_excl",       alpha2=0,      rho=TRUE_RHO, bx2=0, nn=FALSE,
       form=FORM5, instrument=NULL),
  list(run="6_excl_violated", alpha2=ALPHA2, rho=TRUE_RHO, bx2=1, nn=FALSE,
       form=FORM1, instrument="xs2"),
  list(run="7_weak_inst",     alpha2=0,      rho=TRUE_RHO, bx2=0, nn=FALSE,
       form=FORM1, instrument="xs2")
)

## opsr_get_all_vars() looks for the data name in the formula's env (globalenv
## when the formula was defined at top level). Use a for-loop so dat is
## assigned there rather than inside a closure.
diag_rows <- list()
for (.cas in diag_cases) {
  cat(sprintf("  %s\n", .cas$run))
  dat <- dgp(1000, alpha2 = .cas$alpha2, rho = .cas$rho,
             beta_x2_out = .cas$bx2, nonnormal = .cas$nn)
  fit <- suppressWarnings(opsr(.cas$form, dat, printLevel = 0))
  dg  <- opsr_diagnostics(fit)

  prof <- opsr_profile_rho(fit, grid = seq(-0.99, 0.99, length.out = 11),
                           printLevel = 0)

  lr_chisq <- rep(NA_real_, 3)
  lr_pval  <- rep(NA_real_, 3)
  if (!is.null(.cas$instrument)) {
    lr <- opsr_lr_instrument(fit, instrument = .cas$instrument, printLevel = 0)
    lr_chisq[] <- lr$table$Test[2]
    lr_pval[]  <- lr$table$`Pr(>Chi)`[2]
  }

  diag_rows[[.cas$run]] <- do.call(rbind, lapply(1:3, function(j) {
    sub <- prof[prof$regime == j, ]
    data.frame(
      run           = .cas$run,
      regime        = j,
      cond_number   = dg$condition_number,
      max_grad      = dg$max_abs_gradient,
      hess_negdef   = dg$hessian_neg_def,
      profile_range = diff(range(sub$profile_loglik, na.rm = TRUE)),
      lr_chisq      = lr_chisq[j],
      lr_pval       = lr_pval[j]
    )
  }))
}

diag <- do.call(rbind, diag_rows)
diag$run <- factor(diag$run, levels = unique(diag$run))

## =========================================================
## 7.  Bundle and save
## =========================================================
simulation_study <- list(mc = mc, diag = diag)
usethis::use_data(simulation_study, overwrite = TRUE)
cat("\nSaved to data/simulation_study.rda\n")
