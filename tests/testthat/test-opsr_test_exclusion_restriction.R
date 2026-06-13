test_that("opsr_lr_instrument compares restricted vs full model", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, printLevel = 0)

  expect_no_error(lr <- opsr_lr_instrument(fit, instrument = "xs2", printLevel = 0))
  expect_s3_class(lr, "anova.opsr")
  expect_equal(nrow(lr$table), 2)
  ## restricted model (instrument fixed at 0) is nested -> smaller logLik and Df
  expect_lt(lr$table[1, "logLik"], lr$table[2, "logLik"])
  expect_equal(lr$table[2, "Df"] - lr$table[1, "Df"], 1)
  ## restriction metadata drives the annotation in print.anova.opsr
  expect_equal(lr$restriction$row, 1L)
  expect_equal(lr$restriction$instrument, "xs2")
  expect_output(print(lr), "restricted to 0 in selection")
})

test_that("opsr_lr_instrument errors informatively on unknown instrument", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, printLevel = 0)

  expect_error(
    opsr_lr_instrument(fit, instrument = "not_a_var", printLevel = 0),
    "not found in selection equation"
  )
})

test_that("opsr_profile_rho returns a per-regime profile", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, printLevel = 0)

  grid <- seq(-0.8, 0.8, length.out = 5)
  expect_no_error(
    pr <- suppressMessages(opsr_profile_rho(fit, grid = grid, printLevel = 0))
  )
  expect_s3_class(pr, "opsr.profile.rho")
  expect_named(pr, c("regime", "rho", "profile_loglik"))
  expect_equal(nrow(pr), fit$nReg * length(grid))
  expect_equal(sort(unique(pr$regime)), seq_len(fit$nReg))
  ## profile log-likelihood never exceeds the unrestricted maximum
  expect_true(all(pr$profile_loglik <= attr(pr, "max_loglik") + 1e-6, na.rm = TRUE))
  expect_equal(attr(pr, "max_loglik"), fit$maximum)
  expect_output(print(pr), "Profile log-likelihood for rho")
})

test_that("opsr_test_exclusion_restriction bundles both diagnostics", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, printLevel = 0)

  expect_no_error(
    test <- suppressMessages(
      opsr_test_exclusion_restriction(fit, instrument = "xs2",
                                      grid = seq(-0.8, 0.8, length.out = 5),
                                      printLevel = 0)
    )
  )
  expect_s3_class(test, "opsr.exclusion.test")
  expect_s3_class(test$lr_test, "anova.opsr")
  expect_s3_class(test$profile_rho, "opsr.profile.rho")
  expect_equal(test$instrument, "xs2")
  expect_output(print(test), "Exclusion restriction diagnostics")
})
