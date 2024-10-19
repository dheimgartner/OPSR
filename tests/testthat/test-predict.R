test_that("prediction works on intercept-only model (continuous outcome)", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  fit <- opsr(ys | yo ~ xs1 + xs2 | 1, dat, printLevel = 0)
  expect_no_error(predict(fit, group = 1))
  expect_no_error(predict(fit, group = 2))
  expect_no_error(predict(fit, group = 3))
  expect_no_error(predict(fit, group = 1, counterfact = 3))
  expect_no_error(predict(fit, group = 1, type = "prob"))
  expect_no_error(predict(fit, group = 1, type = "unlog-response"))
})

## test output length

## test NAs align
