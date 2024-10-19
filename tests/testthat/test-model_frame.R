test_that("finds data if no new data is passed", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  fit <- opsr(ys | yo ~ xs1 + xs2 | 1, dat, printLevel = 0)
  mf1 <- model.frame(fit, data = dat)
  expect_no_error(model.frame(fit))
  mf2 <- model.frame(fit)
  expect_identical(mf1, mf2)
})
