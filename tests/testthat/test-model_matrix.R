test_that("returns n x 1 matrix", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  fit <- opsr(ys | yo ~ xs1 + xs2 | 1, dat, printLevel = 0)
  mm <- model.matrix(fit)
  test <- unlist(lapply(mm$X, is.matrix))
  expect_true(all(test))
})
