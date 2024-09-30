devtools::load_all()

rm(list = ls())

sim_dat <- sim_dat_1()
dat <- sim_dat$data
formula <- Z | Y ~ X1 + X2 | -1 + X1 + X2 | -1 + X1 + X2 | -1 + X1 + X2
formula <- Z | Y ~ -1 + X1 + X2
system.time(
  fit <- opsr(formula, dat)
)
summary(fit)
class(fit)

## ground truth
sim_dat$params
sim_dat$sigma
