devtools::load_all()

rm(list = ls())

sim_dat <- sim_dat_1()
dat <- sim_dat$data
formula <- Z | Y ~ X1 + X2 | -1 + X1 + X2 | -1 + X1 + X2 | -1 + X1 + X2
formula <- Z | Y ~ X1 + X2 | -1 + X1 + X2
system.time(
  fit <- opsr(formula, dat)
)
summary(fit)
class(fit)

## ground truth
sim_dat$params
sim_dat$sigma


sim_dat <- sim_dat_1()
dat <- sim_dat$data
W <- as.matrix(dat[, c("X1", "X2")])
Z <- dat$Z
Y <- dat$Y
nReg <- length(unique(Z))
Xs <- lapply(seq_len(nReg), function(i) {
  X <- W
  X[Z == i, ]
})
Ys <- lapply(seq_len(nReg), function(i) {
  Y[Z == i]
})
opsr_generate_start(W, Xs, Z, Ys)
