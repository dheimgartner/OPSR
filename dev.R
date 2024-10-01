devtools::load_all()

rm(list = ls())

## test opsr
sim_dat <- sim_dat_1()
dat <- sim_dat$data
formula <- Z | Y ~ X1 + X2 | -1 + X1 + X2 | -1 + X1 + X2 | -1 + X1 + X2
formula <- Z | Y ~ X1 + X2 | -1 + X1 + X2
system.time(
  fit_nm <- opsr(formula, dat, method = "NM")
)
system.time(
  fit_bfgs <- opsr(formula, dat, method = "BFGS")
)
summary(fit_nm)
summary(fit_bfgs)
class(fit_bfgs)

## ground truth
sim_dat$params
sim_dat$sigma

## test generation of starting values
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

## predict
devtools::load_all()
predict(fit_nm, j = 1)
