devtools::load_all()

rm(list = ls())

## test opsr
sim_dat <- opsr_simulate()
dat <- sim_dat$data
formula <- ys | yo ~ xs1 + xs2 | xo1 + xo2 | xo1 + xo2 | xo1 + xo2
formula <- ys | yo ~ xs1 + xs2 | xo1 + xo2  # equivalent to above
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
sim_dat <- opsr_simulate()
dat <- sim_dat$data
W <- as.matrix(dat[, c("xs1", "xs2")])
Z <- dat$ys
Y <- dat$yo
nReg <- length(unique(Z))
Xs <- lapply(seq_len(nReg), function(i) {
  X <- W
  X[Z == i, ]
})
Ys <- lapply(seq_len(nReg), function(i) {
  Y[Z == i]
})
opsr_generate_start(W, Xs, Z, Ys)

## test predict
devtools::load_all()

summary(fit_bfgs)
yo <- 2
p_pred <- predict(fit_bfgs, yo = yo)
p_true <- dat$yo[dat$ys == yo]
plot(p_pred, p_true)

p_counterfact <- predict(fit_bfgs, yo = yo, ys = yo + 1)
ref <- dat$yo[dat$ys == yo + 1]
mean(ref)
mean(p_counterfact)

## without error correlation (compare to lm)
sigma <- diag(1, nrow = 4, ncol = 4)
sim_dat_no_cor <- opsr_simulate(sigma = sigma)
dat <- sim_dat_no_cor$data
formula <- ys | yo ~ xs1 + xs2 | xo1 + xo2 | xo1 + xo2 | xo1 + xo2
fit_no_cor <- opsr(formula, data = dat, method = "BFGS")
summary(fit_no_cor)
j <- 1
idx <- dat$ys == j
p_no_cor <- predict(fit_no_cor, yo = j)
fit_lm <- lm(yo ~ xo1 + xo2, data = dat, subset = idx)
summary(fit_lm)
p_lm <- predict(fit_lm)

df <- data.frame(p_opsr = p_no_cor, p_lm = p_lm)
plot(df)




