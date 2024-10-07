devtools::load_all()

rm(list = ls())

## test opsr
sim_dat <- opsr_simulate()
dat <- sim_dat$data
formula <- ys | yo ~ xs1 + xs2 | xo1 + xo2 | xo1 + xo2 | xo1 + xo2
formula <- ys | yo ~ xs1 + xs2 | xo1 + xo2  # equivalent to above
system.time(
  fit_nm <- opsr(formula, dat, method = "NM", iterlim = 10e3)
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
group <- 1
p_pred <- predict(fit_bfgs, group = group)
p_true <- dat$yo[dat$ys == group]
plot(p_pred, p_true)

counterfact <- 3
p_counterfact <- predict(fit_bfgs, group = group, counterfact = counterfact)
ref <- dat$yo[dat$ys == counterfact]
mean(ref)
mean(p_counterfact)

## with new data
newdat <- dat[dat$ys == group, ][1, ]
p_newdat <- predict(fit_bfgs, newdata = newdat, group = group)
newdat$yo

## without error correlation (compare to lm)
sigma <- diag(1, nrow = 4, ncol = 4)
sim_dat_no_cor <- opsr_simulate(sigma = sigma)
dat <- sim_dat_no_cor$data
formula <- ys | yo ~ xs1 + xs2 | xo1 + xo2
fit_no_cor <- opsr(formula, data = dat, method = "BFGS")
summary(fit_no_cor)
group <- 1
idx <- dat$ys == group
p_no_cor <- predict(fit_no_cor, group = group)
fit_lm <- lm(yo ~ xo1 + xo2, data = dat, subset = idx)
summary(fit_lm)
p_lm <- predict(fit_lm)

df <- data.frame(p_opsr = p_no_cor, p_lm = p_lm)
plot(df)

## model.matrix.opsr
devtools::load_all()

sim_dat <- opsr_simulate()
dat <- sim_dat$data
formula <- ys | yo ~ xs1 + xs2 | xo1 + xo2
fit <- opsr(formula, data = dat, subset = TRUE)
summary(fit)

debugonce(opsr_model_matrices)
test <- dat[1:10, ]
model.matrix(fit, filter = 3)


dat_lm <- dat[dat$ys == 1, ]
fit_lm <- lm(yo ~ xo1 + xo2, data = dat_lm)
debugonce(model.matrix.lm)
debugonce(model.frame)
model.matrix(fit_lm)



library(Rcpp)

cpp <- "
NumericVector test(int len) {
  NumericVector x(len);
  return x;
}
"
cppFunction(cpp)
x <- test(100)





