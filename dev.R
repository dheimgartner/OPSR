devtools::load_all()

rm(list = ls())

## test opsr
sim_dat <- opsr_simulate(n = 10e3)
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
sloop::s3_methods_class("opsr")

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


## factors seem to be handled nicely
## (first level as reference)
sim_dat <- opsr_simulate()
dat <- sim_dat$data
dat$xo3 <- ifelse(dat$xo2 > 0, "a", "b")
fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + factor(xo3), data = dat)
model.frame(fit)
model.matrix(fit)

## what about other transformations
sim_dat <- opsr_simulate()
dat <- sim_dat$data
fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + I(xo2**2) + log(xo1), data = dat)
summary(fit)
model.frame(fit)
model.matrix(fit)









## opsr_check_start
devtools::load_all()
f <- ys | yo ~ xs1 | xo1 | xo1
start <- c(1, 2, 3, 3, 4, 4, 5, 5, 6, 6)
debugonce(opsr_check_start)
opsr_check_start(f, start)


devtools::load_all()
sim_dat <- opsr_simulate()
dat <- sim_dat$data
f <- ys | yo ~ xs1 + xs2 | xo1 + xo2 | xo1 + xo2 | xo1 + xo2
debugonce(opsr_check_start)
fit <- opsr(f, data = dat, start = c(1, 2, 3))



library(Rcpp)
library(RcppArmadillo)
cpp <- "
arma::mat extract_mat(arma::field<arma::mat> X, int index) {
  return X[index];
}
"
cppFunction(cpp, depends = "RcppArmadillo")
x <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, byrow = TRUE)
y <- x + 1
extract_mat(list(x, y), 0)





## sandwich
devtools::load_all()
library(sandwich)

sim_dat <- opsr_simulate()
dat <- sim_dat$data
formula <- ys | yo ~ xs1 + xs2 | xo1 + xo2
fit <- opsr(formula, dat, method = "BFGS", weights = rep(1, 1000))
summary(fit)
sandwich(fit)
?sandwich  # defaults to HC (heteroskedasticity consistent; Eicker-Huber-White estimator) of the covariance matrix S(theta)

## bread
debugonce()
bread(fit)
vcov(fit)
bread(fit) == vcov(fit) * 1000
my_bread <- vcov(fit) * 1000

## meat
dim(estfun(fit))
estfun(fit) == fit$gradientObs
meat(fit)
my_meat <- 1 / 1000 * crossprod(as.matrix(fit$gradientObs))
round(meat(fit), 5) == round(my_meat, 5)

## sandwich
sandwich(fit)  # robust
vcov(fit)  # not robust
lmtest::coeftest(fit, vcov(fit))
lmtest::coeftest(fit, sandwich(fit))
my_sandwich <- 1 / 1000 * (bread(fit) %*% meat(fit) %*% bread(fit))
sandwich(fit) == my_sandwich




## summary.opsr
devtools::load_all()

sim_dat <- opsr_simulate()
dat <- sim_dat$data
formula <- ys | yo ~ xs1 + xs2 | xo1 + xo2
fit <- opsr(formula, dat)
summary(fit)
maxLik:::summary.maxLik(fit)

start <- fit$start
start["rho1"] <- start["rho2"] <- start["rho3"] <- 0
fit2 <- opsr(formula, dat, start = start, fixed = c("rho1", "rho2", "rho3"))
summary(fit2)
fit$finalLL
fit2$finalLL


n <- 1000
x <- rnorm(n)
y <- 1 + 2 * x + rnorm(n)
df <- data.frame(x = x, y = y)
f <- y ~ x
fit_lm <- lm(f, data = df)
summary(fit_lm)
debugonce(summary.lm)





## wald test
?waldtest
model <- lm(mpg ~ disp + hp + wt, data = mtcars)
wald_result <- waldtest(model, terms = c("disp", "hp"))
## waldtest actually runs update() and then refits the model
wald_result

## using car
linearHypothesis(fit, c("rho1 = 0", "rho2 = 0", "rho3 = 0"))

coefs <- names(coef(fit))
## test against the null model
linearHypothesis(fit, coefs)
linearHypothesis(fit, coefs, vcov. = sandwich::sandwich)
## we probably should keep intercepts and cutoff params
keep <- grepl("^kappa|^sigma|^rho|(Intercept)", coefs)
coefs[keep]
linearHypothesis(fit, coefs[!keep])

## no error correlation => wald (rho) => does not reject H0
sigma <- diag(1, nrow = 4)
sim_dat <- opsr_simulate(nobs = 1e3, sigma = sigma)
dat <- sim_dat$data
fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat)
fit$maximum
summary(fit)



## null model
devtools::load_all()
sim_dat <- opsr_simulate()
dat <- sim_dat$data
fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat)
fit$maximum
summary(fit)

fit_null <- opsr_null_model(fit)
summary(fit_null)
fit_null$maximum

mean(dat[dat$ys == 1, "yo"])
mean(dat[dat$ys == 2, "yo"])




## xinyi's model
f <- C9b_TWer | E53_ln ~
  # selection model specification
  edu_2 + edu_3 + hhincome_2 + hhincome_3 +
  B2b_6 + worker_ft_B3b_binary + C5min_month +
  FAC_A4_proactivemode_sd + FAC_A6_procarowning_sd +
  FAC_B1_WIF_sd + FAC_B2_proteamwork_sd +
  FAC_C1_tw_effective_teamwork_sd + FAC_C5_tw_enthusiasm_sd + FAC_C7_tw_location_flex_sd |
  # outcome model NTW specification
  sex_imp_mf + age_mean + age_mean_sq +
  race_3_imp_mf_2 + race_3_imp_mf_3 +
  E2 + D2_2 + D2_3 + D2_4 +
  worker_ft_B3b_binary +
  FAC_A2_prolargehouse_sd + FAC_A6_procarowning_sd +
  region_WAA |
  # outcome model NUTW specification
  edu_2 + edu_3 + D2_2 + D2_3 +  D2_4 +
  worker_ft_B3b_binary +
  FAC_A2_prolargehouse_sd + FAC_A4_proactivemode_sd + FAC_A6_procarowning_sd |
  # outcome model UTW specification
  sex_imp_mf + hhincome_2 + hhincome_3 +
  D11bcd + D2_2 + D2_3 + D2_4 +
  FAC_A6_procarowning_sd +
  region_WAA

fit_xinyi <- opsr(f, OPSR::telework_data)
