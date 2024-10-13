devtools::load_all()

rm(list = ls())

opsr_max_threads()

## test opsr
sim_dat <- opsr_simulate(n = 1e3)
dat <- sim_dat$data
formula <- ys | yo ~ xs1 + xs2 | xo1 + xo2 | xo1 + xo2 | xo1 + xo2
formula <- ys | yo ~ xs1 + xs2 | xo1 + xo2  # equivalent to above
system.time(
  fit_nm <- opsr(formula, dat, method = "NM", iterlim = 10e3, nThreads = 3)
)
system.time(
  fit_bfgs <- opsr(formula, dat, method = "BFGS", nThreads = 3)
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

## predict
devtools::load_all()

p <- predict(fit_xinyi, group = 1, type = "response")
p_unlog <- predict(fit_xinyi, group = 1, type = "unlog-response")
p_unlog_counterfact <- predict(fit_xinyi, group = 1, counterfact = 2, type = "unlog-response")
plot(p_unlog, p_unlog_counterfact)
p_prob <- predict(fit_xinyi, group = 1, type = "prob")  # abbr. are allowed for type
p_prob_1 <- predict(fit_xinyi, group = 1, counterfact = 1, type = "prob")
all(p_prob == p_prob_1, na.rm = TRUE)
p_prob_2 <- predict(fit_xinyi, group = 1, counterfact = 2, type = "prob")
p_prob_3 <- predict(fit_xinyi, group = 1, counterfact = 3, type = "prob")
test <- data.frame(cbind(p_prob_1, p_prob_2, p_prob_3))
test$sum <- rowSums(test)
all(round(test$sum, 4) == 1, na.rm = TRUE)
mills <- predict(fit_xinyi, group = 1, type = "mills")
newdat <- telework_data[1, ]
p_new <- predict(fit_xinyi, newdat, group = 3, type = "response")
p_new_unlog <- predict(fit_xinyi, newdat, group = 3, type = "unlog-response")
p_new_unlog == predict(fit_xinyi, group = 3, type = "unlog-response")[1]




## some extractor methods
## fitted and residuals
n <- 1000
x <- rnorm(n)
y <- 1 + 2 * x + rnorm(n)
df <- data.frame(x = x, y = y)
f <- y ~ x
fit_lm <- lm(f, data = df)
summary(fit_lm)
all.equal(fitted(fit_lm), predict(fit_lm))
debugonce(fitted)
fitted(fit_lm)

residuals(fit_lm)
my_res <- df$y - fitted(fit_lm)
all.equal(residuals(fit_lm), my_res)

devtools::load_all()
sim_dat <- opsr_simulate()
dat <- sim_dat$data
fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat)
fitted(fit)
residuals(fit)
plot(fitted(fit), residuals(fit))

test <- data.frame(
  group = get_z(fit_xinyi),
  f = fitted(fit_xinyi),
  r = residuals(fit_xinyi)
)

plot(r ~ f, data = test, col = test$group)
legend("bottomleft", legend = unique(test$group), col = unique(test$group), pch = 1)

## update
update(fit_lm, . ~ . -1) -> test
debugonce(opsr)
fit_updated <- update(fit, ~ . | . -1)



## PROCEED HERE ##
## anova
?anova
?anova.lm
?anova.glm
## => should we use analysis of residual deviance or residual sum of squares for opsr?
## what does sampleSelection do? => no anova method
## => use likelihood based metric (residual deviance) to test the full model!
## => so anova.glm seems more relevant
debugonce(anova)
stats:::anova.lm
stats:::anova.lmlist
anova(fit_lm, test)
anova(fit, fit_updated)

## example probit
data(mtcars)
mtcars$am <- as.factor(mtcars$am)
model1 <- glm(am ~ hp, family = binomial(link = "probit"), data = mtcars)  # reduced model
model2 <- glm(am ~ hp + wt, family = binomial(link = "probit"), data = mtcars)  # full model
anova(model1, model2, test = "Chisq")
## => so residual deviance is smaller by 31.547 units in model2 and H0 is this is
## not different from 0. However, the Pr(>Chi) value indicates that we reject H0
## i.e., model2 is the preferred one!

## => take-away: will implement deviance based Chisq test
s <- summary(model1)
s$deviance
s$dispersion

## TODO:
## compute these (deviance, dispersion) in summary.opsr
## maybe update residuals.opsr(object, type, ...) to allow for type = "deviance"
## implement in similar architecture: anova.opsr, anova.opsrlist, stat.anova
## conform to the returned object classes

debugonce(stats:::anova.glm)
anova(model1)
## => anova prepares the table input for stat.anova (where the main magic happens)
debugonce(stats:::anova.glmlist)
anova(model1, model2, test = "Chisq")
## => anova simply intersects if more than one model is passed (via ...) and then
## forwards to anova.glmlist which again prepares the table input for stat.anova
debugonce(stats:::stat.anova)
## => chisq test seems pretty trivial => call to pchisq
anova(model1)
debugonce(stats:::stat.anova)
anova(model1, model2)

anova(model1, model2)

## compare to wald test => but deviance based test (LRT) and tests on model coefficients are not the same!
anova(model1)
h <- coefficients(model1)
h <- h[names(h) != "(Intercept)"]
car::linearHypothesis(model1, names(h), vcov. = sandwich::sandwich(model1))
model3 <- glm(am ~ 1, family = binomial(link = "probit"), data = mtcars)
anova(model3, model1)
lmtest::waldtest(model3, model1)
lmtest::waldtest(model1)

anova(model1)  # Pr(>Chi) = 0.1771
lmtest::waldtest(model1)  # Pr(>Chisq) = 0.2253
car::linearHypothesis(model1, "hp")  # Pr(>Chisq) = 0.2157
## => waldtest and linearHypothesis should (approx.) yield the same which they do...
