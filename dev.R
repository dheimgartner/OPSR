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
opsr_2step(W, Xs, Z, Ys)

## test predict
devtools::load_all()

summary(fit_bfgs)
group <- 1
p_pred <- predict(fit_bfgs, group = group)
p_true <- dat$yo[dat$ys == group]
plot(na.omit(p_pred), p_true)

counterfact <- 2
p_counterfact <- predict(fit_bfgs, group = group, counterfact = counterfact)
ref <- dat$yo[dat$ys == counterfact]
mean(ref)
mean(p_counterfact, na.rm = TRUE)

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

df <- data.frame(p_opsr = na.omit(p_no_cor), p_lm = p_lm)
plot(df)

## model.matrix.opsr
devtools::load_all()

sim_dat <- opsr_simulate()
dat <- sim_dat$data
formula <- ys | yo ~ xs1 + xs2 | xo1 + xo2
fit <- opsr(formula, data = dat, subset = TRUE)
summary(fit)

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









# ## opsr_check_start
# devtools::load_all()
# f <- ys | yo ~ xs1 | xo1 | xo1
# start <- c(1, 2, 3, 3, 4, 4, 5, 5, 6, 6)
# debugonce(opsr_check_start)
# opsr_check_start(f, start)


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
anova(model1, test = "Chisq")
anova(model2, test = "Chisq")
anova(model1, model2, test = "Chisq")
## => so residual deviance is smaller by 31.547 units in model2 and H0 is this is
## not different from 0. However, the Pr(>Chi) value indicates that we reject H0
## i.e., model2 is the preferred one!

## => take-away: will implement deviance based Chisq test
s <- summary(model1)
s$deviance
s$dispersion

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


## anova.opsr
devtools::load_all()
sim_dat <- opsr_simulate()
dat <- sim_dat$data
fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat)
test <- anova(fit)
print.anova.opsr(test)

## test with updated models

## also null model should be passable
## also remove tmp, anova_glm, etc.
fit2 <- update(fit, ~ . | . -1)
fit_null <- opsr_null_model(fit)
test <- anova(fit_null, fit2, fit)
print.anova.opsr(test)

test <- anova(fit_null, fit)


## GOF components
devtools::load_all()
sim_dat <- opsr_simulate()
dat <- sim_dat$data
fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat)
s <- summary(fit)
s$GOFcomponents


## texreg
devtools::load_all()
sim_dat <- opsr_simulate()
dat <- sim_dat$data
model <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat)

texreg::screenreg(model)
texreg::screenreg(model, include.pseudoR2 = TRUE, include.R2 = TRUE)

texreg::screenreg(model, beside = TRUE)
texreg::screenreg(model, beside = TRUE, include.structural = FALSE)
texreg::screenreg(model, beside = TRUE, include.structural = FALSE,
                  include.pseudoR2 = TRUE, include.R2 = TRUE)


null_model <- opsr_null_model(model)
texreg::screenreg(list(null_model, model), include.pseudoR2 = TRUE, include.R2 = TRUE)

library(pscl)

data("bioChemists", package = "pscl")
fm_zinb2 <- zeroinfl(art ~ . | ., data = bioChemists, dist = "negbin")

debugonce(texreg:::extract.zeroinfl)
texreg::screenreg(fm_zinb2)
texreg::screenreg(fm_zinb2, beside = TRUE)
texreg::screenreg(list(fm_zinb2, fm_zinb2), beside = TRUE)
texreg::screenreg(list(m1 = fm_zinb2, m2 = fm_zinb2), beside = TRUE)  # buggy
texreg::screenreg(list(fm_zinb2, fm_zinb2), custom.model.names = c("m1 count", "m1 zero", "m2 count", "m2 zero"))  # complains about 4 names passed but only two models





## telework_data update
f <- twing_status | vmd_ln ~
  ## selection model
  edu_2 + edu_3 + hhincome_2 + hhincome_3 +
  flex_work + work_fulltime + twing_feasibility +
  att_proactivemode + att_procarowning +
  att_wif + att_proteamwork +
  att_tw_effective_teamwork + att_tw_enthusiasm + att_tw_location_flex |
  ## outcome model NTW
  female + age_mean + age_mean_sq +
  race_black + race_other +
  vehicle + suburban + smalltown + rural +
  work_fulltime +
  att_prolargehouse + att_procarowning +
  region_waa |
  ## outcome model NUTW
  edu_2 + edu_3 + suburban + smalltown + rural +
  work_fulltime +
  att_prolargehouse + att_proactivemode + att_procarowning |
  ## outcome model UTW
  female + hhincome_2 + hhincome_3 +
  child + suburban + smalltown + rural +
  att_procarowning +
  region_waa

fit <- opsr(f, OPSR::telework_data)
texreg::screenreg(list(fit_xinyi, fit))

fit_null <- opsr_null_model(fit)
BIC(fit_null, fit)
anova(fit_null, fit)


plot(vmd_ln ~ factor(twing_status), data = telework_data, varwidth = TRUE,
     ylab = "Log vehicle miles driven", xlab = "Teleworking status",
     names = c("NTW", "NUTW", "UTW"))

## cran resubmit 0.1.2
sim_dat <- opsr_simulate()
dat <- sim_dat$data
model <- ys | yo ~ xs1 + xs2 | xo1 + xo2

debugonce(opsr_check_start)
start <- opsr(model, dat, .get2step = TRUE)
fit <- opsr(model, dat, start = start)

fit_lm <- lm(yo ~ xo1 + xo2, data = dat, subset = ys == 1)
debugonce(print)
anova(fit_lm)




## remove fixed parameters in opsr_null_model
devtools::load_all()
sim_dat <- opsr_simulate()
dat <- sim_dat$data
model <- ys | yo ~ xs1 + xs2 | xo1 + xo2
fit <- opsr(model, dat)
fit_null <- opsr_null_model(fit)
summary(fit_null)
texreg::screenreg(list(fit_null, fit))

## print.formula arg in print.anova.opsr
print(anova(fit), print.formula = FALSE)

## revising print.summary.opsr
devtools::load_all()
print(summary(fit))
print(summary(fit_null))

## include R2 for each regime in texreg
devtools::load_all()
texreg::screenreg(fit, include.pseudoR2 = TRUE, include.R2 = TRUE)





## https://github.com/dheimgartner/OPSR/issues/7
devtools::load_all()
rm(list = ls())
sim_dat <- opsr_simulate()
dat <- sim_dat$data
list2env(as.list(dat), envir = .GlobalEnv)
rm(dat)
model <- ys | yo ~ xs1 + xs2 | xo1 + I(1 * xo2)
fit <- opsr(model)
fit_null <- opsr_null_model(fit)
summary(fit)
summary(fit_null)
model.frame(fit_null)






## https://github.com/dheimgartner/OPSR/issues/8
devtools::load_all()
sim_dat <- opsr_simulate()
dat <- sim_dat$data
dat$xo3 <- ifelse(dat$xo2 > 0, "a", "b")
dat$xs3 <- ifelse(dat$xs2 > 0, "a", "b")
fit1 <- opsr(ys | yo ~ xs1 + xs2 | xo1 + factor(xo3), data = dat)  # ok
fit2 <- opsr(ys | yo ~ xs1 + factor(xs3) | xo1 + factor(xo3), data = dat)  # not ok
summary(fit2)

debugonce(predict)
predict(fit2, group = 1, type = "prob")





## https://github.com/dheimgartner/OPSR/issues/9
devtools::load_all()
sim_dat <- opsr_simulate()
dat <- sim_dat$data
dat$yo <- dat$yo + 10  # shift to avoid log of neg
dat$log_yo <- log(dat$yo + 1)

fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat)
summary(fit)

fit_log <- opsr(ys | log_yo ~ xs1 + xs2 | xo1 + xo2, dat)
summary(fit_log)

texreg::screenreg(list(fit, fit_log))

boxplot(dat$yo)
boxplot(dat$log_yo)

compare_fun <- function(fit, group, type = "unlog-response") {
  p <- predict(fit, group = group, type = type)  # counterfact = group
  compare <- data.frame(
    yo = subset(dat, subset = ys == group, select = yo),
    yo_pred = na.omit(p)
  )

  title <- paste0("'group' = ", group)
  plot(compare, xlim = c(0, max(compare)), ylim = c(0, max(compare)), main = title)
  abline(a = 0, b = 1, col = "red")
}

par(mfrow = c(1, 3))
compare_fun(fit_log, group = 1)
compare_fun(fit_log, group = 2)
compare_fun(fit_log, group = 3)



## revise summary
devtools::load_all()
sim_dat <- opsr_simulate()
dat <- sim_dat$data
fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat)
(s <- summary(fit))
texreg::screenreg(fit, include.R2 = TRUE, include.pseudoR2 = TRUE)



## https://github.com/dheimgartner/OPSR/issues/10
devtools::load_all()
sim_dat <- opsr_simulate()
dat <- sim_dat$data
f <- ys | yo ~ xs1 + xs2 | xo1 + xo2
start <- opsr(f, dat, .get2step = TRUE)
fix <- c("o2_xo2")
start[fix] <- 0
fit <- opsr(f, dat, start = start, fixed = fix, printLevel = 0)
summary(fit)
debugonce(summary.opsr)
summary(fit)

fit_ <- opsr(f, dat)
summary(fit_)
debugonce(summary.opsr)

fit_null <- opsr_null_model(fit_)
summary(fit_null)


## test .loglik
devtools::load_all()
sim_dat <- opsr_simulate()
dat <- sim_dat$data
dat[1, "yo"] <- -100
dat[2, "yo"] <- -200
f <- ys | yo ~ xs1 + xs2 | xo1 + xo2
fit <- opsr(f, dat)
logLik(fit)
ll1 <- opsr(f, dat, .loglik = TRUE, start = fit$estimate)
sum(ll1)
ll1
boxplot(ll1)
which(ll1 < -50)
ll2 <- fit$loglik(fit$estimate)
sum(ll2)
all(ll1 == ll2)

?loglik_cpp
mm <- model.matrix(fit)
oZ <- order(dat$ys)
Y <- lapply(sort(unique(dat$ys)), function(z) {
  dat$yo[dat$ys == z]
})
ll3 <- loglik_cpp(fit$estimate, mm$W, mm$X, Y, fit$weights, 3, 1)[order(oZ)]
all(ll1 == ll3)




## singularity issues
devtools::load_all()
sim_dat <- opsr_simulate()
dat <- sim_dat$data
dat$xo3 <- dat$xo1
dat$xo4 <- dat$xo2
fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2 | xo1 + xo2 | xo1 + xo2 + xo3 + xo4,
            dat, fixed = c("o3_xo3", "o3_xo4"))
summary(fit)
fit$singular
fit2 <- update(fit, . ~ . | . | . | . - xo3)
texreg::screenreg(list(fit, fit2))





## memory consumption
devtools::load_all()
NOBS <- 10e3
N <- 100
sim_dat <- opsr_simulate(nobs = NOBS)
dat <- sim_dat$data
td <- numeric(length = N)
mem <- numeric(length = N)
for (i in 1:N) {
  tick <- Sys.time()
  fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, data = dat, printLevel = 0)
  mu <- pryr::mem_used()
  tock <- Sys.time()
  td[i] <- tock - tick
  mem[i] <- mu
  cat("timediff: ", tock - tick, "\n")
  cat("mem used: ", mu, "\n\n")
}
op <- par(no.readonly = TRUE)
par(mfrow = c(1, 2))
plot(td, main = "Time", type = "b")
plot(mem, main = "Memory", type = "b")
par(op)




## check R2
devtools::load_all()

f <-
  ## ordinal and continuous outcome
  twing_status | vmd_ln ~
  ## selection model
  edu_2 + edu_3 + hhincome_2 + hhincome_3 +
  flex_work + work_fulltime + twing_feasibility +
  att_proactivemode + att_procarowning +
  att_wif + att_proteamwork +
  att_tw_effective_teamwork + att_tw_enthusiasm + att_tw_location_flex |
  ## outcome model NTW
  female + age_mean + age_mean_sq +
  race_black + race_other +
  vehicle + suburban + smalltown + rural +
  work_fulltime +
  att_prolargehouse + att_procarowning +
  region_waa |
  ## outcome model NUTW
  edu_2 + edu_3 + suburban + smalltown + rural +
  work_fulltime +
  att_prolargehouse + att_proactivemode + att_procarowning |
  ## outcome model UTW
  female + hhincome_2 + hhincome_3 +
  child + suburban + smalltown + rural +
  att_procarowning +
  region_waa

start <- c(
  1.2, 2.4,  # kappa 1 & 2
  0.2, 0.4, 0.1, 0.3, 0.3, 0.2, 0.1, 0.1, -0.1, 0.1, 0.1, 0.3, 0.1, 0.1,  # selection
  3.744, -0.208, 0.010, 0.000, -0.392, -0.019, 0.130, 0.010, 0.415, 0.494, 0.437, 0.186, 0.124, -0.240,  # outcome 1
  2.420, 0.224, 0.670, 0.445, 0.219, 0.824, 0.704, 0.164, -0.176, 0.171,  # outcome 2
  2.355, -0.375, 0.476, 0.317, 0.187, 0.290, 0.313, 0.856, 0.248, -0.275,  # outcome 3
  1.193, 1.248, 1.413,  # sigma
  0.068, 0.128, 0.340  # rho
)

fit <- opsr(f, telework_data, start = start)
summary(fit)

r2_ <- function(object) {
  z <- OPSR:::get_z(object)
  y <- OPSR:::get_y(object)
  RS <- residuals(object)^2
  TS <- (y - mean(y))^2
  R2o <- unlist(lapply(seq_len(object$nReg), function(i) {
    RSS <- sum(RS[z == i])
    yo <- y[z == i]
    TSS <- sum((yo - mean(yo))^2)
    1 - RSS/TSS
  }))
  R2total <- 1 - sum(RS)/sum(TS)
  R2 <- c(R2total, R2o)
  names(R2) <- c("Total", paste0("o", 1:object$nReg))
  R2
}

e <- new.env()
e$r2_ <- r2_
rm(r2_)
attach(e)

debugonce(summary)
summary(fit)
#> Yes, r2_ is correct




## replicate xinyi's treatment effects
devtools::load_all()
f <-
  ## ordinal and continuous outcome
  twing_status | vmd_ln ~
  ## selection model
  edu_2 + edu_3 + hhincome_2 + hhincome_3 +
  flex_work + work_fulltime + twing_feasibility +
  att_proactivemode + att_procarowning +
  att_wif + att_proteamwork +
  att_tw_effective_teamwork + att_tw_enthusiasm + att_tw_location_flex |
  ## outcome model NTW
  female + age_mean + age_mean_sq +
  race_black + race_other +
  vehicle + suburban + smalltown + rural +
  work_fulltime +
  att_prolargehouse + att_procarowning +
  region_waa |
  ## outcome model NUTW
  edu_2 + edu_3 + suburban + smalltown + rural +
  work_fulltime +
  att_prolargehouse + att_proactivemode + att_procarowning |
  ## outcome model UTW
  female + hhincome_2 + hhincome_3 +
  child + suburban + smalltown + rural +
  att_procarowning +
  region_waa

start <- c(
  1.2, 2.4,  # kappa 1 & 2
  0.2, 0.4, 0.1, 0.3, 0.3, 0.2, 0.1, 0.1, -0.1, 0.1, 0.1, 0.3, 0.1, 0.1,  # selection
  3.744, -0.208, 0.010, 0.000, -0.392, -0.019, 0.130, 0.010, 0.415, 0.494, 0.437, 0.186, 0.124, -0.240,  # outcome 1
  2.420, 0.224, 0.670, 0.445, 0.219, 0.824, 0.704, 0.164, -0.176, 0.171,  # outcome 2
  2.355, -0.375, 0.476, 0.317, 0.187, 0.290, 0.313, 0.856, 0.248, -0.275,  # outcome 3
  1.193, 1.248, 1.413,  # sigma
  0.068, 0.128, 0.340  # rho
)

fit <- opsr(f, telework_data, start = start, printLevel = 0)

devtools::load_all("../TWTE")
fit$weights <- telework_data$weight
ate <- TWTE:::opsr_ate(fit, type = "unlog-response")
ate




## numbers in plot_treat_obs()
colvec <- RColorBrewer::brewer.pal(n = 3, "Dark2")
pch <- 19
lty <- 3
alpha <- 0.3
lwd <- 3
fit <- fit_commute
plot_treat_obs(fit, x = 1, y = 2, type = "unlog-response",
               main = "a) Not TWing vs. TWing < 3 times/week",
               xlab = "Weekly distance when NTWing (km)",
               ylab = "Weekly distance when NUTWing (km)", col = colvec, alpha = alpha,
               cex.point = 1.5, pch = pch, lty = lty, lwd = lwd,
               legend.entry = c("NTWers", "NUTWers", "UTWers"),
               legend.position = "bottomright")
## x1, x2, y1, y2
add_text <- function(mpx, mpy) {
  usr <- par("usr")
  labels <- paste(round(as.numeric(mpx)), round(as.numeric(mpy)), sep = ", ")
  for (i in seq_along(labels)) {
    text(usr[1], usr[4]+10-i*10, labels = labels[i], col = col[i], adj = c(-0.1, 1.1))
  }
}




## wrap lines
## TODO
## shouldn't break latex code...
wrap_sweave_text <- function(file, width = 80) {
  lines <- readLines(file, warn = FALSE)
  in_chunk <- FALSE

  wrapped_lines <- lapply(lines, function(line) {
    if (grepl("^<<.*>>=", line)) in_chunk <<- TRUE
    if (grepl("^@", line)) in_chunk <<- FALSE

    if (!in_chunk && !grepl("^%", line)) {
      stringr::str_wrap(line, width = width)
    } else {
      line
    }
  })

  writeLines(unlist(wrapped_lines), file)
}

wrap_sweave_text("./vignettes/opsr.Rnw")


