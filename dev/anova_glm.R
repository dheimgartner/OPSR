data(mtcars)
mtcars$am <- as.factor(mtcars$am)
model1 <- glm(am ~ hp, family = binomial(link = "probit"), data = mtcars)  # reduced model
model2 <- glm(am ~ hp + wt, family = binomial(link = "probit"), data = mtcars)  # full model

debugonce(stats:::anova.glmlist)
anova(model1, model2)

debugonce(stats:::anova.glm)

anova(model1, test = "Chisq")

## if model features multiple terms (variables) then glm.fit is called for every term!
## nvars - 1 (since the last inclusion is the full model)
debug(stats:::glm.fit)
anova(model2, test = "Chisq")
undebug(stats:::glm.fit)
anova(model1, model2, test = "Chisq")

## replicating deviance
set.seed(123)
n <- 100
x <- rnorm(n)
y <- rbinom(n, 1, pnorm(x))
fit <- glm(y ~ x, family = binomial(link = "probit"))

deviance(fit)

fit2 <- glm(y ~ 1, family = binomial(link = "probit"))
deviance(fit2)

logLik_fitted <- logLik(fit)
logLik_saturated <- sum(dbinom(y, size = 1, prob = y, log = TRUE))
#> 0

manual_deviance <- -2 * (logLik_fitted - logLik_saturated)
manual_deviance



