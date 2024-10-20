## works on single model

## works for model comparison

## correctly rejects the null model


sim_dat <- load_sim_dat()
dat <- sim_dat$data
fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, printLevel = 0)
anova(fit)

fit_null <- opsr_null_model(fit)
anova(fit_null)
