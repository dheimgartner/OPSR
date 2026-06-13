sim_dat <- opsr_simulate()
dat <- sim_dat$data

## xs2 is the excluded instrument: in selection but not in the outcome equations
model <- ys | yo ~ xs1 + xs2 | xo1 + xo2

## the diagnostics refit the model many times and are therefore slow
\donttest{
fit <- opsr(model, dat, printLevel = 0)

## run both diagnostics
test <- opsr_test_exclusion_restriction(fit, instrument = "xs2",
                                        grid = seq(-0.99, 0.99, length.out = 11))
print(test)

## visualise the profile likelihood per regime
plot(test$profile_rho)

## the two components are also available individually:
opsr_lr_instrument(fit, instrument = "xs2")
opsr_profile_rho(fit, grid = seq(-0.99, 0.99, length.out = 11))
}
