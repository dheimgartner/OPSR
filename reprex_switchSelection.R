
devtools::load_all()

library(switchSelection)

rm(list = ls())

sim_dat <- opsr_simulate()
dat <- sim_dat$data
dat$ys <- dat$ys - 1

saveRDS(dat, "reprex_switchSelection.rds")



library(switchSelection)

dat <- readRDS("reprex_switchSelection.rds")

fit <- msel(list(ys ~ xs1 + xs2),
            ## same specification for all three regimes (0:2)
            list(yo ~ xo1 + xo2),
            groups = 0:2,
            groups2 = 0:2,
            data = dat)

summary(fit)

## question: how can we use different specifications (for the continuous outcome)
## for the three regimes
dat$yo0 <- ifelse(dat$ys == 0, dat$yo, NA)
dat$yo1 <- ifelse(dat$ys == 1, dat$yo, NA)
dat$yo2 <- ifelse(dat$ys == 2, dat$yo, NA)

fit <- msel(list(ys ~ xs1 + xs2),
            ## different processes for outcome
            list(yo0 ~ xo1 + xo2, yo1 ~ xo1 + xo2, yo2 ~ xo1),
            data = dat)

summary(fit)
class(fit)

predict(fit)
