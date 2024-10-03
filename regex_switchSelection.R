
devtools::load_all()

library(switchSelection)

rm(list = ls())

sim_dat <- opsr_simulate()
dat <- sim_dat$data
dat$ys <- dat$ys - 1

saveRDS(dat, "regex_switchSelection.rds")



library(switchSelection)

dat <- readRDS("regex_switchSelection.rds")

fit <- msel(list(ys ~ xs1 + xs2),
            ## same specification for all three regimes (0:2)
            list(yo ~ xo1 + xo2),
            groups = 0:2,
            groups2 = 0:2,
            data = dat)

summary(fit)

## question: how can we use different specifications (for the continuous outcome)
## for the three regimes
groups <- 0:2
(groups2 <- matrix(c(0, -1, -1, -1, 1, -1, -1, -1, 2), ncol = 3))

dat_ <- dat
dat_$yo0 <- ifelse(dat_$ys == 0, dat_$yo, NA)
dat_$yo1 <- ifelse(dat_$ys == 1, dat_$yo, NA)
dat_$yo2 <- ifelse(dat_$ys == 2, dat_$yo, NA)

fit <- msel(list(ys ~ xs1 + xs2),
            ## remove Intercept for regime 1 and drop xo2 for regime 2
            list(yo0 ~ xo1 + xo2, yo1 ~ -1 + xo1 + xo2, yo2 ~ xo1),
            groups = groups,
            groups2 = groups2,
            data = dat_)

summary(fit)
