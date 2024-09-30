devtools::load_all()

library(tidyverse)

rm(list = ls())

sd1 <- sim_dat_1()
plot.errors(sd1$errors)

estimate <- function(sim_dat) {
  fit_y1 <- lm(Y ~ 0 + X1 + X2, data = sim_dat$data, subset = sim_dat$data$Z == 1)
  fit_y2 <- lm(Y ~ 0 + X1 + X2, data = sim_dat$data, subset = sim_dat$data$Z == 2)
  fit_y3 <- lm(Y ~ 0 + X1 + X2, data = sim_dat$data, subset = sim_dat$data$Z == 3)
  fit_z <- MASS::polr(factor(Z) ~ X1 + X2, data = sim_dat$data, method = "probit")
  list(fit_y1 = fit_y1,
       fit_y2 = fit_y2,
       fit_y3 = fit_y3,
       fit_z = fit_z)
}

fit1 <- estimate(sd1)

extract_coefs <- function(fit_list) {
  all_coefs <- lapply(fit_list, function(x) coefficients(x))
  all_coefs$zeta <- fit_list$fit_z$zeta
  all_coefs
}

all_coefs <- extract_coefs(fit1)

simulation_study <- function(n = 100, sigma = NULL) {
  sd1 <- sim_dat_1(sigma)
  true_params <- sd1$params
  errors <- sd1$errors
  all_coefs <- lapply(1:n, function(x) {
    sim <- sim_dat_1(sigma)
    fit <- estimate(sim)
    all_coefs <- extract_coefs(fit)
    all_coefs
  })
  f <- function(fit, tp) data.frame(
    t(sapply(all_coefs, function(x) x[[fit]])),
    true_X1 = tp[1], true_X2 = tp[2]
  )
  beta1 <- f("fit_y1", true_params$beta1)
  beta2 <- f("fit_y2", true_params$beta2)
  beta3 <- f("fit_y3", true_params$beta3)
  gamma <- f("fit_z", true_params$gamma)
  kappa <- f("zeta", true_params$kappa)
  names(kappa) <- c("kappa1", "kappa2", "true_kappa1", "true_kappa2")
  out <- list(
    beta1 = beta1,
    beta2 = beta2,
    beta3 = beta3,
    gamma = gamma,
    kappa = kappa,
    errors = errors,
    sigma = sigma
  )
  class(out) <- "simulation_study"
  out
}

ss1 <- simulation_study()
class(ss1)

plot.simulation_study <- function(ss) {
  opar <- par(no.readonly = TRUE)
  plt <- function(dat, main = NULL) {
    boxplot(dat[[1]], dat[[2]])
    points(1, dat[[3]][1], col = "red")
    mtext(paste0("bias: ", round(mean(dat[[1]]) - dat[[3]][1], 3)), at = 1, cex = 0.75)
    points(2, dat[[4]][1], col = "red")
    mtext(paste0("bias: ", round(mean(dat[[2]]) - dat[[4]][1], 3)), at = 2, cex = 0.75)
    mtext(main, line = -1.25, adj = 0.03, cex = 0.75, font = 2)
  }
  par(mfrow = c(2, 3), mar = par("mar") - c(3, 1, 2.5, 1.5))
  plt(ss$beta1, main = "beta1")
  plt(ss$beta2, main = "beta2")
  plt(ss$beta3, main = "beta3")
  plt(ss$gamma, main = "gamma")
  plt(ss$kappa, main = "kappa")
  par(opar)
}

plot(ss1)




## check: if correlation increases -> bias increases
sigma2 <- diag(nrow = 4, ncol = 4)
ss2 <- simulation_study(n = 1000, sigma = sigma2)
plot(ss2)

## check if diagonal is 1 for all but off-diagonal != 0
sigma3 <- matrix(c(
  1.0, 0.8, 0.8, 0.8,
  0.8, 1.0, 0.8, 0.8,
  0.8, 0.8, 1.0, 0.8,
  0.8, 0.8, 0.8, 1.0), ncol = 4)
ss3 <- simulation_study(n = 1000, sigma = sigma3)
plot(ss3)


## marginal distributions
## truncated errors induce bias
errors <- as.data.frame(sd1$errors)
names(errors) <- c("epsilon", "eta1", "eta2", "eta3")
z <- sd1$data$Z

pairs(errors, pch = ".", cex = 3, col = c("darkorange", "steelblue", "forestgreen")[z])

plot_truncated <- function(sim_dat) {
  errors <- as.data.frame(sim_dat$errors)
  names(errors) <- c("epsilon", "eta1", "eta2", "eta3")
  z <- sd1$data$Z

  dat <-
    errors %>%
    cbind(z) %>%
    pivot_longer(-c(epsilon, z))

  p <-
    dat %>%
    ggplot(aes(x = value, y = epsilon, col = factor(z))) +
    geom_point(alpha = 0.6, show.legend = FALSE) +
    scale_color_manual(values = c("darkorange", "steelblue", "forestgreen")) +
    labs(x = "eta") +
    Heimisc::my_theme() +
    Heimisc::add_grid()

  # px <- ggExtra::ggMarginal(p, groupColour = TRUE, groupFill = TRUE)
  # py <- ggExtra::ggMarginal(p, yparams = list(col = "darkgrey", fill = "lightgrey"))
  #
  # py$grobs[py$layout$name == "topMargPlot"] <- px$grobs[py$layout$name == "topMargPlot"]
  # py
  ggExtra::ggMarginal(p, groupColour = TRUE, groupFill = TRUE)
}

(p1 <- plot_truncated(sd1))

sigma2
sd2 <- sim_dat_1(sigma = sigma2)
(p2 <- plot_truncated(sd2))

sigma3
sd3 <- sim_dat_1(sigma = sigma3)
(p3 <- plot_truncated(sd3))
