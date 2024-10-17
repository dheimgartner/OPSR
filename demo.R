devtools::load_all()

## estimating a model
## simulated data
sim_dat <- opsr_simulate()
dat <- sim_dat$data  # 1000 observations
head(dat)
sim_dat$sigma
sim_dat$params

## specify a model
model <- ys | yo ~ xs1 + xs2 | xo1 + xo2 | xo1 + xo2 | xo1 + xo2
model <- ys | yo ~ xs1 + xs2 | xo1 + xo2  # since we use the same specification...

## Heckman solution
## opsr is the main API
heckman <- opsr(model, dat, .get2step = TRUE)
## only internally used to generate reasonable starting values
heckman

fit <- opsr(model, dat)

## do we need OPSR (welch test)?
summary(fit)

## xinyi replica
## fast and memory efficient (can handle 1e6 obs pretty easily)
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

fit_xinyi <- opsr(f, OPSR::telework_data)  # data attached to package
summary(fit_xinyi)

## using update and model comparison
fit_updated <- update(fit, ~ . | 1)  # only intercepts for the continuous outcome
## null model
fit_null <- opsr_null_model(fit)

## likelihood ratio test
anova(fit_null, fit_updated, fit)

## predict
vmd <- predict(fit_xinyi, group = 1, type = "unlog-response")
vmd_counterfact <- predict(fit_xinyi, group = 1, counterfact = 2, type = "unlog-response")
plot(vmd, vmd_counterfact,
     main = "Non-TW => Non-usual TW",
     xlab = "VMD [km]",
     ylab = "Counterfactual VMD [km]")
abline(a = 0, b = 1, col = "red", lwd = 2)

texreg::screenreg(fit_xinyi, beside = TRUE, include.pseudoR2 = TRUE, include.R2 = TRUE, include.structural = FALSE)
