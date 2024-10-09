devtools::load_all()

start <- c(1.2, 2.4, # kappa 1 & 2
           0.2, 0.4, 0.1, 0.3, 0.3, 0.2, 0.1, 0.1, -0.1, 0.1, 0.1, 0.3, 0.1, 0.1, # selection
           3.744, -0.208, 0.010, 0.000, -0.392, -0.019, 0.130, 0.010, 0.415, 0.494, 0.437, 0.186, 0.124, -0.240, # outcome 1
           2.420, 0.224, 0.670, 0.445, 0.219, 0.824, 0.704, 0.164, -0.176, 0.171, # outcome 2
           2.355, -0.375, 0.476, 0.317, 0.187, 0.290, 0.313, 0.856, 0.248, -0.275, # outcome 3
           1.193, 1.248, 1.413, # sigma
           0.068, 0.128, 0.340 # rho
)

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

fit_nm <- OPSR::opsr(f,
                     method = "NM",
                     iterlim = 50e3,
                     start = start,
                     data=telework_data)

fit_bfgs <- OPSR::opsr(f, data = telework_data)
summary(fit_bfgs)

fit_R <- OPSR::opsr(f, telework_data, .useR = TRUE)
fit_bfgs$runtime
fit_R$runtime

texreg::screenreg(list(fit_bfgs, fit_R))

devtools::load_all()
my_start <- opsr(f, telework_data, .get2step = TRUE)
(comp <- data.frame(start, my_start))
