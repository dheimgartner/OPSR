devtools::load_all()

start1 <- c(1.2, 2.4,  # kappa 1 & 2
            0.2, 0.4, 0.1, 0.3, 0.3, 0.2, 0.1, 0.1, -0.1, 0.1, 0.1, 0.3, 0.1, 0.1, # selection
            3.744, -0.208, 0.010, 0.000, -0.392, -0.019, 0.130, 0.010, 0.415, 0.494, 0.437, 0.186, 0.124, -0.240, # outcome 1
            2.420, 0.224, 0.670, 0.445, 0.219, 0.824, 0.704, 0.164, -0.176, 0.171, # outcome 2
            2.355, -0.375, 0.476, 0.317, 0.187, 0.290, 0.313, 0.856, 0.248, -0.275, # outcome 3
            1.193, 1.248, 1.413,  # sigma
            0.068, 0.128, 0.340  # rho
)

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

fit1 <- opsr(f, telework_data, start = start1, method = "NM", iterlim = 50e3)
summary(fit1)

start2 <- opsr(f, telework_data, .get2step = TRUE)
start2 <- round(start, 3)
fit2 <- opsr(f, telework_data, start = start2, method = "NM", iterlim = 50e3)

df_start <- data.frame(
  start1 = start1,
  start2 = start2
)

df_start$diff <- with(df_start, start1 - start2)
df_start
