library(MASS)

rm(list = ls())
setwd("~/OneDrive - Georgia Institute of Technology/01-Research/00-Working/PhD dissertation/Dissertation/ch6 OPSR/R package development")

OPSR_data <- read.csv("OPSR_data.csv")

# ==================== ordered probit ====================
op_summary <- function(TW_OP) {
  ctable <- coef(summary(TW_OP))
  p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  ctable <- cbind.data.frame(ctable, "p value" = p)
  sig <- car::recode(p, "lo:0.001='***'; 0.001:0.01='**'; 0.01:0.05='*';  0.05:0.1='.'; 0.1:hi=' '")
  ctable <- cbind.data.frame(ctable, "sig" = sig)
  return(ctable)
}

TW_OP <- polr(as.factor(C9b_TWer) ~ 
                edu_2 + edu_3 + 
                hhincome_2 + hhincome_3 + 
                B2b_6 + 
                worker_ft_B3b_binary + 
                C5min_month + 
                FAC_A4_proactivemode_sd + 
                FAC_A6_procarowning_sd + 
                FAC_B1_WIF_sd + 
                FAC_B2_proteamwork_sd + 
                FAC_C1_tw_effective_teamwork_sd + 
                FAC_C5_tw_enthusiasm_sd +
                FAC_C7_tw_location_flex_sd
              , 
              data = OPSR_data, method = "probit", Hess = T)
op_summary(TW_OP)

# ==================== outcome: regression model - preparation  ====================
TW_VMD_SR_selection <- as.matrix(with(OPSR_data, 
                                      cbind.data.frame(edu_2, edu_3,
                                                       hhincome_2, hhincome_3,
                                                       B2b_6, 
                                                       worker_ft_B3b_binary,
                                                       C5min_month, 
                                                       FAC_A4_proactivemode_sd,
                                                       FAC_A6_procarowning_sd,
                                                       FAC_B1_WIF_sd,
                                                       FAC_B2_proteamwork_sd,
                                                       FAC_C1_tw_effective_teamwork_sd,
                                                       FAC_C5_tw_enthusiasm_sd,
                                                       FAC_C7_tw_location_flex_sd)))

W_gamma <- TW_VMD_SR_selection %*% as.matrix(coef(TW_OP))
K <- c("0|1"=-Inf, TW_OP$zeta, "3|inf"=Inf)
Tj_1 <- K[OPSR_data$C9b_TWer]
Tj <- K[OPSR_data$C9b_TWer+1]
OPSR_data$IMR <- -(dnorm(Tj-W_gamma) - dnorm(Tj_1-W_gamma))/(pnorm(Tj-W_gamma) - pnorm(Tj_1-W_gamma))

# ==================== outcome: regression model - main model  ====================
# NTW
fit_NTW <- lm(E53_ln ~ 
                sex_imp_mf + 
                age_mean + age_mean_sq + 
                race_3_imp_mf_2 + race_3_imp_mf_3 + 
                E2 + 
                D2_2 + D2_3 + D2_4 + 
                worker_ft_B3b_binary + 
                FAC_A2_prolargehouse_sd + FAC_A6_procarowning_sd + 
                region_WAA + 
                IMR,
              data = OPSR_data[OPSR_data$C9b_TWer==1,])
summary(fit_NTW)

k=length(fit_NTW$coefficients)-1
SSE=sum(fit_NTW$residuals**2)
n=length(fit_NTW$residuals)
(sigma_NTW <- sqrt(SSE/(n-(1+k))))
(rho_NTW <- coefficients(fit_NTW)[k+1]/sigma_NTW)


# NUTW
fit_NUTW <- lm(E53_ln ~ 
                 edu_2 + edu_3 + 
                 D2_2 + D2_3 +  D2_4 + 
                 worker_ft_B3b_binary + 
                 FAC_A2_prolargehouse_sd + FAC_A4_proactivemode_sd + FAC_A6_procarowning_sd + 
                 IMR,
               data = OPSR_data[OPSR_data$C9b_TWer==2,])
summary(fit_NUTW)

k=length(fit_NUTW$coefficients)-1
SSE=sum(fit_NUTW$residuals**2)
n=length(fit_NUTW$residuals)
(sigma_NUTW <- sqrt(SSE/(n-(1+k))))
(rho_NUTW <- coefficients(fit_NUTW)[k+1]/sigma_NUTW)


# UTW
fit_UTW <- lm(E53_ln ~ 
                sex_imp_mf + 
                hhincome_2 + hhincome_3 + 
                D11bcd + D2_2 + D2_3 + D2_4 + 
                FAC_A6_procarowning_sd + 
                region_WAA +
                IMR,
              data = OPSR_data[OPSR_data$C9b_TWer==3,])
summary(fit_UTW)

k=length(fit_UTW$coefficients)-1
SSE=sum(fit_UTW$residuals**2)
n=length(fit_UTW$residuals)
(sigma_UTW <- sqrt(SSE/(n-(1+k))))
(rho_UTW <- coefficients(fit_UTW)[k+1]/sigma_UTW)
