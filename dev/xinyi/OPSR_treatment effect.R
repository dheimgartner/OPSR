library(questionr)

rm(list = ls())
setwd("~/OneDrive - Georgia Institute of Technology/01-Research/00-Working/PhD dissertation/Dissertation/ch6 OPSR/R package development")

OPSR_data <- read.csv("OPSR_data.csv")

# ==================== ordered probit + regression set-up ====================
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

W <- TW_VMD_SR_selection
Z <- OPSR_data$C9b_TWer
K <- c(1.2, 2.4)
G <- c(0.2, 0.4, 0.1, 0.3, 0.3, 0.2, 0.1, 0.1, -0.1, 0.1, 0.1, 0.3, 0.1, 0.1)

Y <- OPSR_data$E53_ln
VMD <- OPSR_data$E53

X_NTW <- as.matrix(with(OPSR_data, 
                        cbind.data.frame(1, # intercept
                                         sex_imp_mf,
                                         age_mean, age_mean_sq,
                                         race_3_imp_mf_2, race_3_imp_mf_3,
                                         E2,
                                         D2_2, D2_3, D2_4, 
                                         worker_ft_B3b_binary,
                                         FAC_A2_prolargehouse_sd, 
                                         FAC_A6_procarowning_sd,
                                         region_WAA)))
B_NTW <- c(3.744, -0.208, 0.010, 0.000, -0.392, -0.019, 0.130, 0.010, 0.415, 0.494, 0.437, 0.186, 0.124, -0.240)


X_NUTW <- as.matrix(with(OPSR_data, 
                         cbind.data.frame(1, # intercept
                                          edu_2, edu_3,
                                          D2_2, D2_3, D2_4, 
                                          worker_ft_B3b_binary,
                                          FAC_A2_prolargehouse_sd, 
                                          FAC_A4_proactivemode_sd,
                                          FAC_A6_procarowning_sd)))
B_NUTW <- c(2.420, 0.224, 0.670, 0.445, 0.219, 0.824, 0.704, 0.164, -0.176, 0.171)


X_UTW <- as.matrix(with(OPSR_data, 
                        cbind.data.frame(1, # intercept
                                         sex_imp_mf,
                                         hhincome_2, hhincome_3,
                                         D11bcd,
                                         D2_2, D2_3, D2_4, 
                                         FAC_A6_procarowning_sd,
                                         region_WAA)))
B_UTW <- c(2.355, -0.375, 0.476, 0.317, 0.187, 0.290, 0.313, 0.856, 0.248, -0.275)

S <- c(1.193, 1.248, 1.413)
R <- c(0.068, 0.128, 0.340)

parameter <- c(K, G, B_NTW, B_NUTW, B_UTW, S, R)
par_name <- c("K12", "K23", colnames(W), 
              colnames(X_NTW), colnames(X_NUTW), colnames(X_UTW),
              "sigma1", "sigma2", "sigma3",
              "rho1", "rho2", "rho3")

p_K <- length(K)
p_G <- length(G)
p_B_NTW <- length(B_NTW)
p_B_NUTW <- length(B_NUTW)
p_B_UTW <- length(B_UTW)
p_S <- length(S)
p_R <- length(R)

# ==================== treatment effect ====================
MLE.par <- read.csv('OPSR_MLE.csv')
MLE.par <- MLE.par[,3]

VMD_exp <- function(nLL.par, nLL.Z, nLL.W, nLL.Y, nLL.X_NTW, nLL.X_NUTW, nLL.X_UTW, jp){
  
  threshold_12 <- nLL.par[1]
  threshold_23 <- nLL.par[2]
  gamma <- matrix(nLL.par[3: (p_K + p_G)], ncol =  p_G)
  beta_NTW <- matrix(nLL.par[(p_K + p_G + 1) : (p_K + p_G + p_B_NTW) ], ncol =  p_B_NTW)
  beta_NUTW <- matrix(nLL.par[ (p_K + p_G + p_B_NTW + 1) : (p_K + p_G + p_B_NTW + p_B_NUTW)], ncol = p_B_NUTW)
  beta_UTW <- matrix(nLL.par[(p_K + p_G + p_B_NTW + p_B_NUTW + 1): (p_K + p_G + p_B_NTW + p_B_NUTW + p_B_UTW)], ncol = p_B_UTW)
  sigma1 <- nLL.par[(p_K + p_G + p_B_NTW + p_B_NUTW + p_B_UTW + 1)]
  sigma2 <- nLL.par[(p_K + p_G + p_B_NTW + p_B_NUTW + p_B_UTW + 2)]
  sigma3 <- nLL.par[(p_K + p_G + p_B_NTW + p_B_NUTW + p_B_UTW + 3)]
  rho1 <- nLL.par[(p_K + p_G + p_B_NTW + p_B_NUTW + p_B_UTW + p_S + 1)]
  rho2 <- nLL.par[(p_K + p_G + p_B_NTW + p_B_NUTW + p_B_UTW + p_S + 2)]
  rho3 <- nLL.par[(p_K + p_G + p_B_NTW + p_B_NUTW + p_B_UTW + p_S + 3)]
  VMD_ln <- matrix(nrow = nrow(W), ncol = 1)
  VMD <- matrix(nrow = nrow(W), ncol = 1)
  prob <- matrix(nrow = nrow(W), ncol = 1)
  
  for(i in 1:nrow(nLL.W)){ 
    
    j <- nLL.Z[i]
    
    if (jp==1) {
      base <-  beta_NTW %*% nLL.X_NTW[i,]
      rho <- rho1
      sigm <- sigma1} 
    else if (jp==2) {
      base <-  beta_NUTW %*% nLL.X_NUTW[i,]
      rho <- rho2
      sigm <- sigma2} 
    else {
      base <-  beta_UTW %*% nLL.X_UTW[i,]
      rho <- rho3
      sigm <- sigma3}
    
    if (j==1) {
      low <- -Inf
      high <- threshold_12 - gamma %*% nLL.W[i,]} 
    else if (j==2) {
      low <- threshold_12 - gamma %*% nLL.W[i,]
      high <- threshold_23 - gamma %*% nLL.W[i,]} 
    else {
      low <- threshold_23 - gamma %*% nLL.W[i,]
      high <- Inf}
    
    VMD_ln[i] <- base - rho * sigm * (dnorm(high) - dnorm(low))/(pnorm(high) - pnorm(low))
    VMD[i] <- exp(base + (sigm^2)/2) * (pnorm(high - rho * sigm) - pnorm(low - rho * sigm))/(pnorm(high) - pnorm(low)) - 1
    prob[i] <- pnorm(high) - pnorm(low)
  }
  
  return(cbind(VMD_ln, VMD, prob))
  
}

E53_ln_NTW <- VMD_exp(nLL.par = MLE.par, nLL.Z = Z, nLL.W = W, nLL.Y = Y, nLL.X_NTW = X_NTW, nLL.X_NUTW = X_NUTW, nLL.X_UTW = X_UTW, jp = 1)[,1]
E53_ln_NUTW <- VMD_exp(nLL.par = MLE.par, nLL.Z = Z, nLL.W = W, nLL.Y = Y, nLL.X_NTW = X_NTW, nLL.X_NUTW = X_NUTW, nLL.X_UTW = X_UTW, jp = 2)[,1]
E53_ln_UTW <- VMD_exp(nLL.par = MLE.par, nLL.Z = Z, nLL.W = W, nLL.Y = Y, nLL.X_NTW = X_NTW, nLL.X_NUTW = X_NUTW, nLL.X_UTW = X_UTW, jp = 3)[,1]

E53_NTW <- VMD_exp(nLL.par = MLE.par, nLL.Z = Z, nLL.W = W, nLL.Y = Y, nLL.X_NTW = X_NTW, nLL.X_NUTW = X_NUTW, nLL.X_UTW = X_UTW, jp = 1)[,2]
E53_NUTW <- VMD_exp(nLL.par = MLE.par, nLL.Z = Z, nLL.W = W, nLL.Y = Y, nLL.X_NTW = X_NTW, nLL.X_NUTW = X_NUTW, nLL.X_UTW = X_UTW, jp = 2)[,2]
E53_UTW <- VMD_exp(nLL.par = MLE.par, nLL.Z = Z, nLL.W = W, nLL.Y = Y, nLL.X_NTW = X_NTW, nLL.X_NUTW = X_NUTW, nLL.X_UTW = X_UTW, jp = 3)[,2]

VMD_matrix <- matrix(nrow = 16, ncol = 3)
wt_new_county_trim_pop <- OPSR_data$wt_new_county_trim_pop

for (i in 1:3) {
  cond <- Z==i
  VMD_matrix[1,i] <- mean(Y[cond])
  VMD_matrix[2,i] <- mean(E53_ln_NTW[cond])
  VMD_matrix[3,i] <- mean(E53_ln_NUTW[cond])
  VMD_matrix[4,i] <- mean(E53_ln_UTW[cond])
  
  VMD_matrix[5,i] <- wtd.mean(Y[cond], weights = wt_new_county_trim_pop[cond])
  VMD_matrix[6,i] <- wtd.mean(E53_ln_NTW[cond], weights = wt_new_county_trim_pop[cond])
  VMD_matrix[7,i] <- wtd.mean(E53_ln_NUTW[cond], weights = wt_new_county_trim_pop[cond])
  VMD_matrix[8,i] <- wtd.mean(E53_ln_UTW[cond], weights = wt_new_county_trim_pop[cond])
  
  VMD_matrix[9,i] <- mean(VMD[cond])
  VMD_matrix[10,i] <- mean(E53_NTW[cond])
  VMD_matrix[11,i] <- mean(E53_NUTW[cond])
  VMD_matrix[12,i] <- mean(E53_UTW[cond])
  
  VMD_matrix[13,i] <- wtd.mean(VMD[cond], weights = wt_new_county_trim_pop[cond])
  VMD_matrix[14,i] <- wtd.mean(E53_NTW[cond], weights = wt_new_county_trim_pop[cond])
  VMD_matrix[15,i] <- wtd.mean(E53_NUTW[cond], weights = wt_new_county_trim_pop[cond])
  VMD_matrix[16,i] <- wtd.mean(E53_UTW[cond], weights = wt_new_county_trim_pop[cond])
}
VMD_matrix
rownames(VMD_matrix) <- c('logVMD_non-wt_obs', 'logVMD_non-wt_NTWing', 'logVMD_non-wt_NUTWing', 'logVMD_non-wt_UTWing',
                          'logVMD_wt_obs', 'logVMD_wt_NTWing', 'logVMD_wt_NUTWing', 'logVMD_wt_UTWing',
                          'VMD_non-wt_obs', 'VMD_non-wt_NTWing', 'VMD_non-wt_NUTWing', 'VMD_non-wt_UTWing',
                          'VMD_wt_obs', 'VMD_wt_NTWing', 'VMD_wt_NUTWing', 'VMD_wt_UTWing')
colnames(VMD_matrix) <- c('NTWer', 'NUTWer', 'UTWer')
VMD_matrix

# ==================== model fit ====================

# ordered probit
N <- nrow(OPSR_data)
loglik_EL <- N*log(1/3)
MS_count <- table(OPSR_data$C9b_TWer)
MS_prob <- table(OPSR_data$C9b_TWer)/N
loglik_MS <- MS_count[1] * log(MS_prob[1]) + MS_count[2] * log(MS_prob[2]) + MS_count[3] * log(MS_prob[3])

prob <- VMD_exp(nLL.par = MLE.par, nLL.Z = Z, nLL.W = W, nLL.Y = Y, nLL.X_NTW = X_NTW, nLL.X_NUTW = X_NUTW, nLL.X_UTW = X_UTW, jp = 1)[,3]
loglik_beta <-sum(log(prob))

1-loglik_beta/loglik_EL # equal-likely base psedo R2
1-loglik_beta/loglik_MS # market-share base psedo R2


# regression
E53_ln_pred <- ifelse(OPSR_data$C9b_TWer==1, E53_ln_NTW, ifelse(OPSR_data$C9b_TWer==2, E53_ln_NUTW, E53_ln_UTW))

r2 <- function(s) {
  cond <- OPSR_data$C9b_TWer %in% s
  RSS <- sum((OPSR_data$E53_ln[cond] - E53_ln_pred[cond])^2)
  TSS <- sum((OPSR_data$E53_ln[cond] - mean(OPSR_data$E53_ln[cond]))^2)
  return(1-RSS/TSS)
}

r2(1) # R2 for NTWer
r2(2) # R2 for NUTWer
r2(3) # R2 for UTWer
r2(c(1,2,3))  # R2 for all groups
