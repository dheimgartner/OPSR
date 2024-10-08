
rm(list = ls())
setwd("~/OneDrive - Georgia Institute of Technology/01-Research/00-Working/PhD dissertation/Dissertation/ch6 OPSR/R package development")
install.packages("OPSR_0.0.0.9001.tar.gz", repos = NULL)

OPSR_data <- read.csv("OPSR_data.csv")

# ==================== Xinyi's code ====================
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


# (negative) log likelihood function
nLL_simple <- function(nLL.par, nLL.Z, nLL.W, nLL.Y, nLL.X_NTW, nLL.X_NUTW, nLL.X_UTW){
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
  
  final_sum <- 0
  
  for(i in 1:nrow(nLL.W)){ 
    part1 <- 0
    part2 <- 0
    part3 <- 0
    
    if (nLL.Z[i]==1) {
      res <- nLL.Y[i] -  beta_NTW %*% nLL.X_NTW[i,]
      low <- -Inf
      high <- threshold_12 - gamma %*% nLL.W[i,]
      part1 <- 1/sigma1 * dnorm(res/sigma1)
      part2 <- pnorm((sigma1 * high - rho1 * res) / (sigma1*sqrt(1-rho1**2)))
      part3 <- 0
      
    } else if (nLL.Z[i]==2) {
      res <- nLL.Y[i] -  beta_NUTW %*% nLL.X_NUTW[i,]
      low <- threshold_12 - gamma %*% nLL.W[i,]
      high <- threshold_23 - gamma %*% nLL.W[i,]
      part1 <- 1/sigma2 * dnorm(res/sigma2)
      part2 <- pnorm((sigma2 * high - rho2 * res) / (sigma2*sqrt(1-rho2**2)))
      part3 <- pnorm((sigma2 * low - rho2 * res) / (sigma2*sqrt(1-rho2**2)))
      
    } else {
      res <- nLL.Y[i] -  beta_UTW %*% nLL.X_UTW[i,]
      low <- threshold_23 - gamma %*% nLL.W[i,]
      high <- Inf
      part1 <- 1/sigma3 * dnorm(res/sigma3)
      part2 <- 1
      part3 <- pnorm((sigma3 * low - rho3 * res) / (sigma3*sqrt(1-rho3**2)))
      
    }
    final_sum <- final_sum + log(part1) + log(part2 - part3)
  }
  
  # part1
  # part2 - part3
  
  -final_sum
}


MLE <- optim(parameter, nLL_simple, nLL.Z = Z, nLL.W = W,
             nLL.Y = Y, nLL.X_NTW = X_NTW, nLL.X_NUTW = X_NUTW, nLL.X_UTW = X_UTW, 
             hessian = T, # method = "Nelder-Mead",
             control = list(maxit=50000))
MLE$convergence


(coef <- MLE$par)
(stdev <- sqrt(diag(solve(MLE$hessian))))
t_stats <- coef/stdev
p <-  2*pnorm(-abs(t_stats))
sig <- car::recode(p, "lo:0.001='***'; 0.001:0.01='**'; 0.01:0.05='*';  0.05:0.1='.'; 0.1:hi=' '")
cbind(par_name, coef, stdev, t_stats, p, sig)


# ==================== opsr() function ====================
(OPSR_model <- OPSR::opsr(C9b_TWer | E53_ln ~
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
                           region_WAA,
                          # method = "Nelder-Mead",
                          # start = c(1.2, 2.4, # kappa 1 & 2
                          #           0.2, 0.4, 0.1, 0.3, 0.3, 0.2, 0.1, 0.1, -0.1, 0.1, 0.1, 0.3, 0.1, 0.1, # selection
                          #           3.744, -0.208, 0.010, 0.000, -0.392, -0.019, 0.130, 0.010, 0.415, 0.494, 0.437, 0.186, 0.124, -0.240, # outcome 1
                          #           2.420, 0.224, 0.670, 0.445, 0.219, 0.824, 0.704, 0.164, -0.176, 0.171, # outcome 2
                          #           2.355, -0.375, 0.476, 0.317, 0.187, 0.290, 0.313, 0.856, 0.248, -0.275, # outcome 3
                          #           1.193, 1.248, 1.413, # sigma
                          #           0.068, 0.128, 0.340 # rho
                          #           ),
                         data=OPSR_data))

summary(OPSR_model)


OPSR::opsr_generate_start()



