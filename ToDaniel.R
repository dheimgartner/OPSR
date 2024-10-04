# Load required packages
library("switchSelection")
library("mnorm")

# ---------------------------
# Simulate the data
# ---------------------------

# Set seed for reproducibility
set.seed(123)

# The number of observations
n <- 10000

# Regressors (covariates)
x1 <- runif(n = n, min = -1, max = 1)
x2 <- runif(n = n, min = -1, max = 1)
x3 <- runif(n = n, min = -1, max = 1)
x4 <- runif(n = n, min = -1, max = 1)

# Random errors
sigma       <- matrix(0.5, nrow = 4, ncol = 4)
diag(sigma) <- 1
errors      <- mnorm::rmnorm(n = n, mean = rep(0, 4), sigma = sigma)
u1          <- errors[, 1]
eps0        <- errors[, 2]
eps1        <- errors[, 3]
eps2        <- errors[, 4]

# Coefficients
gamma1 <- c(1, 1, -0.5, 0.5)
beta0  <- c(1, 1, 1)
beta1  <- c(1.5, -1)
beta2  <- c(-1, 2, -1)

# Thresholds
cuts <- c(-0.5, 0.5)

# Latent variable
z1_latent <- gamma1[1] * x1 + gamma1[2] * x2 + u1

# Observable ordinal outcome
z1                                                  <- rep(0, n)
z1[(z1_latent >= cuts[1]) & (z1_latent <= cuts[2])] <- 1
z1[z1_latent  >= cuts[2]]                           <- 2

# Latent continuous outcome
y0 <- beta0[1] + beta0[2] * x1 + beta0[3] * x3 + eps0
y1 <- beta1[1] + beta1[2] * x3 + eps1
y2 <- beta2[1] + beta2[2] * x2 + beta2[3] * x4 + eps2

# Observable continuous outcome
y          <- rep(NA, n)
y[z1 == 0] <- y0[z1 == 0]
y[z1 == 1] <- y1[z1 == 1]
y[z1 == 2] <- y2[z1 == 2]

# Data
data <- data.frame(x1 = x1, x2 = x2, 
                   x3 = x3, x4 = x4,
                   z1 = z1, y  = y)

# ---------------------------
# Analysis of the data
# ---------------------------

# Split the observable outcome into potential outcomes
  # regime 0
data$y0               <- NA
data$y0[data$z1 == 0] <- data$y[data$z1 == 0]
  # regime 1
data$y1               <- NA
data$y1[data$z1 == 1] <- data$y[data$z1 == 1]
  # regime 2
data$y2               <- NA
data$y2[data$z1 == 2] <- data$y[data$z1 == 2]

# Estimate the model
model <- msel(formula  = z1 ~ x1 + x2,
              formula2 = list(y0 ~ x1 + x3,
                              y1 ~ x3,
                              y2 ~ x2 + x4),
              data = data)
summary(model)

# Check the accuracy of the estimates
  # beta0
cbind(real      = beta0, 
      estimates = coef(model, type = "coef2", eq2 = 1, regime = 0))
  # beta1
cbind(real      = beta1, 
      estimates = coef(model, type = "coef2", eq2 = 2, regime = 0))
  # beta2
cbind(real      = beta2, 
      estimates = coef(model, type = "coef2", eq2 = 3, regime = 0))
