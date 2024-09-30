## check out MASS::polr

library(MASS)

rm(list = ls())

?MASS::polr

polr

sd1 <- sim_dat_1()
dat <- sd1$data

doit <- function() {
  fit <<- MASS::polr(factor(Z) ~ X1 + X2, data = dat, method = "probit")
  fit
}

doit()

summary(fit)
sd1$params[c("gamma", "kappa")]

debugonce(polr)
doit()

## just a wraper around glm.fit??
## => no, only for starting values... afterwards calls polr.fit

debugonce(MASS:::polr.fit)
doit()

## key func: fmin (and gmin which is the gradient)
fmin <- function(beta) {
  theta <- beta[pc + ind_q]
  gamm <- c(-Inf, cumsum(c(theta[1L], exp(theta[-1L]))),
            Inf)
  eta <- offset
  if (pc)
    eta <- eta + drop(x %*% beta[ind_pc])
  pr <- pfun(pmin(100, gamm[y + 1] - eta)) - pfun(pmax(-100,
                                                       gamm[y] - eta))
  if (all(pr > 0))
    -sum(wt * log(pr))
  else Inf
}

?MASS:::polr.fit  # no documentation...




## chat formula parsing with lin reg example
## PROBABLY NOT THE BEST EXAMPLE (but ok to understand involved steps)
library(maxLik)

# Define the custom function for MLE with formula and data input
mle_linear_regression <- function(formula, data) {
  # Step 1: Parse the formula to get the response and the design matrix
  X <- model.matrix(formula, data)  # Independent variables (design matrix)
  y <- model.response(model.frame(formula, data))  # Response variable

  # Step 2: Define the log-likelihood function for linear regression
  log_lik <- function(params) {
    beta <- params[1:ncol(X)]  # Regression coefficients (length is number of columns in X)
    sigma <- params[ncol(X) + 1]  # Error standard deviation

    # Predicted values
    y_hat <- X %*% beta

    # Log-likelihood based on normal distribution
    ll <- -0.5 * length(y) * log(2 * pi) - length(y) * log(sigma) -
      0.5 * sum((y - y_hat)^2) / (sigma^2)

    return(ll)
  }

  # Step 3: Set starting values for optimization
  start_params <- c(rep(0, ncol(X)), 1)  # Initial guesses for beta coefficients and sigma

  # Step 4: Use maxLik to perform maximum likelihood estimation
  mle_result <- maxLik(logLik = log_lik, start = start_params)

  # Step 5: Return the summary of MLE results
  return(summary(mle_result))
}

# Example usage:
# Create some example data
set.seed(123)
n <- 100
x1 <- rnorm(n)
x2 <- rnorm(n)
y <- 1 + 2 * x1 + 3 * x2 + rnorm(n, sd = 1)

# Put data in a dataframe
data <- data.frame(y = y, x1 = x1, x2 = x2)

# Call the function with a formula and the data
result <- mle_linear_regression(y ~ x1 + x2, data)
print(result)
