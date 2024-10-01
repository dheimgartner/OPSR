opsr_model_matrices <- function(object, j, newdata = NULL, z = NULL) {
  if (is.null(newdata)) {
    data_env <- environment(object$formula)
    data_nm <- object$call$data
    data <- get(data_nm, envir = data_env)
  } else {
    data <- newdata
  }

  j_ <- ifelse(j >= object$nParts, 2, j + 1)  # if same specification for all outcomes (first is selection)
  X <- model.matrix(object$formula, data = data, rhs = j_)
  Z <- Formula::model.part(object$formula, data = data, lhs = 1, drop = TRUE)
  W <- model.matrix(update(object$formula, ~ . -1), data = data, rhs = 1)  # again, drop intercept

  ## for formula 8 (counterfactual) we need to subset other than Z == j
  filt <- ifelse(is.null(z), j, z)
  X_j <- X[Z == filt, ]
  W_j <- W[Z == filt, ]

  list(X_j = X_j, W_j = W_j)
}
