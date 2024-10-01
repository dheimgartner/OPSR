opsr_model_matrices <- function(object, j, newdata = NULL) {
  if (is.null(newdata)) {
    data_env <- environment(object$formula)
    data_nm <- object$call$data
    data <- get(data_nm, envir = data_env)
  } else {
    data <- newdata
  }
  j_ <- ifelse(j > object$nReg, 2, j + 1)  # if same specification for all outcomes (first is selection)
  X <- model.matrix(object$formula, data = data, rhs = j_)
  Z <- Formula::model.part(object$formula, data = data, lhs = 1, drop = TRUE)
  X_j <- X[Z == j, ]
  W <- model.matrix(update(object$formula, ~ . -1), data = data, rhs = 1)  # again, drop intercept
  list(X_j = X_j, W = W)
}
