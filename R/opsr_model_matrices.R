#' Construct design matrices
#'
#' Somewhat similar to [`model.matrix`] creates a design (or model) matrix from
#' the `formula` (attached to `object`) used when fitting the OPSR model. Creates
#' the design matrix for a specific outcome group `yo`.
#'
#' @param object an object of class `"opsr"`.
#' @param yo creates the design matrix for the process of group `yo` (ordered
#'   outcome level).
#' @param newdata an optional data frame in which to look for varialbes used in
#'   `object$formula`. If omitted, `environment(object$formula)` is used.
#' @param z Sometimes it is useful to create the design matrix for `yo` but filter
#'   the observations belonging to another group `z` (e.g. in [`predict.opsr`]).
#'
#' @return list of design matrices `X_j` and `W_j`.
#' @export
opsr_model_matrices <- function(object, yo, newdata = NULL, z = NULL) {
  if (is.null(newdata)) {
    data_env <- environment(object$formula)
    data_nm <- object$call$data
    data <- get(data_nm, envir = data_env)
  } else {
    data <- newdata
  }

  j <- ifelse(yo >= object$nParts, 1, yo + 1)  # if same specification for all outcomes (first is selection)
  X <- model.matrix(object$formula, data = data, rhs = j)
  Z <- Formula::model.part(object$formula, data = data, lhs = 1, drop = TRUE)
  W <- model.matrix(update(object$formula, ~ . -1), data = data, rhs = 1)  # again, drop intercept

  ## for formula 8 (counterfactual) we need to subset other than Z == yo
  z <- ifelse(is.null(z), yo, z)
  X_j <- X[Z == z, ]
  W_j <- W[Z == z, ]

  list(X_j = X_j, W_j = W_j)
}
