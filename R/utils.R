get_z <- function(object, data) {
  mf <- if (missing(data)) model.frame(object) else model.frame(object, data = data)
  z_name <- attr(terms(object$formula), "term.labels")[1]
  mf[, z_name]
}

get_y <- function(object, data) {
  mf <- if (missing(data)) model.frame(object) else model.frame(object, data = data)
  y_name <- attr(terms(object$formula), "term.labels")[2]
  mf[, y_name]
}

censor <- function(x, lower, upper) {
  pmin(pmax(x, lower), upper)
}

#' @export
nobs.opsr <- function(object, ...) {
  object$nObs[["Total"]]
}

#' @export
fitted.opsr <- function(object, ...) {
  p <- lapply(seq_len(object$nReg), function(j) {
    ## this is the conditional expectation
    predict(object, group = j, type = "response")
  })
  p_df <- Reduce(cbind, p)
  fitted <- rowSums(p_df, na.rm = TRUE)
  fitted
}

#' @export
residuals.opsr <- function(object, ...) {
  y <- get_y(object)
  y - fitted(object)
}

#' Model updating
#' @seealso [update.formula]
#' @export
update.opsr <- function(object, ...) {
  NextMethod("update", object)
}
