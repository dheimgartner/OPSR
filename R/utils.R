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

#' @export
nobs.opsr <- function(object, ...) {
  object$nObs[["Total"]]
}
