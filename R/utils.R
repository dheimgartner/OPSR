get_z <- function(object) {
  mf <- model.frame(object)
  z_name <- attr(terms(object$formula), "term.labels")[1]
  mf[, z_name]
}

get_y <- function(object) {
  mf <- model.frame(object)
  y_name <- attr(terms(object$formula), "term.labels")[2]
  mf[, y_name]
}
