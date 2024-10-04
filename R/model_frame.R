#' @export
model.frame.opsr <- function(formula, ...) {
  object <- formula
  mf <- object$call
  m <- match(c("formula", "data", "subset", "weights", "na.action"), names(mf), 0)
  mf <- mf[c(1, m)]
  mf[[1]] <- as.name("model.frame")
  mf$formula <- object$formula
  mf <- eval(mf, environment(object))
  mf
}
