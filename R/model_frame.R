#' @export
model.frame.opsr <- function(formula, ...) {
  ## almost identical to model.frame.lm
  object <- formula
  dots <- list(...)
  nargs <- dots[match(c("data", "na.action", "subset"), names(dots), 0)]
  if (length(nargs)  || is.null(object$model)) {
    mf <- object$call
    m <- match(c("formula", "data", "subset", "weights", "na.action"), names(mf), 0)
    mf <- mf[c(1, m)]
    mf[[1]] <- as.name("model.frame")
    mf[names(nargs)] <- nargs  # overwrite
    mf$formula <- object$formula
    mf <- eval(mf, environment(object))
    mf
  } else {
    object$model
  }
}
