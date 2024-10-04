#' @export
model.matrix.opsr <- function(object, data = environment(object), filter = NULL, ...) {
  if (missing(data) || is.null(data)) {
    data <- model.frame(object)
  }
  f <- object$formula
  nReg <- object$nReg
  nParts <- object$nParts
  z <- Formula::model.part(f, data = data, lhs = 1, drop = TRUE)
  y <- Formula::model.part(f, data = data, lhs = 2, drop = TRUE)

  w <- model.matrix(update(f, ~ . -1), data, rhs = 1)  # no intercept (identification threshold)!
  W <- lapply(seq_len(nReg), function(i) {
    j <- filter %||% i
    w[z == j, ]
  })

  X <- lapply(seq_len(nReg), function(i) {
    rhs <- ifelse(nParts == 2, 2, i + 1)  # first is for selection process
    x <- model.matrix(f, data, rhs = rhs)
    j <- filter %||% i
    x[z == j, ]
  })

  list(W = W, X = X)
}
