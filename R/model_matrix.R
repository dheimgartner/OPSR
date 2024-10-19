#' @export
model.matrix.opsr <- function(object, data, filter = NULL, ...) {
  mf <- if (missing(data)) model.frame(object) else model.frame(object, data = data)
  f <- object$formula
  nReg <- object$nReg
  nParts <- object$nParts
  z <- Formula::model.part(f, data = mf, lhs = 1, drop = TRUE)
  y <- Formula::model.part(f, data = mf, lhs = 2, drop = TRUE)

  w <- model.matrix(update(f, ~ . -1), mf, rhs = 1)  # no intercept (identification threshold)!
  W <- lapply(seq_len(nReg), function(i) {
    j <- filter %||% i
    z_not_present <- ifelse (all(isFALSE(z == j)), TRUE, FALSE)
    if (z_not_present) NULL else as.matrix(w[z == j, ])
  })

  X <- lapply(seq_len(nReg), function(i) {
    rhs <- ifelse(nParts == 2, 2, i + 1)  # first is for selection process
    x <- model.matrix(f, mf, rhs = rhs)
    j <- filter %||% i
    z_not_present <- ifelse (all(isFALSE(z == j)), TRUE, FALSE)
    if (z_not_present) NULL else as.matrix(x[z == j, ])
  })

  list(W = W, X = X)
}
