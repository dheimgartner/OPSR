## high-level Formula interface to the workhorse opsr.fit()
## most of the arguments of opsr() are passed to model.frame()
## dots are passed to maxLik
opsr <- function(formula, data, subset, weights, na.action, start = NULL,
                 method = "NM", iterlim = 50000, printLevel = 2, ...) {
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "weights", "na.action"), names(mf), 0)
  mf <- mf[c(1, m)]

  f <- Formula::Formula(formula)
  mf[[1]] <- as.name("model.frame")
  mf$formula <- f
  mf <- eval(mf, parent.frame())

  ## prep args for opsr.fit()
  l <- length(f)
  nRes <- l[1]

  if (nRes != 2) {
    stop("formula accepts two responses (selection and continuous outcome).",
         " However, ", nRes, " were specified.")
  }

  nParts <- l[2]
  Z <- Formula::model.part(f, data = mf, lhs = 1, drop = TRUE)
  Y <- Formula::model.part(f, data = mf, lhs = 2, drop = TRUE)
  nReg <- length(unique(Z))
  nObs <- length(Y)

  if (any(sort(unique(Z)) != 1:max(Z))) {
    stop("Selection outcome must be ordered starting from 1 in increasing fashion",
         " without any gaps. However, unique levels are ", unique(Z))
  }

  if (nParts != 1 && nParts != nReg + 1) {  # +1 for W (selection)
    stop("formula parts must match the number of selection outcomes + 1", nReg + 1,
         " or 1 (if the same specification is used for the selection and all",
         " continuous outcomes. However, ", nParts, " were specified.")
  }

  w <- as.vector(model.weights(mf))
  if (!is.null(w) && !is.numeric(w)) {
    stop("'weights' must be a numeric vector")
  }
  if (is.null(w)) {
    w <- rep(1, length(Y))
  }

  ## reorder weights to match with shuffling in opsr.fit() where we compute
  ## likelihood values for all elements Z == 1, then Z == 2, etc. and then
  ## stack them
  w <- w[order(Z)]

  W <- model.matrix(update(f, ~ . -1), mf, rhs = 1)  # no intercept (identification threshold)!
  Ws <- lapply(seq_len(nReg), function(i) {
    W[Z == i, ]
  })

  Xs <- lapply(seq_len(nReg), function(i) {
    ## if the same outcome equation applies
    rhs <- ifelse(nParts == 1, 1, i + 1)  # first is for selection process
    X <- model.matrix(f, mf, rhs = rhs)
    X[Z == i, ]
  })

  Ys <- lapply(seq_len(nReg), function(i) {
    Y[Z == i]
  })

  ## check or generate starting values (theta)
  if (!is.null(start)) {
    expected <- round(opsr_generate_start(W, Xs, Z, Ys), digits = 3)  # only do this, if runtime is relatively short
    if (is.null(names(start)) || !all(names(start) %in% names(expected))) {
      stop("'start' does not conform. Check '?opsr_generate_start'. Here are the",
           " auto-generated starting values: ", deparse(substitute(expected)))
    }
  } else {
    start <- opsr_generate_start(W, Xs, Z, Ys)
  }

  fit <- opsr.fit(Ws, Xs, Ys, start, w,
                  method, iterlim, printLevel, ...)

  ## return also some other useful information
  fit$call <- match.call()
  fit$start <- start
  fit$nReg <- nReg
  fit$nObs <- c(Total = nObs, setNames(c(table(Z)), paste0("o", seq_len(nReg))))

  class(fit) <- c("opsr", class(fit))

  fit
}
