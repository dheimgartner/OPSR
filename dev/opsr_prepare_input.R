opsr_prepare_input <- function(formula, data, subset, weights, na.action) {
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

  if (is.factor(Z)) {
    stop("Selection outcome has to be numeric (and not a 'factor').")
  }

  Y <- Formula::model.part(f, data = mf, lhs = 2, drop = TRUE)
  nReg <- length(unique(Z))
  nObs <- length(Y)

  if (any(sort(unique(Z)) != 1:max(Z))) {
    stop("Selection outcome must be ordered starting from 1 in increasing fashion",
         " without any gaps. However, unique levels are ", unique(Z))
  }

  if (nParts != 2 && nParts != nReg + 1) {  # +1 for W (selection)
    stop("formula parts must be of length ", nReg + 1, " or 2 (if the same",
         " specification is used for all continuous outcomes. However, ", nParts,
         " were specified.")
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
    rhs <- ifelse(nParts == 2, 2, i + 1)  # first is for selection process
    X <- model.matrix(f, mf, rhs = rhs)
    X[Z == i, ]
  })

  Ys <- lapply(seq_len(nReg), function(i) {
    Y[Z == i]
  })

  ## collect
  input <- list()
  input$nObs <- nObs
  input$nReg <- nReg
  input$nParts <- nParts
  input$formula <- f
  input$weights <- w
  input$W <- W
  input$Ws <- Ws
  input$Xs <- Xs
  input$Z <- Z
  input$Ys <- Ys

  class(input) <- c("opsr.input", class(input))
  input
}
