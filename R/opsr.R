#' Fitting ordinal probit switching regression models
#'
#' High-level formula interface to the workhorse [`opsr.fit`].
#'
#' @param formula an object of class `"Formula" "formula"`: A symbolic description
#'   of the model to be fitted. The details of model specification are given under
#'   'Details'.
#' @param data an optional data frame, list or environment (or object coercible by
#'   [`as.data.frame`] to a data frame) containing the variables in the model. If
#'   not found in `data`, the variables are taken from `environment(formula)`,
#'   typically the environment from which `opsr` is called.
#' @param subset an optional vector specifying a subset of observations to be used
#'   in the fitting process. (See additional details about how this argument
#'   interacts with data-dependent bases in the 'Details' section of the [`model.frame`]
#'   documentation.).
#' @param weights an optional vector of weights to be used in the fitting process.
#'   Should be `NULL` or a numeric vector. If non-NULL, then observation-specific
#'   log-likelihood contributions are multiplied by their corresponding weight
#'   before summing.
#' @param na.action a function which indicates what should happen when the data
#'   contain `NA`s. The default is set by the `na.action` setting of [`options`],
#'   and is [`na.fail`] if that is unset. The 'factory-fresh' default is [`na.omit`].
#'   Another possible value is `NULL`, no action. Value [`na.exclude`] can be useful.
#' @param start a named numeric vector with the starting values (passed to [`maxLik`]).
#'   If no starting values are provided, reasonable values are auto-generated via
#'   the 2-step procedure. The structure of `start` has to conform with `opsr`'s
#'   expectations. An example is included in the error message if this should not
#'   be the case.
#' @param method defaults to `"NM"` (see [`maxLik`]).
#' @param iterlim defaults to 50000 (see [`maxLik`]).
#' @param printLevel defaults to 2 (see [`maxLik`]).
#' @param ... passed to [`maxLik`]
#'
#' @return an object of class `"opsr" "maxLik" "maxim"`.
#' @export
#'
#' @details
#' Models for `opsr` are specified symbolically. A typical model has the form
#' `selection_outcome | continuous_outcome ~ selection_process | continuous_process_1 | continuous_process_2 | ...`.
#' `selection_outcome` is the ordered (numeric) response vector (starting from 1,
#' in integer-increasing fashion). For the `process` specification the rules of
#' the regular formula interface applies. See also [stats::lm] or the 'Examples'
#' section below.  The intercept in the `selection_process` is excluded automatically
#' (no need to specify `-1`). If the user wants to specify the same `process` for
#' all groups, two processes are enough (`... ~ selection_process | generic_process`).
#'
#' @examples
#' \dontrun{
#' sim_dat <- sim_dat_1()
#' dat <- sim_dat$data
#' head(dat)
#' formula <- Z | Y ~ X1 + X2 | -1 + X1 + X2 | -1 + X1 + X2 | -1 + X1 + X2
#' formula <- Z | Y ~ X1 + X2 | -1 + X1 + X2
#' system.time(
#'   fit <- opsr(formula, dat)
#' )
#' summary(fit)
#' class(fit)
#'
#' ## ground truth
#' sim_dat$params
#' sim_dat$sigma
#' }
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

  if (nParts != 2 && nParts != nReg + 1) {  # +1 for W (selection)
    stop("formula parts must match the number of selection outcomes + 1", nReg + 1,
         " or 2 (if the same specification is used for all continuous outcomes.",
         " However, ", nParts, " were specified.")
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
