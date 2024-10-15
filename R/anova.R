#' @export
anova.opsr <- function(object, ...) {
  dotargs <- list(...)
  named <- if (is.null(names(dotargs)))
    rep_len(FALSE, length(dotargs))
  else (names(dotargs) != "")
  if (any(named))
    warning("the following arguments to 'anova.opsr' are invalid and dropped: ",
            paste(deparse(dotargs[named]), collapse = ", "))
  dotargs <- dotargs[!named]
  is.opsr <- vapply(dotargs, function(x) inherits(x, "opsr"), NA)
  dotargs <- dotargs[is.opsr]
  ## if multiple models are passed
  if (length(dotargs))
    return(anova.opsrlist(c(list(object), dotargs)))
  ## else create null model and then call anova.opsrlist
  x <- capture.output(
    null_model <- opsr_null_model(object)
  )

  return(anova.opsrlist(c(list(null_model, object))))
}

#' @export
anova.opsrlist <- function(object, ...) {
  prepare_anova <- function(object) {
    ll <- stats::logLik(object)
    df <- attr(ll, "df")
    c("logLik" = as.numeric(ll), "Df" = df)
  }
  ## for all model objects extract
  anova_table <- Reduce(rbind, lapply(object, prepare_anova), numeric())

  ## main call
  table <- stat.anova.opsr(anova_table)

  ## add stuff for print.anova.opsr
  anova_opsr <- list()
  anova_opsr$formulas <- lapply(object, function(x) x$formula)
  anova_opsr$table <- table
  class(anova_opsr) <- c("anova.opsr")
  anova_opsr
}


stat.anova.opsr <- function(table, test = "LRT", ...) {  # could be extended with other tests...
  nModels <- nrow(table)
  ## do the following for each pair
  lrtest2 <- function(table, row1, row2) {
    out <- list()
    out$test <- as.numeric(2 * abs(table[row1, "logLik"] - table[row2, "logLik"]))
    out$restrictions <- abs(table[row1, "Df"] - table[row2, "Df"])
    out$p_value <- 1 - pchisq(out$test, df = out$restrictions)
    out
  }
  lrpairs <- vector("list", nModels - 1)
  row1 <- 1
  row2 <- 2
  for (i in 1:(nModels - 1)) {
    lrpairs[[i]] <- lrtest2(table, row1, row2)
    row1 <- row2
    row2 <- row2 + 1
  }

  test_stats <- do.call(rbind, lapply(lrpairs, as.data.frame))
  test_stats <- rbind(rep(NA, 3), test_stats)

  ## combine in one table
  lrtest_table <- cbind(table, test_stats)
  colnames(lrtest_table) <- c(colnames(table), "Test", "Restrictions", "Pr(>Chi)")
  rownames(lrtest_table) <- 1:nModels
  lrtest_table
}

#' @export
print.anova.opsr <- function (x, digits = max(getOption("digits") - 2L, 3L), signif.stars = getOption("show.signif.stars"),
                              ...) {
  cat("Likelihood Ratio Test\n\n")
  for (i in seq_along(x$formulas)) {
    cat("Model ", i, ": ", deparse(x$formulas[[i]]), "\n", sep = "")
  }
  stats::printCoefmat(x$table, digits = digits, signif.stars = signif.stars, na.print = "", ...)
  invisible(x)
}
