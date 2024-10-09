#' @export
summary.opsr <- function(object, rob = TRUE, ...) {
  model <- object
  varcov <- if (rob) sandwich::sandwich(model) else vcov(model)

  ## LL
  LL2step <- sum(model$objectiveFn(model$start))
  LLfinal <- model$maximum



  ## what about R2 stuff (see also Xinyi paper)



  ## wald test
  wald_test <- function(model, hypothesis, varcov) {
    wt <- car::linearHypothesis(model, hypothesis, vcov. = varcov)
    out <- list()
    out$df <- wt[["Df"]][2]
    out$chisq <- wt[["Chisq"]][2]
    out$pval <- wt[["Pr(>Chisq)"]][2]
    out
  }

  ## independent equations (rho = 0)
  nm <- names(coef(model))
  pattern <- "^rho"
  h_rho <- nm[grepl(pattern, nm)]
  wald_test_rho <- wald_test(model, h_rho, varcov)

  ## constants only model
  pattern <- "^kappa|^sigma|^rho|(Intercept)"
  h_null <- nm[!grepl(pattern, nm)]
  wald_test_null <- wald_test(model, h_null, varcov)

  coef_inf <- function(model, varcov) {
    varcov[, model$fixed] <- NA
    varcov[model$fixed, ] <- NA
    se <- sqrt(diag(varcov))
    se[model$fixed] <- NA
    trat_0 <- model$estimate / se
    trat_1 <- (model$estimate - 1) / se
    pval_0 <- 2 * stats::pnorm(-abs(trat_0))
    pval_1 <- 2 * stats::pnorm(-abs(trat_1))
    out <- list()
    out$varcov <- varcov
    out$se <- se
    out$trat_0 <- trat_0
    out$trat_1 <- trat_1
    out$pval_0 <- pval_0
    out$pval_1 <- pval_1
    out
  }

  cofi <- coef_inf(model, varcov)

  coef_table <- data.frame(
    est = model$estimate,
    se = cofi$se,
    tval = cofi$trat_0,
    pval = cofi$pval_0
  )
  colnames(coef_table) <- c("Estimate", "Std. error", "t value", "Pr(> t)")

  ms <- list()
  ms$call <- model$call
  ms$formula <- model$formula
  ms$robust <- rob
  ms$runtime <- model$runtime
  ms$maxim_type <- model$type
  ms$iterations <- model$iterations
  ms$return_code <- model$code
  ms$message <- model$message
  ms$coef_table <- coef_table
  ms$varcov <- cofi$varcov
  ms$nParams <- length(model$estimate) - sum(model$fixed)
  ms$nReg <- model$nReg
  ms$nObs <- model$nObs

  ms$GOF <- list(
    LL2step = LL2step,
    LLfinal = LLfinal,
    AIC = AIC(model),
    BIC = BIC(model)
  )

  ms$wald <- list(
    null = wald_test_null,
    rho = wald_test_rho
  )

  class(ms) <- "summary.opsr"
  ms
}
