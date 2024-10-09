#' @export
summary.opsr <- function(object, rob = TRUE, ...) {
  model <- object

  ## LL
  LL_2step <- sum(model$objectiveFn(model$start))
  LL_final <- model$maximum



  ## what about R2 stuff??



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

  vc <- if (rob) sandwich::sandwich(model) else vcov(model)
  cofi <- coef_inf(model, vc)

  coef_table <- data.frame(
    est = model$estimate,
    se = cofi$se,
    t_val = cofi$trat_0,
    p_val = cofi$pval_0
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
  ms$n_params <- length(model$estimate) - sum(model$fixed)
  ms$n_regimes <- model$nReg
  ms$n_obs <- model$nObs

  ms$GOF <- list(
    LL_2step = LL_2step,
    LL_final = LL_final,
    AIC = AIC(model),
    BIC = BIC(model)
  )

  class(ms) <- "summary.opsr"
  ms
}
