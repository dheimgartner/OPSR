# Notes

## Keep in mind

- Render vignette using devtools::build_vignettes() => renders to doc
  - Conform to jss-template.Rnw styleguide
  - This uses the Sweave engine and chunk options

## Steps (to submit to CRAN)

**Read and follow chapter 22 (https://r-pkgs.org/release.html)**

>Run `devtools::check()` early and often (`R CMD check .`)
>Use `usethis::use_github_action("check-standard")` for cross-platform checks.
> `usethis::use_readme_rmd()` and `devtools::build_readme()`
> `devtools::submit_cran()`

## A little on testing

>Init via `usethis::ues_testthat(3)`

- Good practice to keep `R/` and `tests/testthat/` aligned (i.e., for each file write a testsuit with the `test-` filename)
- `usethis::use_test("foo")`
- `devtools::test_coverage()`
- If necessary, use `skip_on_cran()` (preferably nested in `test_thath()`)
- `expect_equal()` accepts a tolerance argument (`expect_identical()` not)

## Next steps

-[x] Write opsr_check_start
-[x] Compare starting values (with Xinyi)
-[x] Compute robust standard errors from maxLik output? => see sandwich notes in dev.R
  - Seems pretty easy (based on hessian and gradient which is returned by maxLik) => see also sandwich R package (maybe read JSS paper)
  - stdev <- sqrt(abs(diag(solve(fit$hessian)))) as in OPSR MLE produces the same standard errors as in summary(fit) => these are note robust!
  - I think robust se and stuff should be computed in summary.opsr => yes, summary is conventionally for all the inference stuff
  - default summary.maxLik() already useful => just wrap? => learned from it (e.g., stats::printCoefMat)
  - => then write print.summary.opsr!
  
-[x] Think about what predict method should return (see also stata paper)
  - => oriented on stata but omitted some stuff (because I don't see an obvious usecase to return these values...) added unlog-response
  - Predict without group arg => predict for all with the respective observed groups?
-[ ] Replicate treatment effects => see dev/xinyi/OPSR_treatment effect.R
  - => ask Xinyi (I can't run OPSR_treatment effect)
  
-[x] Write (extractor) methods (if not already inherited) => e.g., update(), residuals(), fitted() [which needs predict], etc.
  - Try whether anova works

-[x] Implement anova.opsr
  -[x] Understand anova.glm (for one model => in particular construction of table passed to stat.anova) => key: these statistics (columns in table)
    - `object$df.residual` and `object$deviance` are key!
  -[x] Implement parsing logic in anova.opsr (until call to anova.opsrlist)
  -[x] Generate table passed to stat.anova(.opsr) for one model in anova.opsr
  -[x] Refactor for multiple models in anova.opsrlist

-[x] Wald test on H0: rho1 == rho2 == ... (see stata paper) => see also lmtest which has waldtest() function (hint from sandwich paper)
  - Stata paper there they compare to the Null model (all params == 0) Wald chi2(7) => how to do this?
  -[x] Model where you fix rhoj = 0 and then wald test (=> separate function [part of summary.opsr])
  -[x] Maybe try making lmtest::waldtest and/or anova to work and compare to car::linearHypothesis (implemented in summary).
    - But waldtest and anova do different things: waldtest is test on coefficient restrictions and anova is comparison of difference in deviance... (see discussion in dev.R)

-[x] GOF indicators R2 and stuff for whole model and submodels? => How did Xinyi do it? => check out her GOF script (OPSR_treatment effect.R)
-[x] Think about reordering the output of ll2 (original order)

-[x] Texreg stuff (compare regimes => set beside = TRUE)
  -[ ] Maybe include R^2 for regimes... => R^2 (1), R^2 (2), ...
-[x] Write tests (first think about what to test)
  -[x] Write fixture with simulated data (s.t. it is always the same data)
  -[x] Init for each (important) file in `R/` a test file with `usethis::use_test()` and write what to test with ## => see also `dev.R` for insp

-[ ] Update documentation (outsource examples [see mixl])
  - [ ] how to document texreg extract?
  - [ ] consider what to export (e.g., should S3 methods be exported => yes they should! => see for example `help(package = "stats")`)
  - [ ] cross references (e.g. seealso seem to complain) => use separate `[]` for each function!
  - [ ] update README and compile
-[ ] `devtools::submit_cran()`: Read and follow chapter 22 (https://r-pkgs.org/release.html)
-[ ] Write paper (read jss.pdf and learn from others)

-[x] OpenMP parallelize (see Statistical Computing in C++ and R pdf => has chapter on it)


## General

-[x] Generalization of own_loglik_imp => Formula (parsing) and preparing of inputs (in particular theta and its names => i.e. similar to mvProbitPrepareCoef() and mvProbitCoefNames()) => also implement some minimal checks of user input and try to conform to the suggestions below (e.g., first wrap Formula or metaprogramming for model.frame mf, etc.)
-[x] Formula vignette (read printed from start)
-[x] understand nLL_simple function (WRITE YOUR OWN IMPLEMENTATION OF FORMULA 6!) => then try to generalize ll func
  - (could we somehow stack the matrices? or some other generalization trick?)
  - How to construct the parameter vector (names) from the Formula (maybe see mlogit for idea)
    - probably based on variable names and prefix for selection and outcome_j
    - mvProbit https://github.com/cran/mvProbit/blob/master/R/mvProbit.R
    - see mvProbitPrepareCoef() and mvProbitCoefNames() in mvProbit()
    - mvProbitLogLikInternal is more or less the idea of mvProbit.fit()

- If we have no error correlation then OPSR should yield ordinal probit and lin reg estimates (write test case).
-[x] What formula interface to use (list of formulas vs. Formula package https://www.jstatsoft.org/article/view/v034i01)
  - Formula!
-[x] Formula vignette has very useful example! ivcoef()
  - lm.fit is the workhorse, no longer taking a formula but simply a response and model matrix
  - the lm just parses the formula and prepares the input
  - see 4 steps in Formula vignette
  -[x] understand (1) processing the call
-[x] lm() which is a high-level formula interface to the work-horse lm.fit() function
  -[x] Follow this convention opsr() and opsr.fit()
-[x] The (conditional) expectation formulas in the paper can be used for predict function
  - Maybe one should add functionality to predict the ordered outcome in isolation
  - We should also compute the standard error of the (conditional) expectation (treatment effect)
  - Using the delta method: see delta_method.R
    - However, not that easy...
-[x] First in opsr(formula, ...) => wrap f <- Formula(formula)
-[x] How to create reasonable starting values? 2-step? For selection regular ordinal probit should do the trick, right? => how did Xinyi do it (also for sigma and rho)?
  - See maybe Chiburis and/or Jimenez (references in Xinyi)

-[x] How to include weights => asked chat already => just multiply loglik (see also mixl)
-[x] Check when to use intercepts (in particular for selection process)
- Write test cases (e.g., hard-coded maxLik with simulated data should yield same)
  - However, you probably don't want to execute all of these tests on cran or package build...
- BFGS converges much faster!
-[x] Formula 7 (conditional probability) => shouldn't it be W_j gamma (instead W gamma)?
  - otherwise you have a dimensionality missmatch W gamma is a vector of length n
  - And what about formula 8?
-[x] rho has to be positive, right? should we use log(rho) (or something) in max lik estimation? and then backtransform?
  - Doesn't have to be! In Stata they censor rho to lie in [-0.85, 0.85] => so if in 2-step rho = 0.86 they use rho = 0.85
-[x] Tobit-5 model (switching regression for binary regime) => see sampleSelection
  - make sense of terminology (general multiple process models or something => difference to heckman, hurdle, zero-inflated, tobit, simultaneous equation, etc.) => see Cameron
-[x] Identification issues?
  - See page 551 in Cameron => could be due to same explanatory variables X in all processes! => note this in paper!
  -[x] Maybe change in opsr() => must specify different processes (for selection and outcome)

- Mention switchSelection very powerful and much more flexible than OPSR (however, accompanying papers only in Russian) and rather for advanced users - provide example.
- How to compute standard errors for the 2-step procedure => regular OLS standard errors are not reliable since the regression includes an estimate as explanatory variable (see Cameron 16.5.4 for the binary selection).
- See also ssmrob package (and jss article => robust analysis of sample selection models in R)
- Use examples @example R/examples/example_script.R (see mixl)

## Literature

Cameron is an excellent review!

- Tobit models originally developed to handle truncated, censored or interval data
  - Only one latent process y* that determines both the censoring mechanism and the outcome
- Then generalization of the original Tobit model => Two-part models (excessive zeros) and sample selection models (where we only observe an outcome for a subsample of the whole population)
  - These generalizations come with many different names type 2 (5) Tobit model, generalized Tobit model, sample selection model or if the selection mechanism is named specifically probit selection equation.
  - However, conceptually, they are all very similar.
- A further generalization is then the switching regression, where the outcome is not only observed for a subsample but the population is partitioned into different groupes (regimes) and a separate process governs the observed outcome. E.g., originally known as the Roy model (also referred to as switching regression model in Maddala and Tobit type 5 model in Amemiya) which is a generalization of the bivariate sample selection model (where now the continuous outcome is observed for all individuals and not just the selected ones).
  - I think Tobit 5 is implemented in sampleSelection package.
- So maybe at the conceptual highest level it is easiest to differentiate whether y censored or truncated, observed only for a subsample or observed for the full sample but stemming from separate processes.
  - See also Amemya p. 384 table 10.2 => nice!
- Many different variants can then be derived by either placing different distributional assumptions on the errors of these latent processes (e.g. multivariate normal) and/or how the latent process manifests into observed outcomes (e.g., continuous outcomes vs. binary outcomes). While originally the sample selection models were developed for binary selection, also the selection process can be extended even to the multivariate case.
- All of these variantes have the same legitimization - namely that regular OLS is rendered inconsistent if the errors of the latent processes are correlated.
