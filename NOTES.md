# Notes


## PROCEED ##
Tidy up repo and think about next steps (probably make package dev version and share => minimal next steps for Xinyi)
- Would be great to test opsr() with the data and specification from https://www.sciencedirect.com/science/article/pii/S0965856424001204
- Could you share the data with me? It would be great to attach it to the package (and conduct a replication exercise - maybe for one table) - would this be possible?
- opsr_generate_start() currently estimates an OL for the selection process and separate linear models for the outcomes (sigma is set to 1 and rho to 0). How did you implement the 2-step estimation procedure (to arrive at reasonable starting values)? Could you share the code (or feel free to implement it yourself)!
- standard errors for conditional expectations / treatment effects (otherwise use delta method)
- Do you have a github account => collaboration would be easiest via github (attached is the package)

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
- lm() which is a high-level formula interface to the work-horse lm.fit() function
  -[x] Follow this convention opsr() and opsr.fit()
- The (conditional) expectation formulas in the paper can be used for predict function
  - Maybe one should add functionality to predict the ordered outcome in isolation
  - We should also compute the standard error of the (conditional) expectation (treatment effect)
-[x] First in opsr(formula, ...) => wrap f <- Formula(formula)
- How to create reasonable starting values? 2-step? For selection regular ordinal probit should do the trick, right? => how did Xinyi do it (also for sigma and rho)?
- How to compute robust standard errors from maxLik output? => asked chat already => seems pretty easy (based on hessian and gradient which is returned by maxLik) => see also sandwich R package (maybe read JSS paper)
  - stdev <- sqrt(abs(diag(solve(fit$hessian)))) as in OPSR MLE produces the same standard errors as in summary(fit) => these are note robust!
  - I think robust se and stuff should be computed in summary.opsr
    - default summary.maxLik() already useful => just wrap?
-[x] How to include weights => asked chat already => just multiply loglik (see also mixl)
-[x] Check when to use intercepts (in particular for selection process)
- Write test cases (e.g., hard-coded maxLik with simulated data should yield same)
  - However, you probably don't want to execute all of these tests on cran or package build...
