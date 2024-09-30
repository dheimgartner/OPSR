library(Formula)

test <- y ~ x1
class(test)
sloop::otype(test)
methods(class = "formula")

y <- rnorm(n = 100)
x1 <- rnorm(n = 100)
plot(test)
terms(test)

## going through the vignette
help(package = "Formula")


set.seed(1090)
dat <- as.data.frame(matrix(round(runif(21), digits = 2), ncol = 7))
colnames(dat) <- c("y1", "y2", "y3", "x1", "x2", "x3", "x4")
for(i in c(2, 6:7)) dat[[i]] <- factor(dat[[i]] < 0.5,
                                       labels = c("a", "b"))
dat$y2[1] <- NA
dat

f <- log(y1) ~ x1 + x2 | I(x1^2)
length(f)
F1 <- Formula(f)
length(F1)

sloop::otype(F1)
attributes(F1)
attr(F1, "rhs")
(uF1 <- unclass(F1))
class(uF1)

mf1 <- model.frame(F1, data = dat)
mf1

## extracting response from model frame
model.response(mf1)

## constructing separate model matrices
model.matrix(F1, data = mf1, rhs = 1)

model.matrix(F1, data = mf1, rhs = 2)

## multiple responses
F2 <- Formula(y1 + y2 ~ x3)
length(F2)

mf2 <- model.frame(F2, data = dat)
model.response(mf2)
model.part(F2, data = mf2, lhs = 1)
# model.part(F2, data = mf2, rhs = 1)  # != model.matrix(F2, data = mf2, rhs = 1)

## REMEMBER:
## model.frame extracts all variables and model.matrix literally X; model.response Y
## model.frame accepts subsets!

## TODO: write down all extractor functions (model.frame, model.matrix, model.weights, response, etc.)

F7 <- Formula(y1 | y2 ~ x1 | x2)
mf7 <- model.frame(F7, data = dat)
model.part(F7, data = mf7, lhs = 1)
model.part(F7, data = mf7, lhs = 2)
model.part(F7, data = mf7, rhs = 1)

## extracting formula and terms
## I think this is mostly implementation detail and in OPSR we can use the regular
## functions model.frame() etc. without having to worry too much about terms objects, etc.
F3 <- Formula(y1 + y2 | log(y3) ~ x1 + I(x2^2) | 0 + log(x1) | x3 / x4)
F3
formula(F3, lhs = 1, rhs = 1)
formula(F3, lhs = 1, rhs = -2)  # drop the second part

## computing model frames, matrices and responses
dat
mf3 <- model.frame(F3, data = dat, subset = y1 < 0.75, weights = x1)
mf3

## all subsequent computations are then based on this model frame (and possibly the original "Formula")
F3
model.matrix(F3, data = mf3, rhs = 2)

## the LHS can be extracted with model.part()
model.part(F3, data = mf3, lhs = 1)

model.weights(mf3)

## further methods: update() and as.Formula()
F1
update(F1, . ~ . - x1 | . + x1)  # updates part by part . means don't change anything

as.Formula(y1 ~ x1, y2 ~ x2, ~ x3)

## usage in model fitting functions
## typical workflow of model-fitting functions and processing its arguments:
## (1) process the call
## (2) set up model frame
## (3) extract response and regressors
## (4) estimate

help(package = "mlogit")

ivcoef <- function(formula, data, subset, na.action, ...)
{
  ## some meta programming to construct the model.frame call from the call above
  ## no need to provide default args => see ?match.call Details
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "na.action"), names(mf), 0)
  mf <- mf[c(1, m)]

  f <- Formula(formula)
  mf[[1]] <- as.name("model.frame")
  mf$formula <- f
  mf <- eval(mf, parent.frame())

  y <- model.response(mf)
  x <- model.matrix(f, data = mf, rhs = 1)
  z <- model.matrix(f, data = mf, rhs = 2)

  xz <- as.matrix(lm.fit(z, x)$fitted.values)
  lm.fit(xz, y)$coefficients
}

debugonce(ivcoef)
ivcoef(log(y1) ~ x1 | x2, data = dat)

dat
f <- log(y1) ~ x1 + x2 | x3 + x4
ivcoef(f, data = dat)

## TAKEAWAY

## so I guess in OPSR one would specify a model something like this
## selection_outcome | continuous_outcome ~ x1 + x2 | x2 + x3
##                                          ^^^^^^^   ^^^^^^^
##                                         selection continuous

## if different process for each group
## z | y ~ x1 + x2 | x2 + x3 | ... where the RHS needs to be length groups + 1

## wrap opsr(formula, ...) => f <- Formula(formula)
## first construct model frame and then derive response and matrix from it
## model.frame()
## model.part() for multi-response (selection and outcome)
## model.matrix()
## see chapter usage in model fitting functions for usual workflow (also summarized above)
## => in particular match.call() workflow
## conform to processing of Formula and then pass to workhorse opsr.fit()

