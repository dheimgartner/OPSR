% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pairs_opsr_te.R
\name{pairs.opsr.te}
\alias{pairs.opsr.te}
\title{Pairs Plot for OPSR TE Objects}
\usage{
\method{pairs}{opsr.te}(
  x,
  pch = 21,
  labels.diag = paste0("T", 1:x$nReg),
  labels.reg = paste0("G", 1:x$nReg),
  col = 1:x$nReg,
  add.rug = TRUE,
  lower.digits = 0,
  diag.digits = 0,
  lwd.dens = 1.5,
  diag.cex.text = 1,
  upper.digits = 2,
  upper.cex.text = 2,
  prefix = "",
  postfix = "",
  lty.diag = 1,
  ...
)
}
\arguments{
\item{x}{an object of class \code{"opsr.te"}.}

\item{pch}{plotting 'character', i.e., symbol to use. See also \code{\link{pch}}.}

\item{labels.diag}{labels used in the diagonal panels.}

\item{labels.reg}{labels for the treatment regimes.}

\item{col}{colour vector.}

\item{add.rug}{if \code{TRUE}, adds rugs to the lower panels.}

\item{lower.digits}{rounding of the digits in the lower panel.}

\item{diag.digits}{rounding of the digits in the diagonal panel.}

\item{lwd.dens}{linewidth of the densities in the diagonal panel.}

\item{diag.cex.text}{\code{cex} for the text in the diagonal panel.}

\item{upper.digits}{rounding of the digits in the upper panel.}

\item{upper.cex.text}{\code{cex} for the text in the upper panel.}

\item{prefix}{for the number plotted in the upper panel.}

\item{postfix}{for the number plotted in the upper panel.}

\item{lty.diag}{linetype for the diagonal panel.}

\item{...}{further arguments passed to or from other methods.}
}
\value{
Returns \code{x} invisibly.
}
\description{
Pairs Plot for OPSR TE Objects
}
\details{
Presents all potential counterfactual outcomes. The diagonal depicts
distributions in any given treatment regime and separate by the current
(factual) treatment group. The weighted mean values are shown as red numbers.
The lower triangular panels compare the model-implied (predicted) outcomes
of two treatment regimes again separate by current treatment group. The red
line indicates the 45-degree line of equal outcomes while the red squares
depict again the weighted mean values. The upper triangular panels show
(weighted) average treatment effects.
}
\examples{
sim_dat <- opsr_simulate()
dat <- sim_dat$data
weights <- runif(nrow(dat))
fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat = dat, weights = weights,
            printLevel = 0)
te <- opsr_te(fit, type = "response")
print(te)
summary(te)

te_w <- opsr_te(fit, type = "response", weights = rep(1, nrow(dat)))
summary(te_w)

pairs(te)
}
\seealso{
\code{\link{pairs}}
}
