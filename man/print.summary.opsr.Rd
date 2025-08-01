% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print_summary_opsr.R
\name{print.summary.opsr}
\alias{print.summary.opsr}
\title{Print Method for Summary OPSR Objects}
\usage{
\method{print}{summary.opsr}(x, digits = max(3L, getOption("digits") - 3L), print.call = TRUE, ...)
}
\arguments{
\item{x}{and object of class \code{"summary.opsr"}}

\item{digits}{minimum number of significant digits to be used for most numbers (passed to \code{\link[stats:printCoefmat]{stats::printCoefmat}}).}

\item{print.call}{if \code{TRUE}, prints the underlying \code{\link{opsr}} call.}

\item{...}{further arguments passed to or from other methods.}
}
\value{
Prints summary in 'pretty' form and returns \code{x} invisibly.
}
\description{
Print Method for Summary OPSR Objects
}
\seealso{
\code{\link[stats:printCoefmat]{stats::printCoefmat}}, \code{\link{summary.opsr}}
}
