#ifndef OPSR_UTILS
#define OPSR_UTILS

#include <RcppArmadillo.h>

Rcpp::List opsr_prepare_coefs(Rcpp::NumericVector& theta, int nReg);

#endif
