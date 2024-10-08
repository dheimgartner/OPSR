#ifndef OPSR_UTILS
#define OPSR_UTILS

#include <RcppArmadillo.h>

Rcpp::List opsr_prepare_coefs(Rcpp::NumericVector& theta, int& nReg);
double dnorm_double(double x);
double pnorm_double(double x);

#endif
