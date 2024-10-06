#ifndef OPSR_UTILS
#define OPSR_UTILS

#include <Rcpp.h>

Rcpp::List opsr_prepare_coefs(Rcpp::NumericVector& theta, int& nReg);
double dot(Rcpp::NumericVector x, Rcpp::NumericVector y);
double dnorm_double(double x);
double pnorm_double(double x);

#endif
