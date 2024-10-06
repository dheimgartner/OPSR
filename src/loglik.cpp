#include <Rcpp.h>
#include "utils.h"

using namespace Rcpp;

NumericVector loglik_j(NumericMatrix& W, NumericMatrix &X, NumericVector& y,
                       NumericVector gamma, double kappa_j_1, double kappa_j,
                       NumericVector beta_j, double sigma_j, double rho_j,
                       int boundary) {
  int n_elem = y.size();
  double res, low, high, part1, part2, part3;
  NumericVector ll(n_elem);
  double ll_i;

  RNGScope scope;  // set seed (necessary?)

  for (int i = 0; i < n_elem; i++) {
    res = y(i) - dot(X.row(i), beta_j);
    low = kappa_j_1 - dot(W.row(i), gamma);
    high = kappa_j - dot(W.row(i), gamma);
    // part1
    part1 = 1.0 / sigma_j * dnorm_double(res / sigma_j);
    // part2
    if (boundary == 1)
      part2 = 1.0;
    else
      part2 = pnorm_double((sigma_j * high - rho_j * res) / (sigma_j * sqrt(1.0 - pow(rho_j, 2))));
    // part3
    if (boundary == -1)
      part3 = 0.0;
    else
      part3 = pnorm_double((sigma_j * low - rho_j * res) / (sigma_j * sqrt(1.0 - pow(rho_j, 2))));

    ll_i = log(part1) + log(part2 - part3);
    ll[i] = ll_i;
  }

  return ll;
}

// [[Rcpp::export]]
double loglik(NumericVector& theta, List& W, List& X, List& Y,
              NumericVector& weights, int nReg) {
  int boundary;
  int min_z = 1;
  int max_z = nReg;
  List theta_j;
  NumericMatrix w, x;
  NumericVector y, gamma, beta, ll_j, ll;
  double kappa1, kappa2, sigma, rho, ll_weighted;

  List theta_ = opsr_prepare_coefs(theta, nReg);

  for (int i = 0; i < nReg; i++) {
    theta_j = theta_.at(i);
    boundary = (i + 1 == min_z) ? -1 : (i + 1 == max_z) ? 1 : 0;  // i + 1!

    // prepare inputs
    w = as<NumericMatrix>(W.at(i));
    x = as<NumericMatrix>(X.at(i));
    y = as<NumericVector>(Y.at(i));
    gamma = theta_j["gamma"];
    kappa1 = theta_j["kappa_j_1"];
    kappa2 = theta_j["kappa_j"];
    beta = theta_j["beta_j"];
    sigma = theta_j["sigma_j"];
    rho = theta_j["rho_j"];

    ll_j = loglik_j(w, x, y, gamma, kappa1, kappa2, beta, sigma, rho, boundary);
    // append to ll
    for (int i = 0; i < ll_j.size(); i++) {
      ll.push_back(ll_j[i]);
    }
  }

  ll_weighted = dot(ll, weights);

  return ll_weighted;
}
