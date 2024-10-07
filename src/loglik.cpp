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
NumericVector loglik(NumericVector& theta, List& W, List& X, List& Y,
                     NumericVector& weights, int nReg, int nObs) {
  int boundary;
  int current = 0;
  int min_z = 1;
  int max_z = nReg;
  List theta_, theta_j;
  NumericMatrix w, x;
  NumericVector y, ll_j, ll(nObs), ll_weighted(nObs);

  theta_ = opsr_prepare_coefs(theta, nReg);

  for (int j = 0; j < nReg; j++) {
    theta_j = theta_[j];
    boundary = (j + 1 == min_z) ? -1 : (j + 1 == max_z) ? 1 : 0;  // j + 1!

    // prepare inputs
    w = as<NumericMatrix>(W[j]);
    x = as<NumericMatrix>(X[j]);
    y = as<NumericVector>(Y[j]);

    ll_j = loglik_j(w, x, y, theta_j["gamma"], theta_j["kappa_j_1"],
                    theta_j["kappa_j"], theta_j["beta_j"], theta_j["sigma_j"],
                            theta_j["rho_j"], boundary);

    // append to ll
    for (int i = current; i < current + ll_j.size(); i++) {
      ll[i] = ll_j[i - current];
    }
    current += ll_j.size();
  }

  for (int i = 0; i < ll.size(); i++) {
    ll_weighted[i] = ll[i] * weights[i];
  }

  return ll_weighted;
}
