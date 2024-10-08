#include <RcppArmadillo.h>
#include "utils.h"

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

arma::colvec loglik_j(arma::mat& W, arma::mat &X, arma::colvec& y,
                      arma::colvec gamma, double kappa_j_1, double kappa_j,
                      arma::colvec beta_j, double sigma_j, double rho_j,
                      int boundary) {
  int n_elem = y.size();
  double res, low, high, part1, part2, part3;
  arma::colvec ll(n_elem);
  double ll_i;

  RNGScope scope;  // set seed

  for (int i = 0; i < n_elem; i++) {
    res = y(i) - arma::dot(X.row(i), beta_j);
    low = kappa_j_1 - arma::dot(W.row(i), gamma);
    high = kappa_j - arma::dot(W.row(i), gamma);
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
arma::colvec loglik(NumericVector& theta, arma::field<arma::mat>& W,
                    arma::field<arma::mat>& X, arma::field<arma::colvec>& Y,
                    arma::colvec& weights, int nReg, int nObs) {
  int boundary;
  int min_z = 1;
  int max_z = nReg;
  List theta_, theta_j;
  arma::colvec ll_j(nReg), ll, ll_weighted(nObs);

  theta_ = opsr_prepare_coefs(theta, nReg);

  for (int j = 0; j < nReg; j++) {
    theta_j = theta_[j];
    boundary = (j + 1 == min_z) ? -1 : (j + 1 == max_z) ? 1 : 0;  // j + 1!

    ll_j = loglik_j(W[j], X[j], Y[j], theta_j["gamma"], theta_j["kappa_j_1"],
                    theta_j["kappa_j"], theta_j["beta_j"], theta_j["sigma_j"],
                            theta_j["rho_j"], boundary);

    // append to ll
    ll = arma::join_cols(ll, ll_j);
  }

  // element-wise multiplication
  ll_weighted = ll % weights;

  return ll_weighted;
}
