#include <Rcpp.h>
#include "utils.h"

using namespace Rcpp;

// [[Rcpp::export]]
List opsr_prepare_coefs(NumericVector& theta, int& nReg) {
  CharacterVector theta_names = theta.names();
  NumericVector gamma, kappa, sigma, rho;
  std::vector<NumericVector> beta(nReg);  // a vector of beta vectors for each region

  kappa.push_back(R_NegInf);  // first element of kappa is -Inf

  for (int i = 0; i < theta.size(); i++) {
    std::string name = as<std::string>(theta_names[i]);  // for substr method

    if (name.substr(0, 2) == "s_") {
      gamma.push_back(theta[i]);
    } else if (name.substr(0, 5) == "kappa") {
      kappa.push_back(theta[i]);
    } else if (name.substr(0, 5) == "sigma") {
      sigma.push_back(theta[i]);
    } else if (name.substr(0, 3) == "rho") {
      rho.push_back(theta[i]);
    } else if (name[0] == 'o' && std::isdigit(name[1]) && name[2] == '_') {
      // extract beta elements, based on the region (e.g., o1_, o2_, etc.)
      int region = name[1] - '0' - 1;  // convert '1', '2', etc. to 0-based index
      beta[region].push_back(theta[i]);
    }
  }

  kappa.push_back(R_PosInf);  // last element of kappa is Inf

  // construct the list of lists for each ordinal category
  List theta_(nReg);
  for (int i = 0; i < nReg; i++) {
    double kappa_j_1 = kappa[i];
    double kappa_j = kappa[i + 1];
    double sigma_j = sigma[i];
    double rho_j = rho[i];

    theta_[i] = List::create(
      Named("gamma") = gamma,
      Named("kappa_j_1") = kappa_j_1,
      Named("kappa_j") = kappa_j,
      Named("beta_j") = beta[i],
                            Named("sigma_j") = sigma_j,
                            Named("rho_j") = rho_j
    );
  }
  return theta_;
}

// dot product
double dot(NumericVector x, NumericVector y) {
  int n_elem = x.size();
  double dot = 0;
  for (int i = 0; i < n_elem; i++) {
    dot += x(i) * y(i);
  }
  return dot;
}

// wrapper around dnorm which expects and returns NumericVector
double dnorm_double(double x) {
  NumericVector y = NumericVector::create(x);
  NumericVector res = dnorm(y);
  return res[0];
}

double pnorm_double(double x) {
  NumericVector y = NumericVector::create(x);
  NumericVector res = pnorm(y);
  return res[0];
}