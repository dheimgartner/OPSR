## make sure these packages are installable
# install.packages("Rcpp")
# install.packages("RcppArmadillo")

cpp <- '
void can_compile() {
  std::cout << "can compile" << std::endl;
}
'
Rcpp::cppFunction(cpp)
can_compile()

cpp <- '
arma::mat can_use_armadillo() {
  return arma::mat(2, 2);
}
'
Rcpp::cppFunction(cpp, depends = "RcppArmadillo")
can_use_armadillo()

## if this runs through without any issues then the problem has something to do
## with OPSR. Otherwise it's simply Rcpp/RcppArmadillo that is "wrongly" set up
## or the associated build tools (compiler).
## https://cran.r-project.org/web/packages/Rcpp/vignettes/Rcpp-FAQ.pdf
## in particular section 1.3; FAQ 2.10 and FAQ 2.16 might help
