library(Rcpp)

## set compiler flags to enable openmp during compilation
Sys.setenv("PKG_CXXFLAGS" = "-fopenmp")
Sys.setenv("PKG_LIBS" = "-fopenmp")
## typically included in Makevars?

cpp <- '
#include <iostream>

#ifdef _OPENMP
#include <omp.h>
#endif

int check_omp() {
  #ifdef _OPENMP
  std::cout << "OpenMP is enabled!" << std::endl;
  std::cout << "Number of threads: " << omp_get_max_threads() << std::endl;
  #else
  std::cout << "OpenMP is not enabled." << std::endl;
  #endif
  return 0;
}
'

?cppFunction
Rcpp::cppFunction(cpp)
check_omp()

## if arma is included then openmp is enabled automatically?
## at least if we set depends = "RcppArmadillo" above then the env vars are not needed...
library(RcppArmadillo)
RcppArmadillo::armadillo_get_number_of_omp_threads()
