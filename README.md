
<!-- README.md is generated from README.Rmd. Please edit that file -->

# OPSR

<!-- badges: start -->
<!-- badges: end -->

The goal of OPSR is to …

## Installation

You can install the development version of OPSR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("dheimgartner/OPSR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(OPSR)
#> 
#> Please cite the 'OPSR' package as:
#> Heimgartner, D. and X. Wang (2024) OPSR: A package for estimating ordinal probit switching regression models in R. tbc.
#> 
#> Wang, X. and P. L. Mokhtarian (2024) Examining the treatment effect of teleworking on vehicle-miles driven: Applying an ordered probit selection model and incorporating the role of travel stress, Transportation Research Part A, 186, 104072, doi:10.1016/j.tra.2024.104072.
#> 
#> If you have questions, suggestions, or comments regarding the 'OPSR' package, please open an issue on https://github.com/dheimgartner/OPSR
#> 
#> To see these entries in BibTeX format, use 'citation('OPSR')'
```

``` r
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
