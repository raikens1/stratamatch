# BigMatch
<!-- badges: start -->
[![CRAN
status](https://www.r-pkg.org/badges/version/BigMatch)](https://cran.r-project.org/package=BigMatch)
<!-- badges: end -->
**Creators:** Rachael Caelie "Rocky" Aikens and Michael Baiocchi

Development version avaiable for download with `devtools`

```r
install.packages("devtools")
devtools::install_github("raikens1/BigMatch")
```

## Objective
**Goal:** The purpose of BigMatch is to provide user-friendly software and tutorials in R to support scientists to perform matching for "big data" observational studies. 

While matching is a mainstay in causal inference for observational studies, the speed of many matching algorithms scale poorly with the number of observations.  As the sizes of observational studies grow larger and larger with the advance of "big data," the computational task of matching becomes more and more cumbersome.  Big_match will break down the matching task by strata of manageable size, and parallelize the matching task across strata.  To do this, we will need to support:

(1) Stratification by user-defined variables or prognostic score quantiles

(2) Visual diagnostics for assessing the useability of strata

(3) Parallelization of matching across strata

(4) Production of a complete, matched dataset across strata.

## Feedback
 Big_match is a work in progress.  If you have questions or suggestions, please send them to Rocky at raikens@stanford.edu.
