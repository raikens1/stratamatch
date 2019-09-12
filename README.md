# BigMatch
<!-- badges: start -->
[![CRAN
status](https://www.r-pkg.org/badges/version/BigMatch)](https://cran.r-project.org/package=BigMatch)
<!-- badges: end -->
**Creators:** Rachael Caelie "Rocky" Aikens and Michael Baiocchi

Development version 0.0.9000 is avaiable for download with `devtools`

```r
install.packages("devtools")
devtools::install_github("raikens1/BigMatch")
```

## Objective
**Goal:** The purpose of BigMatch is to provide user-friendly software and tutorials in R to support scientists to perform matching for "big data" observational studies. 

While matching is a mainstay in causal inference for observational studies, the speed of many matching algorithms scales poorly with the number of observations.  As the sizes of observational studies grow larger and larger with the advance of "big data," the computational task of matching becomes more and more cumbersome.

BigMatch uses a pilot design (see [Aikens et al.](https://arxiv.org/abs/1908.09077))to estimate a quantity called the *prognostic score* (see Hansen, 2008 “The Prognostic Analougue of the Propensity Score”), defined here as an individual’s expected outcome under the control assignment, based on their baseline covariates. Balancing observational data sets based on the prognostic score will reduce heterogeneity between matched individuals, decreasing variance and diminishing the sensitivity of the study results to unobserved confounding (See Aikens et al.; Antonelli et al. 2017; and Leacy and Stuart 2014). Moreover, since the prognostic score is often continuous, strata can be easily determined using prognostic score quantiles to select evenly sized “bins” for the data. This cicumvents common problems with stratification based on expert knowledge, since that process often generates strata which are too large, too small, or too poorly balanced between treatment and control observations (although BigMatch does contain a function, manual_stratify, to facilitate this kind of stratification as well).

## Feedback
 Big_match is a work in progress.  If you have questions or suggestions, please send them to Rocky at raikens@stanford.edu.
