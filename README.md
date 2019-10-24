# stratamatch
<!-- badges: start -->
[![CRAN
status](https://www.r-pkg.org/badges/version/stratamatch)](https://cran.r-project.org/package=stratamatch)
<!-- badges: end -->

**Creator:** Rachael Caelie "Rocky" Aikens

**Authors:** Rachael C. Aikens, Joseph Rigdon, Justin Lee, Jonathan H. Chen, Michael Baiocchi

## Quick Start

Stratamatch requires R version 3.4.0 or higher.  In order to install this package, you may need to [update R](https://www.linkedin.com/pulse/3-methods-update-r-rstudio-windows-mac-woratana-ngarmtrakulchol)

To install from CRAN:

```r
install.packages("stratamatch"
```

After installation, to load the package and attach it to the namespace, run:

```r
library(stratamatch)
```

For a tutorial of the basic usage, view the stratamatch vignette.  To access this from your R console, run:

```
browseVignettes("stratamatch")
```

## Installation from Source

You can also install the development version of stratamatch from github using devtools:

```r
install.packages("devtools")
devtools::install_github("raikens1/stratamatch")
```

The optmatch package must be installed to successfully build the vignettes.  To make sure that vignettes are built when you install, make sure optmatch is installed, then run the following instead:

```r
install.packages("devtools")
devtools::install_github("raikens1/stratamatch", build_vignettes = T)
```

Then, to access the vignette, run:

```r
browseVignettes("stratamatch")
```

## Objective
**Goal:** The purpose of stratamatch is to provide user-friendly software and tutorials in R to support scientists to perform matching for "big data" observational studies. 

While matching is a mainstay in causal inference for observational studies, the speed of many matching algorithms scales poorly with the number of observations.  As the sizes of observational studies grow larger and larger with the advance of "big data," the computational task of matching becomes more and more cumbersome.

Stratamatch uses a pilot design (see [Aikens et al.](https://arxiv.org/abs/1908.09077))to estimate a quantity called the *prognostic score* (see Hansen, 2008 “The Prognostic Analougue of the Propensity Score”), defined here as an individual’s expected outcome under the control assignment, based on their baseline covariates. Balancing observational data sets based on the prognostic score will reduce heterogeneity between matched individuals, decreasing variance and diminishing the sensitivity of the study results to unobserved confounding (See Aikens et al.; Antonelli et al. 2017; and Leacy and Stuart 2014). Moreover, since the prognostic score is often continuous, strata can be easily determined using prognostic score quantiles to select evenly sized “bins” for the data. This cicumvents common problems with stratification based on expert knowledge, since that process often generates strata which are too large, too small, or too poorly balanced between treatment and control observations (although stratamatch does contain a function, `manual_stratify`, to facilitate this kind of stratification as well).

The `auto_stratify` function carries out the prognostic score stratification pilot design described above. Although there are many additional options available when running this function, the most basic procedure does the following:

1. Partition the data set into a pilot data set and an analysis data set

2. Fit a model for the prognostic score from the observations in the pilot set

3. Estimate prognostic scores for the analysis set using the prognostic model

4. Stratify the analysis set based on prognostic score quantiles.

Once the data set has been stratified (either manually or automatically), the treatment and control individuals can then be matched. At this point, the reader may choose to match the data set within each stratum using the strata_match function, or they may select and implement their own matching scheme.

## Feedback
We welcome your feedback! Post bug reports, questions, or suggestions as github issues.  Or (less preferred) email the maintainer at `rockyaikens at gmail`
