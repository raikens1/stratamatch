#----------------------------------------------------------
### CONTAINS: 
# Package documentation
#----------------------------------------------------------

#' BigMatch: stratify and match large data sets
#'
#' This package employs a "Pilot Matching" design to automatically stratify and
#' match large datasets.  The \code{\link{manual_stratify}} function allows users to
#' manually stratify a dataset based on categorical variables of interest, while
#' the \code{\link{auto_stratify}} function does automatically by allocating a held-aside
#' "pilot" data set, fitting a prognostic score (see Hansen (2008)) on the pilot
#' set, and stratifying the data set based on prognostic score quantiles.  The
#' \code{\link{big_match}} function then does optimal matching of the data set in parallel
#' within strata.
#' 
#'@seealso
#'\enumerate{
#'\item \url{https://github.com/raikens1/BigMatch}
#'}
#'
#'@docType package
#'@name BigMatch
NULL
