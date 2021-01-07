#----------------------------------------------------------
### CONTAINS:
# Package documentation
#----------------------------------------------------------

#' stratamatch: stratify and match large data sets
#'
#' This package employs a pilot matching design to automatically stratify and
#' match large datasets.  The \code{\link{manual_stratify}} function allows
#' users to manually stratify a dataset based on categorical variables of
#' interest, while the \code{\link{auto_stratify}} function does automatically
#' by allocating a held-aside (pilot) data set, fitting a prognostic score (see
#' Hansen (2008) <doi:10.1093/biomet/asn004>) on the pilot set, and stratifying
#' the data set based on prognostic score quantiles.  The
#' \code{\link{strata_match}} function then does optimal matching of the data
#' set within strata.
#'
#' @seealso \enumerate{ \item \url{https://github.com/raikens1/stratamatch} }
#'
#' @docType package
#' @name stratamatch
NULL
