
#' @title Manually creates strata for matching
#' @description Creates \code{strata} object, given a set of covariates to stratify on
#' @param data data frame with observations as rows, features as columns
#' @param treat string giving the name of column designating treatment assignment
#' @param outcome string giving the name of column with outcome information
#' @param covariates a vector of the columns to be used as covariates for stratification
#' @return Returns a \code{strata} object

manual_stratify <- function(data, treat, outcome, covariates){
  
  # Check that all covariates are discrete
  
  # Interact covariates, generating names for each strata
  
  # If necessary, eliminate empty strata
  
  # Add strata labels and strata names; eliminate interaction columns
  
  # return strata object
  return(0)
}

#' @title Automatically creates strata for matching
#' @description Creates \code{strata} object, given covariates for a linear model, or pre-calculated prognostic scores
#' @param data data frame with observations as rows, features as columns
#' @param treat string giving the name of column designating treatment assignment
#' @param outcome string giving the name of column with outcome information
#' @param covariates a vector of the columns to be used as covariates building prognostic score model
#' @return Returns a \code{strata} object

auto_stratify <- function(data, treat, outcome, covariates = NULL, prog_scores = NULL, size = 2000){
  
  # check that exactly one of covariates or prog_scores is non-NULL
  
  # if prog_scores precalculated: check that they are valid
  
  # else: build prognostic scores with a linear model
  
  # Create strata from prognostic score quantiles
  
  # If prognostic score is not continuous, handle that.
  
  # return strata object
  
  return(0)
}