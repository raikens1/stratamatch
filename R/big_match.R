require("binr")


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
  
  result <- list(data = NULL, prog_scores = NULL, prog_model = NULL)
  
  # check inputs
  
  if (is.null(covariates) && is.null(prog_scores)){
    stop("At least one of covariates and prog_scores should be specified.")
  }
  
  if (!is.null(covariates) && !is.null(prog_scores)){
    warning("covariates and prog_scores are both specified. Using prog_scores; ignoring covariates.") 
  }
  
  if (!is.null(prog_scores)){
    if (length(prog_scores) != dim(data)[1]){
      stop("prog_scores must be the same length as the data")
    } 
  } else {
    # if prog_scores are not specified, build them
    
    prog_model <- build_prog_model(data, treat, outcome, covariates)
    prog_scores <- predict(prog_model, lalonde, type = "response")
    
    result$prog_model <- prog_model
    
  }
  
  # Create strata from prognostic score quantiles
  n_bins <- ceiling(dim(data)[1]/size)
  data$stratum <- ntile(prog_scores, n_bins)
  
  # package and resturn result
  result$data = data
  result$prog_scores = prog_scores
  class(result) <- "strata"
  
  return(result)
}

#' @title Generates prognostic score model from the data
#' @description When the prog_score is not precalculated, fits a prognostic score model to the data according to specifications.  Currently just fits a logistic model on controls.
#' @param data data frame with observations as rows, features as columns
#' @param treat string giving the name of column designating treatment assignment
#' @param outcome string giving the name of column with outcome information
#' @param covariates a vector of the columns to be used as covariates building prognostic score model
#' @return Returns a \code{strata} object

build_prog_model <- function(data, treat, outcome, covariates){
  formula_str <- paste(outcome, paste(covariates, collapse ="+"), sep = "~")
  
  # fit model on controls only
  data0 <- subset(data, treat == 0)
  
  model <- glm(formula(formula_str), data = data0, family = "binomial")
  
  return(model)
}