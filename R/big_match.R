require("dplyr")
require("ggplot2")
require("ggrepel")
require("RColorBrewer")

#----------------------------------------------------------
### MANUAL STRATIFY
#----------------------------------------------------------

#' @title Manually creates strata for matching
#' @description Creates \code{strata} object, given a set of covariates to stratify on
#' @param data data frame with observations as rows, features as columns
#' @param covariates a vector of the columns to be used as covariates for stratification
#' @param force a boolean. If true, run even if a variable appears continuous.
#' @return Returns a \code{strata} object

manual_stratify <- function(data, covariates, force = FALSE){
  
  result <- structure(list(data = NULL, strata_table = NULL),
                      class = c("manual_strata" , "strata"))
  
  # Check that all covariates are discrete
  for (i in 1:length(covariates)){
    warn_if_continuous(data[,covariates[i]], covariates[i], force)
  }
  
  # helper function to extract group labels from dplyr
  get_next_integer = function(){
    i = 0
    function(u,v){ i <<- i+1 }
  }
  get_integer = get_next_integer()
  
  # Interact covariates
  grouped_table <- group_by_at(data, covariates) %>% mutate(stratum = get_integer())
  
  result$data <- grouped_table %>% ungroup()
  
  result$strata_table <- grouped_table %>% summarize(stratum = first(stratum),
                                                     size = n())
  return(result)
}

#' @title Throws an error if a column is continuous
#' @description checks if there is a large number of unique values in the input column.
#' @param column vector or factor column from a data frame
#' @param name name of the input column
#' @param force, a boolean. If true, warn but do not stop
#' @return Does not return anything

warn_if_continuous <- function(column, name, force){
  if (is.factor(column)){
    return() # assume all factors are discrete
  } else {
    values <- length(unique(column))
    if (values > min(c(15, 0.05*length(column)))){
      if ( force == FALSE ){
        stop(paste("There are ", values, " distinct values for ", name,". Is it continuous?", sep = ""))
      } else {
        warning(paste("There are ", values, " distinct values for ", name,". Is it continuous?", sep = ""))
      }
    }
    return()
  }
}

#----------------------------------------------------------
### AUTO STRATIFY
#----------------------------------------------------------

#' @title Automatically creates strata for matching
#' @description Creates \code{strata} object, given covariates for a linear model, or pre-calculated prognostic scores
#' @param data data frame with observations as rows, features as columns
#' @param treat string giving the name of column designating treatment assignment
#' @param outcome string giving the name of column with outcome information
#' @param covariates a vector of the columns to be used as covariates building prognostic score model
#' @return Returns a \code{strata} object

auto_stratify <- function(data, treat, outcome, covariates = NULL, prog_scores = NULL, size = 2000, held_sample = "controls", held_size = NULL){
  
  result <- structure(list(data = NULL, prog_scores = NULL, prog_model = NULL,
                           treat = treat, outcome = outcome, covariates = covariates),
                      class = c("auto_strata", "strata"))
  
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
    
    prog_model <- build_prog_model(data, treat, outcome, covariates, held_sample, held_size)
    prog_scores <- predict(prog_model, lalonde, type = "response")
    
    result$prog_model <- prog_model
    
  }
  
  # Create strata from prognostic score quantiles
  n_bins <- ceiling(dim(data)[1]/size)
  data$stratum <- ntile(prog_scores, n_bins)
  
  # package and resturn result
  result$data = data
  result$prog_scores = prog_scores
  
  return(result)
}

#' @title Generates prognostic score model from the data
#' @description When the prog_score is not precalculated, fits a prognostic score model to the data according to specifications.  Currently just fits a logistic model on controls.
#' @param data data frame with observations as rows, features as columns
#' @param treat string giving the name of column designating treatment assignment
#' @param outcome string giving the name of column with outcome information
#' @param covariates a vector of the columns to be used as covariates building prognostic score model
#' @param held_sample, a string defining which sample to use to build the prognostic model.
#' @param held_size, an integer giving the desired size of the hold-out sample
#' @return Returns a \code{glm} object
# TODO (raikens): implement other options for "held_sample"
build_prog_model <- function(data, treat, outcome, covariates, held_sample, held_size){
  formula_str <- paste(outcome, paste(covariates, collapse ="+"), sep = "~")
  
  if (held_sample == "controls"){
    # fit model on controls only
    data0 <- subset(data, treat == 0)
  }
  else {
    stop("Not a valid option for held_sample.")
  }
  model <- glm(formula(formula_str), data = data0, family = "binomial")
  
  return(model)
}