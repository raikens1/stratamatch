require("dplyr")
require("ggplot2") # TODO: remove this dependency
require("ggrepel") # TODO: remove this dependency
require("RColorBrewer") # TODO: remove this dependency
require("Hmisc")
require("optmatch")

#----------------------------------------------------------
### GENERAL HELPER FUNCTIONS
#----------------------------------------------------------

#' @title Produce table of Treated and Control by strata
#' @description Given a dataframe with strata assigned, tally the number of treated and control samples
#' @param a_set data frame with observations as rows, features as columns
#' @param treat string name of treatment column
#' @return Returns a 3 by [numer of strata] dataframe with Treat, Control, Total, Control Proportion, and Potential Issues

make_issue_table <- function(a_set, treat){
  names(a_set)[names(a_set) == treat] <- "treat"
  df <- a_set %>%
    group_by(stratum) %>%
    dplyr::summarize(Treated = sum(treat), Control = sum(1-treat), Total = n()) %>%
    mutate(Control_Proportion = Control/Total)
  
  colnames(df)<- c("Stratum", "Treat", "Control", "Total", "Control_Proportion")
  
  df$Potential_Issues <- apply(df, 1, get_issues)
 
  return(df)
}


#' @title Helper for make_issue_table to return issues string
#' @description Given a row which summarizes the Treat, Control, Total, and 
#' Control_Proportion of a stratum, return a string of potential issues with the stratum
#' @param row a row of the data.frame produced in make_issue_table
#' @return Returns a string of potential issues ("none" if everything is fine)

get_issues <- function(row){
  row <- as.numeric(row[4:5])
  
  # set parameters
  CONTROL_MIN = 0.2
  CONTROL_MAX = 0.8
  SIZE_MIN = 75
  SIZE_MAX = 4000
  
  issues <- c(
    if (row[1] > SIZE_MAX) "Too many samples",
    if (row[1] < SIZE_MIN) "Too few samples",
    if (row[2] > CONTROL_MAX) "Not enough treated samples",
    if (row[2] < CONTROL_MIN) "Not enough control samples"
  )
  
  if(is.null(issues)){
    issues <- c("none")
  }
  
  return(paste(issues, collapse = "; "))
}

#----------------------------------------------------------
### MANUAL STRATIFY
#----------------------------------------------------------

#' @title Manually creates strata for matching
#' @description Creates \code{strata} object, given a set of covariates to stratify on
#' @param data data frame with observations as rows, features as columns
#' @param covariates a vector of the columns to be used as covariates for stratification
#' @param force a boolean. If true, run even if a variable appears continuous.
#' @return Returns a \code{strata} object

manual_stratify <- function(data, treat, covariates, force = FALSE){
  
  result <- structure(list(analysis_set = NULL, treat = treat, covariates = covariates, 
                           call = match.call(), issue_table = NULL,
                           strata_table = NULL),
                      class = c("manual_strata" , "strata"))
  
  n <- dim(data)[1]
  
  # Check that all covariates are discrete
  for (i in 1:length(covariates)){
    warn_if_continuous(data[,covariates[i]], covariates[i], force, n)
  }
  
  # helper function to extract group labels from dplyr
  get_next_integer = function(){
    i = 0
    function(u,v){ i <<- i+1 }
  }
  get_integer = get_next_integer()
  
  # Interact covariates
  grouped_table <- group_by_at(data, covariates) %>% mutate(stratum = get_integer())
  
  result$analysis_set <- grouped_table %>% ungroup()
  
  result$strata_table <- grouped_table %>% dplyr::summarize(stratum = first(stratum),
                                                     size = n())
  
  result$issue_table <- make_issue_table(result$analysis_set, treat)
  
  return(result)
}

#' @title Throws an error if a column is continuous
#' @description checks if there is a large number of unique values in the input column.
#' @param column vector or factor column from a data frame
#' @param name name of the input column
#' @param force, a boolean. If true, warn but do not stop
#' @return Does not return anything

warn_if_continuous <- function(column, name, force, n){
  if (is.factor(column)){
    return() # assume all factors are discrete
  } else {
    values <- length(unique(column))
    if (values > min(c(15, 0.05*n))){
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
#' @param size numeric, desired size of strata
#' @param held_frac numeric between 0 and 1 giving the proportion of samples to be allotted for building the prognostic score
#' @param held_sample (optional) a data.frame of held aside samples for building prognostic score model.
#' @return Returns a \code{strata} object

auto_stratify <- function(data, treat, outcome, covariates = NULL, prog_scores = NULL, size = 2500, held_frac = 0.1, held_sample = NULL){
  
  result <- structure(list(analysis_set = NULL,treat = treat, covariates = covariates, 
                           call = match.call(), issue_table = NULL,
                           outcome = outcome, prog_scores = NULL, prog_model = NULL, model_set = NULL),
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
    } else {
      result$analysis_set <- data
    }
    
  } else {
    # if prog_scores are not specified, build them
    prog_build <- build_prog_model(data, treat, outcome, covariates, held_frac, held_sample)
    prog_model <- prog_build$m
    prog_scores <- tryCatch(predict(prog_model, prog_build$a_set, type = "response"), 
                            error = function(e) {
                              if (e$call == "model.frame.default(Terms, newdata, na.action = na.action, xlev = object$xlevels)"){
                                e$print <- paste("Error applying prognostic model: Some categorical variable value(s) in the analysis set do not appear in the modeling set. Consider stratifying by these variable(s).", 
                                                   "GLM error:", e$print)
                                stop(e)
                              }
                            })
    
    result$prog_model <- prog_model
    result$model_set <- prog_build$m_set
    result$analysis_set <- prog_build$a_set
  }
  
  print("Generating strata assignments based on prognostic score.")
  
  # Create strata from prognostic score quantiles
  n_bins <- ceiling(dim(result$analysis_set)[1]/size)
  #data$stratum <- ntile(prog_scores, n_bins)
  result$analysis_set$stratum <- cut2(prog_scores, g = n_bins)
  
  # package and resturn result
  result$prog_scores = prog_scores
  
  print("Completing strata diagnostics.")
  result$issue_table  = make_issue_table(result$analysis_set, treat)
  
  return(result)
}

#' @title Generates prognostic score model from the data
#' @description When the prog_score is not precalculated, subsamples to create a model set (if necessary), then fits a prognostic score on that set.
#' @param data data frame with observations as rows, features as columns
#' @param treat string giving the name of column designating treatment assignment
#' @param outcome string giving the name of column with outcome information
#' @param covariates a vector of the columns to be used as covariates building prognostic score model
#' @param held_size, an integer giving the desired size of the hold-out sample
#' @param held_sample, (optional) a held aside dataset to be used to fit the prognostic score model
#' @return Returns list of: m, a \code{glm} object prognostic model, m_set, the model set, and a_set, the analysis set (data - model set)
build_prog_model <- function(data, treat, outcome, covariates, held_frac = 0.1, held_sample = NULL){
  formula_str <- paste(outcome, paste(covariates, collapse ="+"), sep = "~")
  model_set <- NULL
  
  # if held_sample is specified use that to build score
  if (!is.null(held_sample)){
    print("Using user-specified set for prognostic score modeling.")
    model_set <- held_sample
    analysis_set <- data
    
  } else {
    print("Constructing a model set via subsampling.")
    # otherwise, construct a model sample
    # Adds an id column and removes it
    data$join_id_57674 <- 1:nrow(data)
    model_set <- data %>% filter_(paste(treat, "==", 0)) %>% sample_frac(held_frac, replace = FALSE)
    analysis_set <- dplyr::anti_join(data, model_set, by = "join_id_57674") %>%
     dplyr::select(-join_id_57674)
    model_set$join_id_57674 <- NULL
  }
  print(paste("Fitting prognostic model:", formula_str))
  model <- glm(formula(formula_str), data = model_set, family = "binomial")
  
  return(list(m = model, m_set = model_set, a_set = analysis_set))
}

#----------------------------------------------------------
### Match
#----------------------------------------------------------

#' @title Match One
#' @description Match one dataset using the optmatch package
#' @param dat a data.frame with observations as rows, outcome column masked
#' @return a data.frame like dat with pair assignments?
match_one <- function(dat, propensity_model, k = 1){
  dat$match_id <- pairmatch(propensity_model, data = dat, controls = k)
  return(dat)
}

#' @title Big Match v2
#' @description Match within strata in parallel by calling match_one
#' @param strat a strata object
#' @return a data.frame like dat with pair assignments?
big_match_v2 <- function(strat, propensity_formula = NULL) {
  if (is.null(propensity_formula)){
    propensity_formula <- formula(paste(c(strat$treat, "~ . -", strat$outcome, "- stratum"), collapse = ""))
  }
  # build propensity model
  propensity_model <- glm(propensity_formula, data = strat$analysis_set, family = binomial())
  
  # do match_one
}

#' @title Big Match
#' @description Match within strata in parallel using optmatch
#' @param strat a strata object
#' @param propensity_formula (optional) formula for propensity score
#' @param k numeric, the number of control individuals to be matched to each treated individual
#' @return a named factor with matching assignments
big_match <- function(strat, propensity_formula = NULL, k = 1){
  if (is.null(propensity_formula)){
    # match on all variables, stratified by stratum
    propensity_formula <- formula(paste(strat$treat, "~ . -", strat$outcome, "- stratum", "+ strata(stratum)"))
  } else {
    # append phrase to stratify by stratum
    orig_form <- Reduce(paste, deparse(propensity_formula))
    propensity_formula <- formula(paste(orig_form, "+ strata(stratum)"))
  }
  
  print(propensity_formula)
  # build propensity model
  propensity_model <- glm(propensity_formula, data = strat$analysis_set, family = binomial())
  
  return(pairmatch(propensity_model, data = strat$analysis_set, controls = k))
}

#' @title Big Match (slow)
#' @description For performance testing purposes, match using optmatch but do not stratify
#' @param strat a strata object
#' @param propensity_formula (optional) formula for propensity score
#' @param k numeric, the number of control individuals to be matched to each treated individual
#' @return a named factor with matching assignments
big_match_slow <- function(strat, propensity_formula = NULL, k = 1){
  if (is.null(propensity_formula)){
    # match on all variables, stratified by stratum
    propensity_formula <- formula(paste(strat$treat, "~ . -", strat$outcome, "- stratum"))
  } else {
    # do not modify original formula to append "+strata(stratum)"
  }
  
  print(propensity_formula)
  # build propensity model
  propensity_model <- glm(propensity_formula, data = strat$analysis_set, family = binomial())
  
  return(pairmatch(propensity_model, data = strat$analysis_set, controls = k))
}
