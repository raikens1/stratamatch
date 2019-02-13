require("dplyr")
require("ggplot2") # TODO: remove this dependency
require("ggrepel") # TODO: remove this dependency
require("RColorBrewer") # TODO: remove this dependency
require("Hmisc")
require("optmatch")
require("foreach")
require("doParallel")
# install.packages("devtools")
# devtools::install_github("hadley/multidplyr") # TODO since multiplyr is not a real R package yet, this is annoying
library(multidplyr)


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
  
  result <- structure(list(analysis_set = NULL, treat = treat, 
                           call = match.call(), issue_table = NULL,
                           covariates = covariates, strata_table = NULL),
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
#' @description Creates \code{strata} object, given formula for a prognostic model, or pre-calculated prognostic scores
#' @param data data frame with observations as rows, features as columns
#' @param treat string giving the name of column designating treatment assignment
#' @param outcome string giving the name of column with outcome information
#' @param prog_formula formula for building prognostic score model
#' @param size numeric, desired size of strata
#' @param held_frac numeric between 0 and 1 giving the proportion of samples to be allotted for building the prognostic score
#' @param held_sample (optional) a data.frame of held aside samples for building prognostic score model.
#' @return Returns a \code{strata} object
auto_stratify <- function(data, treat, outcome, prog_formula = NULL, prog_scores = NULL, size = 2500, held_frac = 0.1, held_sample = NULL){
  
  result <- structure(list(analysis_set = NULL,treat = treat,  
                           call = match.call(), issue_table = NULL, strata_table = NULL,
                           outcome = outcome, prog_scores = NULL, prog_model = NULL, prognostic_set = NULL),
                      class = c("auto_strata", "strata"))
  
  # check inputs
  if (is.null(prog_formula) && is.null(prog_scores)){
    stop("At least one of prog_formula and prog_scores should be specified.")
  }
  
  if (!is.null(prog_formula) && !is.null(prog_scores)){
    warning("prog_formula and prog_scores are both specified. Using prog_scores; ignoring formula.") 
  }
  
  if (!is.null(prog_scores)){
    if (length(prog_scores) != dim(data)[1]){
      stop("prog_scores must be the same length as the data")
    } else {
      result$analysis_set <- data
    }
    
  } else {
    # if prog_scores are not specified, build them
    prog_build <- build_prog_model(data, treat, outcome, prog_formula, held_frac, held_sample)
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
    result$prognostic_set <- prog_build$m_set
    result$analysis_set <- prog_build$a_set
  }
  
  print("Generating strata assignments based on prognostic score.")
  
  # Create strata from prognostic score quantiles
  n_bins <- ceiling(dim(result$analysis_set)[1]/size)
  qcut <- cut2(prog_scores, g = n_bins)
  result$strata_table <- data.frame(qcut) %>% 
    mutate(stratum = as.numeric(qcut), quantile_bin = qcut) %>%
    group_by(quantile_bin) %>%
    dplyr::summarise(size = n(), stratum = first(stratum)) %>%
    arrange(stratum) %>%
    select(stratum, quantile_bin, size)
  result$analysis_set$stratum <- as.numeric(qcut)
  
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
#' @param prog_formula formula for building prognostic score model
#' @param held_size, an integer giving the desired size of the hold-out sample
#' @param held_sample, (optional) a held aside dataset to be used to fit the prognostic score model
#' @return Returns list of: m, a \code{glm} object prognostic model, m_set, the model set, and a_set, the analysis set (data - model set)
build_prog_model <- function(data, treat, outcome, prog_formula, held_frac = 0.1, held_sample = NULL){
  prognostic_set <- NULL
  
  # if held_sample is specified use that to build score
  if (!is.null(held_sample)){
    print("Using user-specified set for prognostic score modeling.")
    prognostic_set <- held_sample
    analysis_set <- data
    
  } else {
    print("Constructing a model set via subsampling.")
    # otherwise, construct a model sample
    # Adds an id column and removes it
    data$join_id_57674 <- 1:nrow(data)
    prognostic_set <- data %>% filter_(paste(treat, "==", 0)) %>% sample_frac(held_frac, replace = FALSE)
    analysis_set <- dplyr::anti_join(data, prognostic_set, by = "join_id_57674") %>%
     dplyr::select(-join_id_57674)
    prognostic_set$join_id_57674 <- NULL
  }
  print(paste("Fitting prognostic model:",Reduce(paste, deparse(prog_formula))))
  model <- glm(prog_formula, data = prognostic_set, family = "binomial")
  
  return(list(m = model, m_set = prognostic_set, a_set = analysis_set))
}

#----------------------------------------------------------
### Match
#----------------------------------------------------------

#' @title Match One
#' @description Match one dataset using the optmatch package
#' @param dat a data.frame with observations as rows, outcome column masked
#' @return a data.frame like dat with pair assignments column
match_one <- function(dat, propensity_model, treat, k = 1){
  dist_matrix <- make_distance_matrix(dat, propensity_model = propensity_model, treat = treat)
  dat$match_id <- pairmatch(dist_matrix, data = dat, controls = k)
  return(dat)
}

#' @title Make distance matrix
#' @description Makes the distance matrix to be passed to pairmatch. Similar to match_on.glm in optmatch
#' except that the model need not have been fit on the data we are matching
#' @param dat a data.frame of observations to be matched
#' @param propensity_model a glm object modeling propensity scores
#' @return a matrix of distances to be passed to pairmatch()
make_distance_matrix <- function(dat, propensity_model, treat){
  names(dat)[names(dat) == treat] <- "treat"
  z <- dat$treat
  print(length(z))
  lp <- predict(propensity_model, dat)
  pooled.sd <- sqrt(((sum(!z) - 1) * mad(lp[!z])^2 +
                           (sum(!!z) - 1) * mad(lp[!!z])^2) / (length(lp) - 2))
  
  return(match_on(x = lp/pooled.sd, z = z, rescale = F))
}

#' @title Big Match dopar
#' @description Match within strata in parallel by calling match_one with dopar.
#' Doesn't work right now 
#' @param strat a strata object
#' @return a data.frame like dat with pair assignments?
big_match_dopar <- function(strat, propensity_formula = NULL) {
  if (is.null(propensity_formula)){
    propensity_formula <- formula(paste(c(strat$treat, "~ . -", strat$outcome, "- stratum"), collapse = ""))
  }
  # build propensity model
  propensity_model <- glm(propensity_formula, data = strat$analysis_set, family = binomial())
  
  # set up cluster for dopar
  numCores <- detectCores() 
  registerDoParallel(numCores)
  
  # just use do for now so we can debug
  foreach(i = as.character(unique(a.strat1$analysis_set$stratum))) %do% {
    filter(strat$analysis_set, stratum == i) %>% 
      match_one(., propensity_model = propensity_model, treat = strat$treat)
    
  }
  stopImplicitCluster()
}


#' @title Big Match v2
#' @description Match within strata in parallel by calling match_one
#' @param strat a strata object
#' @param propensity_formula the formula for the propensity score
#' @return a data.frame like dat with pair assignments?
big_match_multidplyr <- function(strat, propensity_formula = NULL) {
  if (is.null(propensity_formula)){
    propensity_formula <- formula(paste(c(strat$treat, "~ . -", strat$outcome, "- stratum"), collapse = ""))
  }
  # build propensity model
  propensity_model <- glm(propensity_formula, data = strat$analysis_set, family = binomial())
  
  # set up multidplyr cluster
  numCores <- detectCores() 
  treat <- strat$treat
  cluster <- create_cluster(numCores)
  cluster_assign_value(cluster, 'propensity_model', propensity_model)
  cluster_assign_value(cluster, 'match_one', match_one)
  cluster_assign_value(cluster, 'make_distance_matrix', make_distance_matrix)
  cluster_assign_value(cluster, 'match_on', match_on)
  cluster_assign_value(cluster, 'pairmatch', pairmatch)
  cluster_assign_value(cluster, 'treat', treat)

  # match in parallel
  result <- strat$analysis_set %>% 
    partition(stratum, cluster = cluster) %>% 
    do(match_one(., propensity_model = propensity_model, treat = treat)) %>% 
    collect()
  
  return(result)
}

#' @title Big Match
#' @description Match within strata in series using optmatch
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

#' @title Big Match (not stratified)
#' @description For performance testing purposes, match using optmatch but do not stratify
#' @param strat a strata object
#' @param propensity_formula (optional) formula for propensity score
#' @param k numeric, the number of control individuals to be matched to each treated individual
#' @return a named factor with matching assignments
big_match_nstrat <- function(strat, propensity_formula = NULL, k = 1){
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
