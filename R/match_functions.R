#----------------------------------------------------------
### CONTAINS: 
# Matching methods to be called on strata objects, and their helpers
#----------------------------------------------------------

#' Strata Match
#'
#' Match within strata in series using optmatch.
#'
#' @param object a strata object
#' @param propensity (optional) formula for propensity score.  If left
#'   blank, all columns of the dataset will be used as covariates (except
#'   outcome, treatment and stratum)
#' @param k numeric, the number of control individuals to be matched to each
#'   treated individual
#' @return a named factor with matching assignments
#' @export
#' @examples 
#' # make a sample data set
#' set.seed(1)
#' dat <- make_sample_data(n = 75)
#'   
#' # stratify with auto_stratify
#' a.strat <- auto_stratify(dat, "treat", outcome ~ X2, size = 25)
#'   
#' # 1:1 match based on propensity formula: treat ~ X1 + X2
#' strata_match(a.strat, propensity = treat ~ X1 + X2, k = 1)
strata_match <- function(object, propensity = NULL, k = 1){

  check_inputs_matcher(object, propensity, k)

  message("This function makes essential use of the optmatch package, which has an academic license.")
  message("For more information, run optmatch::relaxinfo()")

  if (is.null(propensity)){
    # match on all variables, stratified by stratum
    propensity <- formula(paste(object$treat, "~ . -", object$outcome,
                                        "- stratum",
                                        "+ strata(stratum)"))
  } else {
    # append phrase to stratify by stratum
    orig_form <- Reduce(paste, deparse(propensity))
    propensity <- formula(paste(orig_form,
                                        "+ strata(stratum)"))
  }
  
  message(paste("Fitting propensity model:",
                Reduce(paste, deparse(propensity))))
  
  # build propensity model
  propensity_model <- glm(propensity,
                          data = object$analysis_set,
                          family = binomial())

  return(optmatch::pairmatch(propensity_model,
                             data = object$analysis_set,
                             controls = k))
}

#' Match without Stratification
#'
#' Not meant to be called externally.  Match a data set without stratifying.
#' Used to compare performance with and without stratification.
#'
#' @param object a strata object
#' @param propensity (optional) formula for propensity score
#' @param k numeric, the number of control individuals to be matched to each
#'   treated individual
#' @return a named factor with matching assignments
strata_match_nstrat <- function(object, propensity = NULL, k = 1){

  check_inputs_matcher(object, propensity, k)

  if (is.null(propensity)){
    # match on all variables, stratified by stratum
    propensity <- formula(paste(object$treat, "~ . -", object$outcome,
                                        "- stratum"))
  } else {
    # do not modify original formula to append "+ strata(stratum)"
  }

  # build propensity model
  propensity_model <- glm(propensity,
                          data = object$analysis_set,
                          family = binomial())

  return(optmatch::pairmatch(propensity_model,
                             data = object$analysis_set,
                             controls = k))
}

#' Check inputs to any matching function
#'
#' @inheritParams strata_match
#'
#' @return nothing
check_inputs_matcher <- function(object, propensity, k){
  if (!is.strata(object)) {
    stop("strat must be a strata object")
  }
  if (!is.null(propensity)) {
    if (!inherits(propensity, "formula")) {
      stop("propensity must be a formula")
    }
    check_prop_formula(propensity, object$analysis_set, object$treat)
  }
  if (is.na(suppressWarnings(as.integer(k)))) stop("k must be an integer")
  if (k < 1) stop("k must be 1 or greater")
}
