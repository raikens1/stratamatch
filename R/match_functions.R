#----------------------------------------------------------
### CONTAINS: 
# Matching methods to be called on strata objects, and their helpers
#----------------------------------------------------------

#' Big Match
#'
#' Match within strata in series using optmatch.  Probably needs to be renamed.
#'
#' @param strat a strata object
#' @param propensity_formula (optional) formula for propensity score
#' @param k numeric, the number of control individuals to be matched to each
#'   treated individual
#' @return a named factor with matching assignments
#' @export
big_match <- function(strat, propensity_formula = NULL, k = 1){

  check_inputs_matcher(strat, propensity_formula, k)

  message("This function makes essential use of the optmatch package, which has an academic license.")
  message("For more information, run optmatch::relaxinfo()")

  if (is.null(propensity_formula)){
    # match on all variables, stratified by stratum
    propensity_formula <- formula(paste(strat$treat, "~ . -", strat$outcome,
                                        "- stratum",
                                        "+ strata(stratum)"))
  } else {
    # append phrase to stratify by stratum
    orig_form <- Reduce(paste, deparse(propensity_formula))
    propensity_formula <- formula(paste(orig_form,
                                        "+ strata(stratum)"))
  }
  
  # build propensity model
  propensity_model <- glm(propensity_formula,
                          data = strat$analysis_set,
                          family = binomial())

  return(optmatch::pairmatch(propensity_model,
                             data = strat$analysis_set,
                             controls = k))
}

#' Big Match - Not Stratified
#'
#' Not meant to be called externally, but exported currently for convenience.
#' This function is for performance testing purposes, so that we can compare the
#' speed of matching within strata to matching the entire dataset without
#' stratification.
#'
#' @param strat a strata object
#' @param propensity_formula (optional) formula for propensity score
#' @param k numeric, the number of control individuals to be matched to each
#'   treated individual
#' @return a named factor with matching assignments
#' @export
big_match_nstrat <- function(strat, propensity_formula = NULL, k = 1){

  check_inputs_matcher(strat, propensity_formula, k)

  if (is.null(propensity_formula)){
    # match on all variables, stratified by stratum
    propensity_formula <- formula(paste(strat$treat, "~ . -", strat$outcome,
                                        "- stratum"))
  } else {
    # do not modify original formula to append "+ strata(stratum)"
  }

  # build propensity model
  propensity_model <- glm(propensity_formula,
                          data = strat$analysis_set,
                          family = binomial())

  return(optmatch::pairmatch(propensity_model,
                             data = strat$analysis_set,
                             controls = k))
}

#' Check inputs to any matching function
#'
#' @inheritParams big_match
#'
#' @return nothing
check_inputs_matcher <- function(strat, propensity_formula, k){
  if (!is.strata(strat)) {
    stop("strat must be a strata object")
  }
  if (!is.null(propensity_formula)) {
    if (!inherits(propensity_formula, "formula")) {
      stop("propensity_formula must be a formula")
    }
    check_prop_formula(propensity_formula, strat$analysis_set, strat$treat)
  }
  if (is.na(suppressWarnings(as.integer(k)))) stop("k must be an integer")
  if (k < 1) stop("k must be 1 or greater")
}
