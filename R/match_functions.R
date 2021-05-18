#----------------------------------------------------------
### CONTAINS:
# Matching methods to be called on strata objects, and their helpers
#----------------------------------------------------------

#' Strata Match
#'
#' Match within strata in series using optmatch.  Requires optmatch package to
#' be installed.
#'
#' @param object a strata object
#' @param model (optional) formula for matching.  If left blank, \emph{all}
#'   columns of the analysis set in \code{object} will be used as covariates in
#'   the propensity model or mahalanobis match (except outcome, treatment and
#'   stratum)
#' @param method either "prop" for propensity score matching based on a glm fit
#'   with model \code{model}, or "mahal" for mahalanobis distance matching by
#'   the covariates in \code{model}.
#' @param k the number of control individuals to be matched to each treated
#'   individual.  If \code{"k = full"} is used, fullmatching is done instead of
#'   pairmatching
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
#' strata_match(a.strat, model = treat ~ X1 + X2, k = 1)
#' # full match within strata based on mahalanobis distance.
#' strata_match(a.strat, model = treat ~ X1 + X2, method = "mahal", k = 1)
strata_match <- function(object, model = NULL, method = "prop", k = 1) {
  if (!requireNamespace("optmatch", quietly = TRUE)) {
    stop("optmatch package is required.  Please install it.")
  }
  
  check_inputs_matcher(object, model, k)
  
  message("This function makes essential use of the optmatch package, which has an academic license.")
  message("For more information, run optmatch::relaxinfo()")
  
  if (is.null(model)) {
    # match on all variables, stratified by stratum
    model <- formula(paste(
      object$treat, "~ . -", object$outcome,
      "- stratum"
    ))
  }
  
  matchdist <- make_match_distances(object, model, method)
  
  if (k == "full"){
    return(optmatch::fullmatch(matchdist), data = object$analysis_set)
  } else{
    return(optmatch::pairmatch(matchdist,
                               data = object$analysis_set,
                               controls = k
    )) 
  }
}

#' Match without Stratification
#'
#' Not meant to be called externally.  Match a data set without stratifying.
#' Used to compare performance with and without stratification.
#'
#' @inheritParams strata_match
#' 
#' @return a named factor with matching assignments
#' 
#' @keywords internal
strata_match_nstrat <- function(object, model = NULL, k = 1) {
  if (!requireNamespace("optmatch", quietly = TRUE)) {
    stop("optmatch package is required.  Please install it.")
  }

  check_inputs_matcher(object, model, k)

  if (is.null(model)) {
    # match on all variables, stratified by stratum
    model <- formula(paste(
      object$treat, "~ . -", object$outcome,
      "- stratum"
    ))
  } else {
    # do not modify original formula to append "+ strata(stratum)"
  }

  # build propensity model
  propensity_model <- glm(model,
    data = object$analysis_set,
    family = binomial()
  )

  return(optmatch::pairmatch(propensity_model,
    data = object$analysis_set,
    controls = k
  ))
}

#' Check inputs to any matching function
#'
#' @inheritParams strata_match
#'
#' @return nothing
#' @keywords internal
check_inputs_matcher <- function(object, model, k) {
  if (!is.strata(object)) {
    stop("strat must be a strata object")
  }
  if (!is.null(model)) {
    if (!inherits(model, "formula")) {
      stop("model must be a formula")
    }
    check_prop_formula(model, object$analysis_set, object$treat)
  }
  if (!k == "full"){
    if (is.na(suppressWarnings(as.integer(k)))) stop("k must be an integer for pair matching, or \"full\" for full matching")
    if (k < 1) stop("When pair matching, k must be 1 or greater") 
  }
}

#' Make match distances within strata
#'
#' Makes the match distance with strata specifications for \code{strata_match}.
#' This function is largely unecessary to call outside of stratamatch, but it is
#' exported for the benefit of the user to aid in debugging.
#'
#' @inheritParams strata_match
#'
#' @return a match distance matrix for optmatch
#' @export
#'
#' @examples
#' 
#' dat <- make_sample_data(n = 75)
#'
#' # stratify with auto_stratify
#' a.strat <- auto_stratify(dat, "treat", outcome ~ X2, size = 25)
#' md <- make_match_distances(a.strat, treat ~ X1 + X2, method = "mahal")
make_match_distances <- function(object, model, method){
  
  strata_formula <- formula(paste(object$treat, "~ stratum"))
  
  if (method == "prop"){
    message(paste(
      "Fitting propensity model:",
      Reduce(paste, deparse(model))
    ))
    
    # build propensity model
    propensity_model <- glm(model,
                            data = object$analysis_set,
                            family = binomial()
    )
    
    # optmatch issues incorrect warning.  Catch this and suppress it.
    withCallingHandlers(matchdist <- optmatch::match_on(propensity_model,
                                              within = optmatch::exactMatch(strata_formula,
                                                                  data = object$analysis_set)), 
                        warning = function(w) {
                          if(grepl("Error gathering complete data", w$message)){
                            message(w$type)
                            invokeRestart("muffleWarning")
                          } 
                        })
  } else if (method == "mahal"){
    message(paste(
      "Using Mahalanobis distance:",
      Reduce(paste, deparse(model))
    ))
    
    matchdist <- optmatch::match_on(model, data = object$analysis_set,
                          within = optmatch::exactMatch(strata_formula,
                                              data = object$analysis_set))
  } else{
    stop(paste("Method ", method, " not suppported."))
  }
  return(matchdist)
}
