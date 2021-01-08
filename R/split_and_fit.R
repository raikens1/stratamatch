#----------------------------------------------------------
### CONTAINS:
# split_and_fit
#----------------------------------------------------------

#' Split and fit
#'
#' Splits a dataset (optional) and fits propensity and pronostic scores
#' according to specified models, then returns the result without stratifying.
#'
#' @inheritParams new_split_fit
#'
#' @return a \code{split_fit} object
#' @export
#'
#' @examples
split_and_fit <- function(data, treat, prognosis, propensity,
                          outcome = NULL, 
                          pilot_fraction = 0.1, pilot_size = NULL,
                          pilot_sample = NULL, group_by_covariates = NULL) {
  # check base inputs
  check_base_inputs_auto_stratify(data, treat, outcome)
  
  # if input data is grouped, all sorts of strange things happen
  data <- data %>% dplyr::ungroup()
  
  propensity_scores <- get_prop_scores(propensity, data, treat)
  
  build <- build_autostrata(
    data, treat, prognosis, outcome, pilot_fraction,
    pilot_size, pilot_sample, group_by_covariates
  )
  
  analysis_set <- build$analysis_set
  prognostic_scores <- build$prognostic_scores
  pilot_set <- build$pilot_set
  prognostic_model <- build$prognostic_model
  outcome <- build$outcome
  
  # package and return result
  result <- list(
    analysis_set = analysis_set,
    treat = treat,
    call = match.call(),
    outcome = outcome,
    prognostic_scores = prognostic_scores,
    prognostic_model = prognostic_model,
    propensity_scores = propensity_scores,
    pilot_set = pilot_set
  )
  
  return(result)
}

#' New Split and Fit
#'
#' Basic constructor for an \code{split_fit} object. These objects hold all the
#' information associated with a dataset that has been split and fit
#' \code{\link{split_fit}}.  Plotting and matching of this object is not yet
#' supported but may be in the future.
#'
#' @param outcome a string giving the name of the column where outcome
#'   information is stored
#' @param treat a string giving the name of the column where treatment
#'   information is stored
#' @param analysis_set the data set
#' @param call the call to \code{split_and_fit} used to generate this object
#' @param prognostic_scores a vector of prognostic scores on the analysis set.
#' @param prognostic_model a model for prognosis fit on the pilot data set.
#' @param propensity a vector of prognostic scores on the analysis set.
#' @param pilot_set the set of controls used to fit the prognostic model. These
#'   are excluded from subsequent analysis so that the prognostic score is not
#'   overfit to the data used to estimate the treatment effect.
#'
#' @seealso \code{\link{split_and_fit}}, a function which calls this constructor
#'   to produce a \code{split_fit} object.
#'
#' @return a basic \code{split_fit} object
#' @keywords internal
new_split_fit <- function(outcome, treat,
                            analysis_set = NULL,
                            call = NULL,
                            prognostic_scores = NULL,
                            prognostic_model = NULL,
                            propensity_scores = NULL,
                            propensity_model = NULL,
                            pilot_set = NULL) {
  stopifnot(is.character(outcome))
  stopifnot(is.character(treat))
  stopifnot(is.data.frame(analysis_set))
  stopifnot(is.call(call))
  stopifnot(is.data.frame(issue_table))
  stopifnot(is.data.frame(strata_table))
  stopifnot(is.numeric(prognostic_scores))
  
  my_split_fit <- structure(list(
    analysis_set = analysis_set,
    treat = treat,
    call = call,
    outcome = outcome,
    prognostic_scores = prognostic_scores,
    prognostic_model = prognostic_model,
    propensity_scores = propensity_scores,
    propensity_model = propensity_model,
    pilot_set = pilot_set
  ),
  class = c("split_fit")
  )
  return(my_split_fit)
}