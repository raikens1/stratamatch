#----------------------------------------------------------
### CONTAINS: 
# Low-level constructors for strata objects
#----------------------------------------------------------

#' New Autostrata
#'
#' Basic constructor for an \code{auto_strata} object. These objects hold all
#' the information associated with a dataset that has been stratified via
#' \code{\link{auto_stratify}}.  This object may be passed to
#' \code{\link{big_match}} to be matched or it may be unpacked by the user to be
#' matched by other means.
#'
#' @param outcome a string giving the name of the column where outcome
#'   information is stored
#' @param treat a string giving the name of the column where treatment
#'   information is stored
#' @param analysis_set the data set which will be stratified
#' @param call the call to \code{auto_stratify} used to generate this object
#' @param issue_table a table of each stratum and potential issues of size and
#'   treat:control balance
#' @param strata_table a table of each stratum and the prognostic score quantile
#'   bin this corresponds to
#' @param prog_scores a vector of prognostic scores.  Either \code{prog_scores}
#'   or \code{prog_model} must be specified to stratify a dataset automatically.
#' @param prog_model a model for prognosis fit on a separate data set. Either
#'   \code{prog_scores} or \code{prog_model} must be specified to stratify a
#'   dataset automatically.
#' @param pilot_set the set of controls used to fit the prognostic model.
#'   These are excluded from subsequent analysis so that the prognostic score is
#'   not overfit to the data used to estimate the treatment effect.
#'
#' @seealso \code{\link{auto_stratify}}, a function which calls this constructor
#'   to produce an \code{auto_strata} object.
#'
#' @return a basic \code{auto_strata} object
#' @export
new_auto_strata <- function(outcome, treat, 
                           analysis_set = NULL,
                           call = NULL,
                           issue_table = NULL,
                           strata_table = NULL,
                           prog_scores = NULL,
                           prog_model = NULL,
                           pilot_set = NULL){

  my_autostrata <- structure(list(analysis_set = analysis_set,
                                 treat = treat,
                                 call = call,
                                 issue_table = issue_table,
                                 strata_table = strata_table,
                                 outcome = outcome,
                                 prog_scores = prog_scores,
                                 prog_model = prog_model,
                                 pilot_set = pilot_set),
                            class = c("auto_strata", "strata"))
  return(my_autostrata)
}

#' New Manual Strata
#' 
#' Basic constructor for an \code{manual_strata} object. These objects hold all
#' the information associated with a dataset that has been stratified via
#' \code{\link{manual_stratify}}.  This object may be passed to
#' \code{\link{big_match}} to be matched or it may be unpacked by the user to be
#' matched by other means.
#'
#' @param treat a string giving the name of the column where treatment
#'   information is stored
#' @param covariates 
#' @param analysis_set the data set which will be stratified
#' @param call the call to \code{auto_stratify} used to generate this object
#' @param issue_table a table of each stratum and potential issues of size and
#'   treat:control balance
#' @param strata_table a table of each stratum and the covariate bin this corresponds to
#' @return a basic \code{manual_strata} object
#' @export
new_manual_strata <- function(treat, covariates, 
                             analysis_set = NULL,
                             call = NULL,
                             issue_table = NULL,
                             strata_table = NULL){

  my_manualstrata <- structure(list(analysis_set = analysis_set,
                                    treat = treat,
                                    call = call,
                                    issue_table = issue_table,
                                    covariates = covariates,
                                    strata_table = strata_table),
                               class = c("manual_strata", "strata"))
  
}