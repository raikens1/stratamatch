#----------------------------------------------------------
### CONTAINS: 
# auto_stratify its helper functions
#----------------------------------------------------------
#----------------------------------------------------------
### AUTO STRATIFY
#----------------------------------------------------------

#' Auto Stratify
#'
#' Automatically creates strata for matching based on a prognostic score formula
#' or a vector of prognostic scores already estimated by the user. Creates a
#' \code{auto_strata} object, which can be passed to \code{\link{big_match}} for
#' stratified matching or unpacked by the user to be matched by some other
#' means.
#'
#' Stratifying by prognostic score quantiles can be more effective than manually
#' stratifying a data set because the prognostic score is continuous, thus the
#' strata produced tend to be of equal size with similar prognosis.
#'
#' Automatic stratification requires information on how the prognostic scores
#' should be derived.  This is primarily determined by the specifciation of the
#' \code{prognosis} argument.  Three main forms of input for \code{prognosis}
#' are allowed: \enumerate{ \item A vector of prognostic scores. This vector
#' should be the same length and order of the rows in the data set.  If this
#' method is used, the \code{outcome} argument must also be specified; this is
#' simply a string giving the name of the column which contains outcome
#' information. \item A formula for prognosis (e.g. \code{outcome ~ X1 + X2}).
#' If this method is used, \code{auto_stratify} will automatically split the
#' data set into a \code{pilot_set} and an \code{analysis_set}.  The pilot set
#' will be used to fit a logistic regression model for outcome in the absence of
#' treatment, and this model will be used to estimate prognostic scores on the
#' analysis set.  The analysis set will then be stratified based on the
#' estimated prognostic scores.  In this case the \code{outcome} argument need
#' not be specified since it can be inferred from the input formula. \item A
#' model for prognosis (e.g. a \code{glm} object).  If this method is used, the
#' \code{outcome} argument must also be specified}
#'
#' @param data \code{data.frame} with observations as rows, features as columns
#' @param treat string giving the name of column designating treatment
#'   assignment
#' @param outcome string giving the name of column with outcome information.
#'   Required if prog_scores is specified.  Otherwise it will be inferred from
#'   prog_formula
#' @param prognosis information on how to build prognostic scores.  Three
#'   different input types are allowed: \enumerate{ \item vector of prognostic
#'   scores for all individuals in the data set. Should be in the same order as
#'   the rows of \code{data}. \item a \code{formula} for fitting a prognostic
#'   model \item an already-fit prognostic score model}
#' @param size numeric, desired size of strata (default = 2500)
#' @param held_frac numeric between 0 and 1 giving the proportion of controls to
#'   be allotted for building the prognostic score (default = 0.1)
#' @param held_sample a data.frame of held aside samples for building prognostic
#'   score model.
#' @return Returns a \code{strata} object
#' @seealso \code{\link{manual_stratify}}, \code{\link{new_auto_strata}}
#' @export
auto_stratify <- function(data, treat, prognosis,
                          outcome = NULL, size = 2500,
                          held_frac = 0.1, held_sample = NULL) {
  
  check_inputs_auto_stratify(data, treat, outcome)
  
  build <- build_prog_scores(data, treat, prognosis,
                             outcome, held_frac, held_sample)

  analysis_set <- build$analysis_set
  prog_scores <- build$prog_scores
  pilot_set <- build$pilot_set
  prog_model <- build$prog_model
  outcome <- build$outcome
  
  # Create strata from prognostic score quantiles
  n_bins <- ceiling(dim(analysis_set)[1] / size)
  qcut <- Hmisc::cut2(prog_scores, g = n_bins)

  # make strata table
  strata_table <- make_strata_table(qcut)
  analysis_set$stratum <- as.integer(qcut)

  # package and return result
  result <- new_auto_strata(analysis_set = analysis_set,
                            treat = treat,
                            call = match.call(),
                            issue_table = make_issue_table(analysis_set, treat),
                            strata_table = strata_table,
                            outcome = outcome,
                            prog_scores = prog_scores,
                            prog_model = prog_model,
                            pilot_set = pilot_set)

  return(result)
}

#----------------------------------------------------------
### Helpers
#----------------------------------------------------------


#' Build Prognostic Scores
#'
#' Not meant to be called externally. Given the arguments to auto_stratify,
#' build the prognostic scores and return the analysis set, the prognostic
#' scores, the pilot set, the prognostic model, and the outcome string.  The
#' primary function of this code is to determine the type of \code{prognosis}
#' and handle it appropriately.
#'
#' @inheritParams auto_stratify
#' @seealso \code{\link{auto_stratify}}
#'
#' @return a list of: analysis set, prognostic scores, pilot set, prognostic
#'   model, and outcome string
build_prog_scores <- function(data, treat, prognosis,
                              outcome, held_frac, held_sample){
  # prognosis is a vector of prognostic scores
  if (is.numeric(prognosis)){
    check_scores(prognosis, data)
    if (is.null(outcome)) {
      stop("If specifying prognostic scores, outcome must be specified")
    }
    prog_scores <- prognosis
    analysis_set <- data
    pilot_set <- NULL
    prog_model <- NULL
  }
  
  # prognosis is a formula
  else if (inherits(prognosis, "formula")){
    check_prog_formula(prognosis, data, outcome, treat)
    split <- split_pilot_set(data, treat, held_frac, held_sample)
    if (!is.null(held_sample) & !all(is.element(all.vars(prognosis), colnames(held_sample)))) {
      stop("All variables in prognostic score formula must be in held_sample")
    }
    pilot_set <- split$pilot_set
    analysis_set <- split$analysis_set
    outcome <- all.vars(prognosis)[1]
    message(paste("Fitting prognostic model:",
                  Reduce(paste, deparse(prognosis))))
    prog_model <- glm(prognosis, data = pilot_set, family = "binomial")
    prog_scores <- make_prog_scores(prog_model, analysis_set)
  }
  
  # prognosis is a model
  else if (TRUE){ # TODO: should enter here if predict does not throw an error?
    if (is.null(outcome)) {
      stop("If specifying a prognostic score model, outcome must be specified")
    }
    prog_scores <- predict(prognosis, newdata = data, type = "response")
    analysis_set <- data
    pilot_set <- NULL
    prog_model <- prognosis
  }
  else {stop("Error: prognosis type not recognized.")}

  return(list(analysis_set = analysis_set, prog_scores = prog_scores,
              pilot_set = pilot_set, prog_model = prog_model,
              outcome = outcome))
}

#' Split data into pilot and analysis sets
#'
#' Exported for the convenience of the user, although in practice this process
#' is almost always done using \code{\link{auto_stratify}}.  Given a data set
#' and some parameters about how to split the data, this function partitions the
#' data accordingly and returns the partitioned data as a list containing the
#' \code{analysis_set} and \code{pilot_set}.
#'
#' @inheritParams auto_stratify
#'
#' @return a list with analaysis_set and pilot_set
#' @export
split_pilot_set <- function(data, treat, held_frac, held_sample){
  check_pilot_set_options(held_sample, held_frac)
  pilot_set <- NULL
  
  # if held_sample is specified use that to build score
  if (!is.null(held_sample)){
    message("Using user-specified set for prognostic score modeling.")
    pilot_set <- held_sample
    analysis_set <- data
    
  } else {
    # otherwise, construct a pilot set
    message("Constructing a pilot set via subsampling.")
    # Adds an id column and removes it
    data$BigMatch_id <- 1:nrow(data)
    pilot_set <- data %>% dplyr::filter_(paste(treat, "==", 0)) %>%
      dplyr::sample_frac(held_frac, replace = FALSE)
    analysis_set <- dplyr::anti_join(data, pilot_set,
                                     by = "BigMatch_id") %>%
      dplyr::select(-BigMatch_id)
    pilot_set$BigMatch_id <- NULL
  }
  
  return(list(analysis_set = analysis_set, pilot_set = pilot_set))
}

#' Make Prog Scores
#'
#' Tries to make prognostic scores.  If successfull, returns them, otherwise
#' throws an error message.  Common failure mode is that the prognostic score is
#' built on some categorical variable that takes on some values in the analysis
#' set that are never seen in the pilot set.  Outputs are on the response scale,
#' (rather than the linear predictor), so the score is the expected value of the
#' outcome under the control assignement based on the observed covariates.
#'
#' @param prog_model Model of prognosis
#' @param analysis_set data set on which prognostic scores should be estimated
#'
#' @return vector of prognostic scores
make_prog_scores <- function(prog_model, analysis_set){
  tryCatch(predict(prog_model, analysis_set,
                   type = "response"),
           error = function(e) {
             if (e$call == "model.frame.default(Terms, newdata, na.action = na.action, xlev = object$xlevels)"){
               e$print <- paste("Error applying prognostic model:
                                Some categorical variable value(s) in
                                the analysis set do not appear in the
                                modeling set. Consider stratifying by
                                these variable(s).",
                                "GLM error:", e$print)
               stop(e)
             }
           })
}

#' Make strata table
#'
#' @param qcut the prognostic score quantile cuts
#'
#' @return data.frame of strata definitions
make_strata_table <- function(qcut){
  data.frame(qcut) %>%
    dplyr::mutate(stratum = as.integer(qcut), quantile_bin = qcut) %>%
    dplyr::group_by(quantile_bin) %>%
    dplyr::summarise(size = dplyr::n(), stratum = dplyr::first(stratum)) %>%
    dplyr::arrange(stratum) %>%
    dplyr::select(stratum, quantile_bin, size)
}

#----------------------------------------------------------
### Input Checkers
#----------------------------------------------------------


#' Check inputs from auto_stratify
#'
#' Not meant to be called externally.  Throws errors if basic auto_stratify 
#' inputs are incorrect.
#'
#' @inheritParams auto_stratify
#'
#' @return nothing; produces errors and warnings if anything is wrong
check_inputs_auto_stratify <- function(data, treat, outcome){
  # general checks
  if (!is.data.frame(data)) stop("data must be a data.frame")
  if ( !(is.character(treat) & length(treat) == 1)){
    stop("treat must be a single string")
  }
  if (!is.element(treat, colnames(data))){
    stop("treat must be the name of a column in data")
  }
  if (!is.null(outcome)){
    check_outcome(outcome, data, treat)
  }
}

#' Check Scores
#' 
#' Checks that prognostic scores are numeric and same length as data
#'
#' @inheritParams auto_stratify
#' @param prog_scores, a numeric vector
#'
#' @return nothing
check_scores <- function(prog_scores, data){
  if (length(prog_scores) != dim(data)[1]){
    stop("prognostic scores must be the same length as the data")
  }
}

#' Check Outcome
#' 
#' Checks that outcome is a string which is a column in the data
#'
#' @inheritParams auto_stratify
#'
#' @return nothing
check_outcome <- function(outcome, data, treat){
  if (!(is.character(outcome) & length(treat) == 1)) {
    stop("outcome must be a single string")
  }
  if (!is.element(outcome, colnames(data))) {
    stop("outcome must be a column in data")
  }
}

#' Check Prognostic Formula
#'
#' @inheritParams auto_stratify
#' @param prog_formula a formula for prognostic score
#' 
#' @return nothing
check_prog_formula <- function(prog_formula, data, outcome, treat){
  if (!all(is.element(all.vars(prog_formula), colnames(data)))) {
    stop("not all variables in prognosis formula appear in data")
  }
  if (treat == all.vars(prog_formula)[1]) {
    stop("prognostic formula must model outcome, not treatment")
  }
  if (is.element(treat, all.vars(prog_formula))) {
    stop("prognostic formula must model the outcome in the absence of treatment; the treatment assignment may not be a predictor for the prognostic score model")
  }
  if (!is.null(outcome)){
    if (outcome != all.vars(prog_formula)[1]){
      stop("prognostic formula must model outcome")
    }
  }
}

#' Check Prognostic Formula
#'
#' @inheritParams auto_stratify
#' 
#' @return nothing
check_pilot_set_options <- function(held_sample, held_frac){
  if (!is.numeric(held_frac)) stop("held_frac must be numeric")
  if (held_frac >= 1 | held_frac <= 0) stop("held_frac must be between 0 and 1")

  if (!is.null(held_sample) & !is.data.frame(held_sample)){
    stop("held_sample must be a data.frame")
  }
}
