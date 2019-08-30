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
#' There are two main methods:
#'
#' \enumerate{ \item Prespecify a vector of prognostic scores
#' (\code{prog_scores}) that you would like to use. \item Provide a prognostic
#' formula (\code{prog_formula}) and allow \code{auto_stratify} to partition a
#' 'pilot' data set of controls on which to fit a prognostic score.  The
#' individuals from the pilot set will be removed from the rest of the data (the
#' analysis set). There are various methods for specifying how to partition the
#' pilot set. }
#'
#' At least one of these two methods must be used. As a result, at least one of
#' \code{prog_scores} or \code{prog_formula} must be supplied.
#'
#' @param data \code{data.frame} with observations as rows, features as columns
#' @param treat string giving the name of column designating treatment
#'   assignment
#' @param outcome string giving the name of column with outcome information.
#'   Required if prog_scores is specified.  Otherwise it will be inferred from
#'   prog_formula
#' @param prog_formula formula for building prognostic score model
#' @param prog_scores vector of prognostic scores for all individuals in the
#'   data set. Should be in the same order as the rows of \code{data}.
#' @param size numeric, desired size of strata (default = 2500)
#' @param held_frac numeric between 0 and 1 giving the proportion of controls to
#'   be allotted for building the prognostic score (default = 0.1)
#' @param held_sample a data.frame of held aside samples for building prognostic
#'   score model.
#' @return Returns a \code{strata} object
#' @seealso \code{\link{manual_stratify}}, \code{\link{new_auto_strata}}
#' @export
auto_stratify <- function(data, treat, outcome = NULL, prog_formula = NULL,
                          prog_scores = NULL, size = 2500,
                          held_frac = 0.1, held_sample = NULL) {

  check_inputs_auto_stratify(data, treat, outcome, prog_formula,
                             prog_scores, size, held_frac, held_sample)

  # if prognostic scores are given, use them by default
  if (!is.null(prog_scores)){
      analysis_set <- data
      pilot_set <- NULL
      prog_model <- NULL
  } else {
    # if prog_scores are not specified, build them
    outcome <- all.vars(prog_formula)[1]
    prog_build <- build_prog_model(data, treat,
                                   prog_formula, held_frac,
                                   held_sample)
    prog_model <- prog_build$m
    analysis_set <- prog_build$a_set
    prog_scores <- make_prog_scores(prog_model, analysis_set)
    pilot_set <- prog_build$p_set
  }

  # Create strata from prognostic score quantiles
  n_bins <- ceiling(dim(analysis_set)[1] / size)
  qcut <- Hmisc::cut2(prog_scores, g = n_bins)

  # make strata table
  strata_table <- make_strata_table_qcut(qcut)
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
### General Helpers
#----------------------------------------------------------

#' Build Prognostic Model
#'
#' Not meant to be called externally. If a pilot set is not provided, one is
#' partitioned from the input data according to the input parameters. The
#' prognostic model is then fit on the pilot set based on the prognostic score
#' formula (\code{prog_score}).
#'
#' @param data \code{data.frame} with observations as rows, features as columns
#' @param treat string giving the name of column designating treatment
#'   assignment
#' @param prog_formula formula for building prognostic score model
#' @param held_frac, numeric giving the fraction of controls to partition to the
#'   pilot set (default = 0.1)
#' @param held_sample, (optional) a held aside dataset to be used to fit the
#'   prognostic score model
#' @return Returns a \code{list} of: m, a \code{glm} object prognostic model,
#'   m_set, the model set, and a_set, the analysis set (data - model set)
build_prog_model <- function(data, treat,
                             prog_formula, held_frac = 0.1,
                             held_sample = NULL){
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
    data$join_id <- 1:nrow(data)
    pilot_set <- data %>% dplyr::filter_(paste(treat, "==", 0)) %>%
      dplyr::sample_frac(held_frac, replace = FALSE)
    analysis_set <- dplyr::anti_join(data, pilot_set,
                                     by = "join_id") %>%
      dplyr::select(-join_id)
    pilot_set$join_id <- NULL
  }
  message(paste("Fitting prognostic model:",
                Reduce(paste, deparse(prog_formula))))

  model <- glm(prog_formula, data = pilot_set, family = "binomial")

  return(list(m = model, p_set = pilot_set, a_set = analysis_set))
}

#' Make Prog Scores
#'
#' Tries to make prognostic scores.  If successfull, returns them, otherwise
#' throws an error message.  Common failure mode is that the prognostic score is
#' built on some categorical variable that takes on some values in the analysis
#' set that are never seen in the pilot set.
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
make_strata_table_qcut <- function(qcut){
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
#' Not meant to be called externally.  Throws errors if auto_stratify inputs are
#' incorrect.
#'
#' @inheritParams auto_stratify
#'
#' @return nothing; produces errors and warnings if anything is wrong
check_inputs_auto_stratify <- function(data, treat, outcome,
                                       prog_formula,
                                       prog_scores, size,
                                       held_frac, held_sample){
  # general checks
  if (!is.data.frame(data)) stop("data must be a data.frame")
  if (!is.character(treat) & length(treat) == 1){
    stop("treat must be a single string")
  }
  if (!is.element(treat, colnames(data))){
    stop("treat must be the name of a column in data")
  }

  # Check that either prognostic formula or scores are supplied
  scores_given <- !is.null(prog_scores)

  if (!is.null(prog_formula) && scores_given){
    warning("prog_formula and prog_scores are both specified. Using prog_scores; ignoring formula.")
  }

  if (is.null(prog_formula) && !scores_given){
    stop("At least one of prog_formula and prog_scores should be specified.")
  }

  # if scores are given, check format
  if (scores_given){
    check_scores(prog_scores, data)
    check_outcome(outcome, data, treat)
  } else{
    check_prog_formula(prog_formula, data, outcome, treat)
    check_pilot_set_options(held_sample, held_frac, prog_formula)
  }
}

#' Check Scores
#' 
#' Checks that prognostic scores are numeric and same length as data
#'
#' @inheritParams check_inputs_auto_stratify
#'
#' @return nothing
check_scores <- function(prog_scores, data){
  if (length(prog_scores) != dim(data)[1]){
    stop("prog_scores must be the same length as the data")
  }
  if (!is.numeric(prog_scores)){
    stop("prognostic scores must be numeric")
  }
}

#' Check Outcome
#' 
#' Checks that outcome is a string and is provided when scores are provided
#'
#' @inheritParams check_inputs_auto_stratify
#'
#' @return nothing
check_outcome <- function(outcome, data, treat){
  if (is.null(outcome)) {
    stop("If using prog_scores, outcome must be specified")
  }
  if (!is.character(outcome) & length(treat) == 1) {
    stop("outcome must be a single string")
  }
  if (!is.element(outcome, colnames(data))) {
    stop("outcome must be a column in data")
  }
}

#' Check Prognostic Formula
#'
#' @inheritParams check_inputs_auto_stratify
#' 
#' @return nothing
check_prog_formula <- function(prog_formula, data, outcome, treat){
  if (!inherits(prog_formula, "formula")) {
    stop("prog_formula must be a formula")
  }
  if (!all(is.element(all.vars(prog_formula), colnames(data)))) {
    stop("not all variables in prog_formula appear in data")
  }
  if (treat == all.vars(prog_formula)[1]) {
    stop("prognostic formula must model outcome, not treatment")
  }
  if (!is.null(outcome)){
    if (outcome != all.vars(prog_formula)[1]){
      stop("prognostic formula must model outcome")
    }
  }
}

#' Check Prognostic Formula
#'
#' @inheritParams check_inputs_auto_stratify
#' 
#' @return nothing
check_pilot_set_options <- function(held_sample, held_frac, prog_formula){
  if (!is.numeric(held_frac)) stop("held_frac must be numeric")
  if (held_frac >= 1 | held_frac <= 0) stop("held_frac must be between 0 and 1")

  if (!is.null(held_sample)){
    if (!is.data.frame(held_sample)) stop("held_sample must be a data.frame")
    if (!all(is.element(all.vars(prog_formula), colnames(held_sample)))) {
      stop("All variables in prog_formula must be in held_sample")
    }
  }
}
