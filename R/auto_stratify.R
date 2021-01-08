#----------------------------------------------------------
### CONTAINS:
# auto_stratify its helper functions, except split_pilot_set
#----------------------------------------------------------
#----------------------------------------------------------
### AUTO STRATIFY
#----------------------------------------------------------

#' Auto Stratify
#'
#' Automatically creates strata for matching based on a prognostic score formula
#' or a vector of prognostic scores already estimated by the user. Creates a
#' \code{auto_strata} object, which can be passed to \code{\link{strata_match}}
#' for stratified matching or unpacked by the user to be matched by some other
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
#' @section Troubleshooting:
#'
#'   This section suggests fixes for common errors that appear while fitting the
#'   prognostic score or using it to estimate prognostic scores on the analysis
#'   set.
#'
#'   \itemize{
#'
#'   \item \code{Encountered an error while fitting the prognostic model...
#'   numeric probabilities 0 or 1 produced}. This error means that the
#'   prognostic model can perfectly separate positive from negative outcomes.
#'   Estimating a treatment effect in this case is unwise since an individual's
#'   baseline characteristics perfectly determine their outcome, regardless of
#'   whether they recieve the treatment.  This error may also appear on rare
#'   occaisions when your pilot set is very small (number of observations
#'   approximately <= number of covariates in the prognostic model), so that
#'   perfect separation happens by chance.
#'
#'   \item \code{Encountered an error while estimating prognostic scores ...
#'   factor X has new levels ... } This may indicate that some value(s) of one
#'   or more categorical variables appear in the analysis set which were not
#'   seen in the pilot set. This means that when we try to obtain prognostic
#'   scores for our analysis set, we run into some new value that our prognostic
#'   model was not prepared to handle.  There are a few options we have to
#'   troubleshoot this problem: \itemize{
#'
#'   \item \strong{Rejection sampling.}  Run \code{auto_stratify} again with the
#'   same arguments until this error does not occur (i.e. until some
#'   observations with the missing value are randomly selected into the pilot
#'   set)
#'
#'   \item \strong{Eliminate this covariate from the prognostic formula.}
#'
#'   \item \strong{Remove observations with the rare covariate value from the
#'   entire data set.} Consider carefully how this exclusion might affect your
#'   results.
#'
#'   } }
#'
#'   Other errors or warnings can occur if the pilot set is too small and the
#'   prognostic formula is too complicated.  Always make sure that the number of
#'   observations in the pilot set is large enough that you can confidently fit
#'   a prognostic model with the number of covariates you want.
#'
#' @param data \code{data.frame} with observations as rows, features as columns
#' @param treat string giving the name of column designating treatment
#'   assignment
#' @param outcome string giving the name of column with outcome information.
#'   Required if prognostic_scores is specified.  Otherwise it will be inferred
#'   from prog_formula
#' @param prognosis information on how to build prognostic scores.  Three
#'   different input types are allowed: \enumerate{ \item vector of prognostic
#'   scores for all individuals in the data set. Should be in the same order as
#'   the rows of \code{data}. \item a \code{formula} for fitting a prognostic
#'   model \item an already-fit prognostic score model}
#' @param size numeric, desired size of strata (default = 2500)
#' @param pilot_fraction numeric between 0 and 1 giving the proportion of
#'   controls to be allotted for building the prognostic score (default = 0.1)
#' @param pilot_size alternative to pilot_fraction. Approximate number of
#'   observations to be used in pilot set. Note that the actual pilot set size
#'   returned may not be exactly \code{pilot_size} if \code{group_by_covariates}
#'   is specified because balancing by covariates may result in deviations from
#'   desired size. If \code{pilot_size} is specified, \code{pilot_fraction} is ignored.
#' @param pilot_sample a data.frame of held aside samples for building
#'   prognostic score model. If \code{pilot_sample} is specified, \code{pilot_size} and
#'   \code{pilot_fraction} are both ignored.
#' @param group_by_covariates character vector giving the names of covariates to
#'   be grouped by (optional). If specified, the pilot set will be sampled in a
#'   stratified manner, so that the composition of the pilot set reflects the
#'   composition of the whole data set in terms of these covariates.  The
#'   specified covariates must be categorical.
#' @return Returns an \code{auto_strata} object.  This contains: \itemize{
#'
#'   \item \code{outcome} - a string giving the name of the column where outcome
#'   information is stored
#'
#'   \item \code{treat} - a string giving the name of the column encoding
#'   treatment assignment
#'
#'   \item \code{analysis_set} - the data set with strata assignments
#'
#'   \item \code{call} - the call to \code{auto_stratify} used to generate this
#'   object
#'
#'   \item \code{issue_table} - a table of each stratum and potential issues of
#'   size and treat:control balance
#'
#'   \item \code{strata_table} - a table of each stratum and the prognostic
#'   score quantile bin to which it corresponds
#'
#'   \item \code{prognostic_scores} - a vector of prognostic scores.
#'
#'   \item \code{prognostic_model} - a model for prognosis fit on a pilot data
#'   set.  Will be \code{NULL} if a vector of prognostic scores was provided as
#'   the \code{prognosis} argument to \code{auto_stratify} rather than a model
#'   or formula.
#'
#'   \item \code{pilot_set} - the set of controls used to fit the prognostic
#'   model. These are excluded from subsequent analysis so that the prognostic
#'   score is not overfit to the data used to estimate the treatment effect.
#'   Will be \code{NULL} if a pre-fit model or a vector of prognostic scores was
#'   provided as the \code{prognosis} argument to \code{auto_stratify} rather
#'   than formula.
#'
#'   }
#' @seealso \code{\link{manual_stratify}}, \code{\link{new_auto_strata}}
#' @export
#' @examples
#' # make sample data set
#' set.seed(111)
#' dat <- make_sample_data(n = 75)
#'
#' # construct a pilot set, build a prognostic score for `outcome` based on X2
#' # and stratify the data set based on the scores into sets of about 25
#' # observations
#' a.strat_formula <- auto_stratify(dat, "treat", outcome ~ X2, size = 25)
#'
#' # stratify the data set based on a model for prognosis
#' pilot_data <- make_sample_data(n = 30)
#' prognostic_model <- glm(outcome ~ X2, pilot_data, family = "binomial")
#' a.strat_model <- auto_stratify(dat, "treat", prognostic_model,
#'   outcome = "outcome", size = 25
#' )
#'
#' # stratify the data set based on a vector of prognostic scores
#' prognostic_scores <- predict(prognostic_model,
#'   newdata = dat,
#'   type = "response"
#' )
#' a.strat_scores <- auto_stratify(dat, "treat", prognostic_scores,
#'   outcome = "outcome", size = 25
#' )
#'
#' # diagnostic plots
#' plot(a.strat_formula)
#' plot(a.strat_formula, type = "AC", propensity = treat ~ X1, stratum = 1)
#' plot(a.strat_formula, type = "hist", propensity = treat ~ X1, stratum = 1)
#' plot(a.strat_formula, type = "residual")
auto_stratify <- function(data, treat, prognosis,
                          outcome = NULL, size = 2500,
                          pilot_fraction = 0.1, pilot_size = NULL,
                          pilot_sample = NULL, group_by_covariates = NULL) {
  check_base_inputs_auto_stratify(data, treat, outcome)

  # if input data is grouped, all sorts of strange things happen
  data <- data %>% dplyr::ungroup()

  build <- build_autostrata(
    data, treat, prognosis, outcome, pilot_fraction,
    pilot_size, pilot_sample, group_by_covariates
  )

  analysis_set <- build$analysis_set
  prognostic_scores <- build$prognostic_scores
  pilot_set <- build$pilot_set
  prognostic_model <- build$prognostic_model
  outcome <- build$outcome

  # Create strata from prognostic score quantiles
  n_bins <- ceiling(dim(analysis_set)[1] / size)
  qcut <- Hmisc::cut2(prognostic_scores, g = n_bins)

  # make strata table
  strata_table <- make_autostrata_table(qcut)
  analysis_set$stratum <- as.integer(qcut)

  # package and return result
  result <- new_auto_strata(
    analysis_set = analysis_set,
    treat = treat,
    call = match.call(),
    issue_table = make_issue_table(analysis_set, treat),
    strata_table = strata_table,
    outcome = outcome,
    prognostic_scores = prognostic_scores,
    prognostic_model = prognostic_model,
    pilot_set = pilot_set
  )

  return(result)
}

#----------------------------------------------------------
### Helpers
#----------------------------------------------------------


#' Build Autostrata object
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
#' @keywords internal
build_autostrata <- function(data, treat, prognosis, outcome, pilot_fraction,
                             pilot_size, pilot_sample, group_by_covariates) {

  # if prognosis is a vector of prognostic scores
  if (is.numeric(prognosis)) {
    check_scores(prognosis, data, outcome)
    prognostic_scores <- prognosis
    analysis_set <- data
    pilot_set <- NULL
    prognostic_model <- NULL
  }

  # if prognosis is a formula
  else if (inherits(prognosis, "formula")) {
    check_prognostic_formula(prognosis, data, outcome, treat)

    if (!is.null(pilot_sample)) {
      # check pilot sample
      if (!is.data.frame(pilot_sample)) {
        stop("pilot_sample must be a data.frame")
      }
      message("Using user-specified set for prognostic score modeling.")
      pilot_set <- pilot_sample
      analysis_set <- data
    } else {
      split <- split_pilot_set(
        data, treat, pilot_fraction, pilot_size,
        group_by_covariates
      )
      pilot_set <- split$pilot_set
      analysis_set <- split$analysis_set
    }
    outcome <- all.vars(prognosis)[1]

    prognostic_model <- fit_prognostic_model(pilot_set, prognosis, outcome)
    prognostic_scores <- estimate_scores(prognostic_model, analysis_set)
  }

  # if prognosis is a model, or it is unrecognized
  else {
    # try to predict.  If successful, prognosis was a model.
    # otherwise, throw an error: prognosis type not recognized
    prognostic_scores <- tryCatch(predict(prognosis,
      newdata = data,
      type = "response"
    ),
    error = function(c) {
      stop("prognosis type not recognized")
    }
    )
    if (is.null(outcome)) {
      stop("If specifying a prognostic score model, outcome must be specified")
    }
    analysis_set <- data
    pilot_set <- NULL
    prognostic_model <- prognosis
  }

  return(list(
    analysis_set = analysis_set, prognostic_scores = prognostic_scores,
    pilot_set = pilot_set, prognostic_model = prognostic_model,
    outcome = outcome
  ))
}


#' Fit Prognostic Model
#'
#' Given a pilot set and a prognostic formula, return the fitted formula.  If
#' the outcome is binary, fit a logistic regression.  Otherwise, fit a linear
#' model.
#'
#' @param dat data.frame on which model should be fit
#' @param prognostic_formula formula for prognostic model
#' @param outcome string giving name of column of data where outcomes are
#'   recorded
#'
#' @return a glm or lm object fit from \code{prognostic_formula} on \code{data}
#' @keywords internal
fit_prognostic_model <- function(dat, prognostic_formula, outcome) {

  # if outcome is binary or logical, run logistic regression
  if (is_binary(na.omit(dat[[outcome]]))) {
    message(paste(
      "Fitting prognostic model via logistic regression:",
      Reduce(paste, deparse(prognostic_formula))
    ))
    withCallingHandlers({prognostic_model <- glm(prognostic_formula,
      data = dat,
      family = "binomial"
    )},
    error = function(e) {
      message("Error while fitting the prognostic model.")
      message("For troubleshooting help, run help(\"auto_stratify\")")
      stop(e)
    },
    warning = function(w) {
      message("Warning while fitting the prognostic model.")
      message("For troubleshooting help, run help(\"auto_stratify\")")
      warning(w)
    }
    )
  }

  # if outcome is numeric, run linear regression
  else if (is.numeric(na.omit(dat[[outcome]]))) {
    if (length(unique(na.omit(dat[[outcome]]))) == 2) {
      warning("outcome column has only two values. Is it binary?")
      message("Convert to 01 or logical format to run logistic regression instead")
    }
    message(paste(
      "Fitting prognostic model via linear regression:",
      Reduce(paste, deparse(prognostic_formula))
    ))
    withCallingHandlers({prognostic_model <- lm(prognostic_formula, data = dat)},
      error = function(e) {
        message("Error while fitting the prognostic model.")
        message("For troubleshooting help, run help(\"auto_stratify\")")
        stop(e)
      },
      warning = function(w) {
        message("Warning while fitting the prognostic model.")
        message("For troubleshooting help, run help(\"auto_stratify\")")
        warning(w)
      }
    )
  }
  else {
    stop("Outcome was not a recognized type. Must be binary, logical, or numeric")
  }

  return(prognostic_model)
}

#' Estimate Prognostic Scores
#'
#' Tries to make prognostic scores.  If successfull, returns them, otherwise
#' throws an error message.  Common failure mode is that the prognostic score is
#' built on some categorical variable that takes on some values in the analysis
#' set that are never seen in the pilot set.  Outputs are on the response scale,
#' (rather than the linear predictor), so the score is the expected value of the
#' outcome under the control assignement based on the observed covariates.
#'
#' @param prognostic_model Model of prognosis
#' @param analysis_set data set on which prognostic scores should be estimated
#'
#' @return vector of prognostic scores
#' @keywords internal
estimate_scores <- function(prognostic_model, analysis_set) {
  withCallingHandlers({scores <- predict(prognostic_model, analysis_set, type = "response")},
    error = function(e) {
      message("Error while estimating prognostic scores from the prognostic model.")
      message("For troubleshooting help, run help(\"auto_stratify\")")
      stop(e)
    },
    warning = function(w) {
      message("Warning while estimating prognostic scores from the prognostic model.")
      message("For troubleshooting help, run help(\"auto_stratify\")")
      warning(w)
    }
  )
  
  return(scores)
}

#' Make strata table
#'
#' @param qcut the prognostic score quantile cuts
#'
#' @return data.frame of strata definitions
#' @keywords internal
make_autostrata_table <- function(qcut) {
  data.frame(qcut) %>%
    dplyr::mutate(stratum = as.integer(qcut), quantile_bin = as.character(qcut)) %>%
    dplyr::group_by(.data$quantile_bin) %>%
    dplyr::summarise(
      size = dplyr::n(), stratum = dplyr::first(.data$stratum),
      .groups = "drop_last"
    ) %>%
    dplyr::arrange(.data$stratum) %>%
    dplyr::select(.data$stratum, .data$quantile_bin, .data$size)
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
#' @keywords internal
check_base_inputs_auto_stratify <- function(data, treat, outcome) {
  # general checks
  if (!is.data.frame(data)) stop("data must be a data.frame")
  if (!(is.character(treat) & length(treat) == 1)) {
    stop("treat must be a single string")
  }
  if (!is.element(treat, colnames(data))) {
    stop("treat must be the name of a column in data")
  }
  if (!is_binary(data[[treat]])) {
    stop("treatment column must be binary or logical")
  }
  if (!is.null(outcome)) {
    check_outcome(outcome, data, treat)
  }
}

#' Check Scores
#'
#' Checks that prognostic scores are the same length as data
#'
#' @inheritParams auto_stratify
#' @param prognostic_scores, a numeric vector
#'
#' @return nothing
#' @keywords internal
check_scores <- function(prognostic_scores, data, outcome) {
  if (length(prognostic_scores) != dim(data)[1]) {
    stop("prognostic scores must be the same length as the data")
  }
  if (is.null(outcome)) {
    stop("If specifying prognostic scores, outcome must be specified")
  }
}

#' Check Outcome
#'
#' Checks that outcome is a string which is a column in the data
#'
#' @inheritParams auto_stratify
#'
#' @return nothing
#' @keywords internal
check_outcome <- function(outcome, data, treat) {
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
#' @keywords internal
check_prognostic_formula <- function(prog_formula, data, outcome, treat) {
  if (treat == all.vars(prog_formula)[1]) {
    stop("prognostic formula must model outcome, not treatment")
  }

  if (is.element(treat, all.vars(prog_formula))) {
    RHS <- as.character(prog_formula)[3]
    allowed <- paste("-", treat)

    # check that every time treat appears on RHS, it appears as "- treat"
    if (lengths(regmatches(RHS, gregexpr(allowed, RHS))) != lengths(regmatches(RHS, gregexpr(treat, RHS)))) {
      stop("prognostic formula must model the outcome in the absence of treatment; the treatment assignment may not be a predictor for the prognostic score model")
    }
  }
  if (!is.null(outcome)) {
    if (outcome != all.vars(prog_formula)[1]) {
      stop("prognostic formula must model outcome")
    }
  }
}


#' Check if a vector is binary
#'
#' return TRUE if the input is logical or if it contains only 0's and 1's
#'
#' @param col a column from a data frame
#'
#' @return logical
#' @keywords internal
is_binary <- function(col) {
  if (is.logical(na.omit(col))) {
    return(TRUE)
  }
  if (all(is.element(na.omit(col), 0:1))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
