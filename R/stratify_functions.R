#----------------------------------------------------------
### CONTAINS: 
# manual_stratify and auto_stratify, and their helper functions
#----------------------------------------------------------
#----------------------------------------------------------
### MANUAL STRATIFY
#----------------------------------------------------------

#' Manual Stratify
#'
#' Stratifies a data set based on a set of blocking covariates specified by the
#' user. Creates a \code{manual_strata} object, which can be passed to
#' \code{\link{big_match}} for stratified matching or unpacked by the user to be
#' matched by some other means.
#'
#' @param data data.frame with observations as rows, features as columns
#' @param treat the name of the column which specifies treatment assignment
#' @param covariates a vector strings, which are the names of the columns to be
#'   used as covariates stratification
#' @param force a boolean. If true, run even if a variable appears continuous.
#'   (default = FALSE)
#' @return A \code{manual_strata} object
#' @seealso \code{\link{auto_stratify}}, \code{\link{new_manual_strata}}
#' @export
manual_stratify <- function(data, treat, covariates, force = FALSE){

  result <- new_manual_strata(analysis_set = NULL,
                            treat = treat,
                            call = match.call(),
                            issue_table = NULL,
                            covariates = covariates,
                            strata_table = NULL)

  n <- dim(data)[1]

  # Check that all covariates are discrete
  for (i in 1:length(covariates)){
    warn_if_continuous(data[, covariates[i]], covariates[i], force, n)
  }

  # helper function to extract group labels from dplyr
  get_next_integer <- function(){
    i <- 0
    function(u, v) {
      i <<- i + 1
    }
  }
  get_integer <- get_next_integer()

  # Interact covariates
  grouped_table <- dplyr::group_by_at(data, covariates) %>%
    dplyr::mutate(stratum = as.integer(get_integer()))

  result$analysis_set <- grouped_table %>%
    dplyr::ungroup()

  result$strata_table <- grouped_table %>%
    dplyr::summarize(stratum = dplyr::first(stratum),
                     size = dplyr::n())

  result$issue_table <- make_issue_table(result$analysis_set, treat)

  return(result)
}

#' Warn if continuous
#'
#' Throws an error if a column is continuous
#'
#' Not meant to be called externally. Only categorical or binary covariates
#' should be used to manually stratify a data set.  However, it's hard to tell
#' for sure if something is continuous or just discrete with real-numbered
#' values. Returns without throwing an error if the column is a factor, but
#' throws an error or warning if the column has many distinct values.
#'
#' @param column vector or factor column from a \code{data.frame}
#' @param name name of the input column
#' @param force, a boolean. If true, warn but do not stop
#' @param n, the number of rows in the data set
#' @return Does not return anything
warn_if_continuous <- function(column, name, force, n){
  if (is.factor(column)){
    return() # assume all factors are discrete
  } else {
    values <- length(unique(column))
    if (values > min(c(15, 0.3 * n))){
      if ( force == FALSE ){
        stop(paste("There are ", values,
                   " distinct values for ", name,
                   ". Is it continuous?", sep = ""))
      } else {
        warning(paste("There are ", values,
                      " distinct values for ", name,
                      ". Is it continuous?", sep = ""))
      }
    }
    return()
  }
}

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
#' @param outcome string giving the name of column with outcome information
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
auto_stratify <- function(data, treat, outcome, prog_formula = NULL,
                          prog_scores = NULL, size = 2500,
                          held_frac = 0.1, held_sample = NULL) {

  result <- new_auto_strata(analysis_set = NULL,
                           treat = treat,
                           call = match.call(),
                           issue_table = NULL,
                           strata_table = NULL,
                           outcome = outcome,
                           prog_scores = NULL,
                           prog_model = NULL,
                           pilot_set = NULL)

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
    prog_build <- build_prog_model(data, treat,
                                   prog_formula, held_frac,
                                   held_sample)
    prog_model <- prog_build$m
    prog_scores <- tryCatch(predict(prog_model, prog_build$a_set,
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

    result$prog_model <- prog_model
    result$pilot_set <- prog_build$m_set
    result$analysis_set <- prog_build$a_set
  }

  print("Generating strata assignments based on prognostic score.")

  # Create strata from prognostic score quantiles
  n_bins <- ceiling(dim(result$analysis_set)[1] / size)
  qcut <- Hmisc::cut2(prog_scores, g = n_bins)
  result$strata_table <- data.frame(qcut) %>%
    dplyr::mutate(stratum = as.integer(qcut), quantile_bin = qcut) %>%
    dplyr::group_by(quantile_bin) %>%
    dplyr::summarise(size = dplyr::n(), stratum = dplyr::first(stratum)) %>%
    dplyr::arrange(stratum) %>%
    dplyr::select(stratum, quantile_bin, size)
  result$analysis_set$stratum <- as.integer(qcut)

  # package and resturn result
  result$prog_scores <- prog_scores

  print("Completing strata diagnostics.")
  result$issue_table <- make_issue_table(result$analysis_set, treat)

  return(result)
}

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
    print("Using user-specified set for prognostic score modeling.")
    pilot_set <- held_sample
    analysis_set <- data

  } else {
    print("Constructing a model set via subsampling.")
    # otherwise, construct a model sample
    # Adds an id column and removes it
    data$join_id_57674 <- 1:nrow(data)
    pilot_set <- data %>% dplyr::filter_(paste(treat, "==", 0)) %>%
      dplyr::sample_frac(held_frac, replace = FALSE)
    analysis_set <- dplyr::anti_join(data, pilot_set,
                                     by = "join_id_57674") %>%
      dplyr::select(-join_id_57674)
    pilot_set$join_id_57674 <- NULL
  }
  print(paste("Fitting prognostic model:",
              Reduce(paste, deparse(prog_formula))))

  model <- glm(prog_formula, data = pilot_set, family = "binomial")

  return(list(m = model, m_set = pilot_set, a_set = analysis_set))
}


#----------------------------------------------------------
### GENERAL HELPER FUNCTIONS
#----------------------------------------------------------

#' Make Issue Table
#' 
#' Not meant to be called externally. Produce table of the number of treated and control individuals in each stratum. Also checks for potential problems with treat/control ratio or stratum size which might result in slow or poor quality matching.
#' 
#' @param a_set \code{data.frame} with observations as rows, features as columns.  This should be the analysis set from the recently stratified data.
#' @param treat string name of treatment column
#' @return Returns a 3 by [number of strata] dataframe with Treat, Control, Total, Control Proportion, and Potential Issues
#' @export
make_issue_table <- function(a_set, treat){
  names(a_set)[names(a_set) == treat] <- "treat"
  df <- a_set %>%
    dplyr::group_by(stratum) %>%
    dplyr::summarize(Treated = sum(treat),
                     Control = sum(1 - treat),
                     Total = dplyr::n()) %>%
    dplyr::mutate(Control_Proportion = Control / Total)

  colnames(df) <- c("Stratum", "Treat",
                    "Control", "Total",
                    "Control_Proportion")
  df$Potential_Issues <- apply(df, 1, get_issues)

  return(df)
}


#' Get Issues
#'
#' Helper for make_issue_table to return issues string.  Given a row which
#' summarizes the Treat, Control, Total, and Control_Proportion of a stratum,
#' return a string of potential issues with the stratum.
#'
#' @param row a row of the data.frame produced in make_issue_table
#' @return Returns a string of potential issues ("none" if everything is fine)
get_issues <- function(row){
  row <- as.numeric(row[4:5])

  # set parameters
  CONTROL_MIN <- 0.2
  CONTROL_MAX <- 0.8
  SIZE_MIN <- 75
  SIZE_MAX <- 4000

  issues <- c(
    if (row[1] > SIZE_MAX) "Too many samples",
    if (row[1] < SIZE_MIN) "Too few samples",
    if (row[2] > CONTROL_MAX) "Not enough treated samples",
    if (row[2] < CONTROL_MIN) "Not enough control samples"
  )

  if (is.null(issues)) {
    issues <- c("none")
  }

  return(paste(issues, collapse = "; "))
}
