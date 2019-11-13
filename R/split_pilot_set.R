#----------------------------------------------------------
### CONTAINS: 
# split_pilot_set and it's helper functions
#----------------------------------------------------------
#----------------------------------------------------------
### SPLIT_PILOT_SET
#----------------------------------------------------------


#' Split data into pilot and analysis sets
#'
#' Exported for the convenience of the user, although in practice this process
#' is almost always done using \code{\link{auto_stratify}}.  Given a data set
#' and some parameters about how to split the data, this function partitions the
#' data accordingly and returns the partitioned data as a list containing the
#' \code{analysis_set} and \code{pilot_set}.
#'
#' @inheritParams auto_stratify
#' @param group_by_covariates character vector giving the names of covariates to
#'   be grouped by (optional). If specified, the pilot set will be sampled in a
#'   stratified manner, so that the composition of the pilot set reflects the
#'   composition of the whole data set in terms of these covariates.  The
#'   specified covariates must be categorical.
#'
#' @return a list with analaysis_set and pilot_set
#' @export
#' @examples
#'   dat <- make_sample_data()
#'   splt <- split_pilot_set(dat, "treat", 0.2)
#'   a.strat <- auto_stratify(splt$analysis_set, "treat", outcome ~ X1, pilot_sample = splt$pilot_set)
split_pilot_set <- function(data, treat,
                            pilot_fraction = 0.1,
                            pilot_sample = NULL,
                            group_by_covariates = NULL){
  check_pilot_set_options(pilot_sample, pilot_fraction,
                          group_by_covariates, data)
  
  # since this function can be called externally now, need to check standard inputs
  check_base_inputs_auto_stratify(data, treat, outcome = NULL)
  
  pilot_set <- NULL
  
  # if pilot_sample is specified use that to build score
  if (!is.null(pilot_sample)){
    message("Using user-specified set for prognostic score modeling.")
    pilot_set <- pilot_sample
    analysis_set <- data
    
  } else {
    # otherwise, construct a pilot set
    message(paste0("Constructing a pilot set by subsampling ", pilot_fraction*100, "% of controls."))
    
    # Adds an id column
    data$stratamatch_id <- 1:nrow(data)
    
    # if group_by_covariates are provided, sample the pilot set
    # while stratifying for group_by_covariates
    if (!is.null(group_by_covariates)){
      message("Subsampling while balancing on:")
      message(group_by_covariates)
      data <- dplyr::group_by_at(data, group_by_covariates)
    }
    
    pilot_set <- data[ (data[[treat]] == 0),] %>%
      dplyr::sample_frac(pilot_fraction, replace = FALSE) %>%
      dplyr::ungroup()
    
    data <- data %>% dplyr::ungroup()
    
    analysis_set <- dplyr::anti_join(data, pilot_set,
                                     by = "stratamatch_id") %>%
      dplyr::select(-.data$stratamatch_id)
    
    pilot_set$stratamatch_id <- NULL
  }
  return(list(analysis_set = analysis_set, pilot_set = pilot_set))
}

#----------------------------------------------------------
### Input Checkers
#----------------------------------------------------------

#' Check Pilot set options
#'
#' @inheritParams split_pilot_set
#' 
#' @return nothing
check_pilot_set_options <- function(pilot_sample, pilot_fraction,
                                    group_by_covariates, data){
  if (!is.numeric(pilot_fraction)) stop("pilot_fraction must be numeric")
  if (pilot_fraction >= 1 | pilot_fraction <= 0) stop("pilot_fraction must be between 0 and 1")
  
  if (!is.null(pilot_sample) & !is.data.frame(pilot_sample)){
    stop("pilot_sample must be a data.frame")
  }
  
  if (!is.null(group_by_covariates)){
    if (!is.character(group_by_covariates)) {
      stop("group_by_covariates should be a character vector")
    }
    if (!all(is.element(group_by_covariates, colnames(data)))){
      stop("All covariates in group_by_covariates must be columns of the data")
    }
  }
}