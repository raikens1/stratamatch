#----------------------------------------------------------
### CONTAINS: 
# split_pilot_set and it's helper functions
#----------------------------------------------------------
#----------------------------------------------------------
### SPLIT_PILOT_SET
#----------------------------------------------------------


#' Split data into pilot and analysis sets
#'
#' Given a data set and some parameters about how to split the data, this
#' function partitions the data accordingly and returns the partitioned data as
#' a list containing the \code{analysis_set} and \code{pilot_set}.
#'
#' @inheritParams auto_stratify
#'
#' @return a list with analaysis_set and pilot_set
#' @export
#' @examples
#'   dat <- make_sample_data()
#'   splt <- split_pilot_set(dat, "treat", 0.2)
#'   # can be passed into auto_stratify if desired
#'   a.strat <- auto_stratify(splt$analysis_set, "treat", outcome ~ X1,
#'    pilot_sample = splt$pilot_set)
split_pilot_set <- function(data, treat,
                            pilot_fraction = 0.1,
                            pilot_size = NULL,
                            pilot_sample = NULL,
                            group_by_covariates = NULL){
  
  # since this function can be called externally, need to check standard inputs
  check_base_inputs_auto_stratify(data, treat, outcome = NULL)
  
  check_pilot_set_options(pilot_sample, pilot_fraction, pilot_size,
                          group_by_covariates, data)
  
  # if input data is grouped, all sorts of strange things happen
  data <- data %>% dplyr::ungroup()
  
  pilot_set <- NULL
  
  n_c <- sum(data[[treat]] == 0)
  
  # if pilot_sample is specified, use that to build score
  if (!is.null(pilot_sample)){
    message("Using user-specified set for prognostic score modeling.")
    pilot_set <- pilot_sample
    analysis_set <- data
    
  } else {     # otherwise, construct a pilot set
    if(!is.null(pilot_size)){
      message(paste0("Constructing a pilot set by subsampling approximately ",
                     pilot_size, " controls."))
      pilot_fraction <- pilot_size / n_c
      
      if(pilot_fraction >= 1) {
        stop("Requested pilot size outnumbers control sample") 
      }
      if(pilot_fraction >= 0.7){
        warning("Requested pilot size requires more than 70% of all controls")
      }
      
    } else{
      message(paste0("Constructing a pilot set by subsampling ",
                     pilot_fraction*100, "% of controls."))
    }
    
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
#' @keywords internal
check_pilot_set_options <- function(pilot_sample, pilot_fraction, pilot_size,
                                    group_by_covariates, data){
  n <- dim(data)[1]
  
  # check pilot sample
  if (!is.null(pilot_sample) & !is.data.frame(pilot_sample)){
    stop("pilot_sample must be a data.frame")
  }
  
  # check pilot size
  if(!is.null(pilot_size)){
    if (!is.numeric(pilot_size)) stop("pilot_size must be numeric")
    if (pilot_size <= 1) {
      stop("pilot_size must be greater than 1")
    } 
  }
  
  # check pilot fraction
  if (!is.numeric(pilot_fraction)) stop("pilot_fraction must be numeric")
  if (pilot_fraction >= 1 | pilot_fraction <= 0) {
    stop("pilot_fraction must be between 0 and 1")
  }
  
  # check group_by_covariates
  if (!is.null(group_by_covariates)){
    if (!is.character(group_by_covariates)) {
      stop("group_by_covariates should be a character vector")
    }
    if (!all(is.element(group_by_covariates, colnames(data)))){
      stop("All covariates in group_by_covariates must be columns of the data")
    }
    n <- dim(data)[1]
    
    # Check that all covariates are discrete
    for (i in 1:length(group_by_covariates)){
      tryCatch(warn_if_continuous(data[[group_by_covariates[i]]],
                                  group_by_covariates[i], TRUE, n), 
               warning = function(w) {
                 message("All covariates in group_by_covariates should be discrete")
                 warning(w)
               })
    }
  }
}