#---------------------------------------------------------- 
## CONTAINS: Methods for the strata objects for the generics `is`, `print`,
## `summarize`, and `plot`
#----------------------------------------------------------

#----------------------------------------------------------
### INHERITANCE
#----------------------------------------------------------

#' Checks \code{strata} class
#' 
#' Checks if the target object is a \code{strata} object.
#' 
#' @param object any R object
#' @return Returns \code{TRUE} if its argument has class "strata" among its classes and
#' \code{FALSE} otherwise.
#' @export
is.strata <- function(object) {
  inherits(object, "strata")
}

#' Checks \code{auto_strata} class
#' 
#' Checks if the target object is an \code{auto_strata} object.
#' 
#' @param object any R object
#' @return Returns \code{TRUE} if its argument has class "auto_strata" among its classes and
#' \code{FALSE} otherwise.
#' @export
is.auto_strata <- function(object) {
  inherits(object, "auto_strata")
}


#' Checks \code{manual_strata} class
#' 
#' Checks if the target object is a \code{manual_strata} object.
#' 
#' @param object any R object
#' @return Returns \code{TRUE} if its argument has class "manual_strata" among its classes and
#' \code{FALSE} otherwise.
#' @export
is.manual_strata <- function(object) {
  inherits(object, "manual_strata")
}


#----------------------------------------------------------
### PRINT METHODS
#----------------------------------------------------------

#' Print Auto Strata
#' 
#' Print method for \code{auto_strata} object
#' 
#' @param x, an \code{auto_strata} object
#' @param ... other arguments
#' @export
print.auto_strata <- function(x, ...) {
  writeLines("auto_strata object from package stratamatch.\n")

  writeLines("Function call:")
  print(x$call)

  writeLines(paste("\nAnalysis set dimensions:",
                   dim(x$analysis_set)[1], "X",
                   dim(x$analysis_set)[2]))

  if (!is.null(x$pilot_set)) {
    writeLines(paste("\nPilot set dimensions:",
                     dim(x$pilot_set)[1], "X",
                     dim(x$pilot_set)[2]))
  }
  if (!is.null(x$prognostic_model)){
    writeLines("\nPrognostic Score Formula:")
    print(x$prognostic_model$formula)
    
  } else {
    writeLines("\nPrognostic Scores prespecified.")
  }

  writeLines(paste("\nNumber of strata:", dim(x$issue_table)[1],
                   "\n\n\tMin size:", min(x$issue_table$Total),
                   "\tMax size:", max(x$issue_table$Total)))
}

#' Print Manual Strata
#' 
#' Print method for \code{manual_strata} object
#' 
#' @param x, a \code{manual_strata} object
#' @param ... other arguments
#' @export
print.manual_strata <- function(x, ...) {
  writeLines("manual_strata object from package stratamatch.\n")

  writeLines("Function call:")
  print(x$call)

  writeLines(paste("\nAnalysis set dimensions:",
                   dim(x$analysis_set)[1], "X",
                   dim(x$analysis_set)[2]))

  writeLines(paste("\nNumber of strata:", dim(x$issue_table)[1],
                   "\n\n\tMin size:", min(x$issue_table$Total),
                   "\tMax size:", max(x$issue_table$Total)))
}

#----------------------------------------------------------
### PLOT METHODS
#----------------------------------------------------------

#' Plot method for \code{strata} object
#'
#' Generates diagnostic plots for the product of a stratification by
#' \code{\link{auto_stratify}} or \code{\link{manual_stratify}}.  There are
#' three plot types: \enumerate{ \item \code{"SR"} (default) - produces a
#' scatter plot of strata by size and treat:control ratio \item \code{"hist"} -
#' produces a histogram of propensity scores within a stratum \item \code{"FM"}
#' - produces a Fisher-Mill plot of individuals within a stratum  (not supported
#' for \code{manual strata} objects) \item \code{"residual"} - produces a
#' residual plot for the prognostic model (not supported for \code{manual
#' strata} objects)}
#'
#' @param x a \code{strata} object returned by \code{\link{auto_stratify}} or
#'   \code{\link{manual_stratify}}
#' @param type string giving the plot type (default = \code{"SR"}).  Other
#'   options are \code{"hist"}, \code{"FM"} and \code{"residual"}
#' @param label ignored unless \code{type = "SR"}. If \code{TRUE}, a
#'   clickable plot is produced. The user may click on any number of strata and
#'   press "finish" to have those strata labeled.  Note: uses \code{\link{identify}},
#'   which may not be supported on some devices
#' @param propensity ignored unless \code{type = "hist} or \code{type = "FM}.
#'   Specifies propensity score information for plots where this is required.
#'   Accepts either a vector of propensity scores, a \code{glm} model for
#'   propensity scores, or a formula for fitting a propensity score model.
#' @param stratum ignored unless \code{type = "hist} or \code{type = "FM}. A
#'   number specifying which stratum to plot.
#' @param ... other arguments
#' @seealso Aikens et al. (preprint) \url{https://arxiv.org/abs/1908.09077} .
#'   Section 3.2 for an explaination of Fisher-Mill plots
#' @export
plot.strata <- function(x, type = "SR", label = FALSE,
                        propensity, stratum, ...){
  if (type == "SR") make_SR_plot(x, label)
  else if (type == "hist") make_hist_plot(x, propensity, stratum)
  else if (type == "FM") make_fm_plot(x, propensity, stratum)
  else if (type == "residual") make_resid_plot(x)
  else {
    stop("Not a recognized plot type.")
  }
}

#' Make Size-Ratio plot
#'
#' Not meant to be called externally.  Helper plot function for \code{strata}.
#' Produces a scatter plot of strata by size and control proportion.
#'
#' @inheritParams plot.strata
make_SR_plot <- function(x, label) {
  issue_table <- x$issue_table

  # set parameters
  CONTROL_MIN <- 0.2
  CONTROL_MAX <- 0.8
  SIZE_MIN <- 75
  SIZE_MAX <- 4000

  xmax <- max(issue_table$Total, SIZE_MAX * 1.05)

  plot(x = issue_table$Total, y = issue_table$Control_Proportion, pch = 16,
       xlim = c(0, xmax), ylim = c(0, 1),
       xlab = "Stratum Size", ylab = "Fraction Control Observations")
  rect(0, 0, xmax, CONTROL_MIN, col = rgb(1, 1, 0, 0.25), border = NA)
  rect(0, CONTROL_MAX, xmax, 1, col = rgb(1, 1, 0, 0.25), border = NA)
  rect(0, 0, SIZE_MIN, 1, col = rgb(1, 0, 0, 0.25), border = NA)
  rect(SIZE_MAX, 0, xmax, 1, col = rgb(1, 0, 0, 0.25), border = NA)

  if (label == TRUE){
    identify(x = issue_table$Total, y = issue_table$Control_Proportion,
             labels = issue_table$Stratum, offset = 0.5)
  }
}

#' Make histogram plot
#'
#' Not meant to be called externally.  Helper plot function for \code{strata}
#' object with type = "hist". Produces a histogram of propensity scores within a
#' stratum
#'
#' @inheritParams plot.strata
#' @param s the number code of the strata to be plotted
make_hist_plot <- function(x, propensity, s){
  a_set <- x$analysis_set

  if (!is.element(s, unique(a_set$stratum))){
    stop("Stratum number does not exist in analysis set")
  }
  
  prop_scores = get_prop_scores(propensity, a_set, x$treat)

  plt_data <- a_set %>%
    dplyr::mutate(prop_score = prop_scores) %>%
    dplyr::filter(.data$stratum == s)

  ht <- plt_data[ (plt_data[[x$treat]] == 1),]$prop_score
  hc <- plt_data[ (plt_data[[x$treat]] == 0),]$prop_score

  # workaround to get plot area correct
  # make separate histograms, then use the info in the histogram objects
  # to determine x and y axis limits
  histt <- hist(ht, plot = FALSE)
  histc <- hist(hc, plot = FALSE)
  ymax <- max(histt$counts, histc$counts)
  xmin <- min(histt$breaks, histc$breaks)
  xmax <- max(histt$breaks, histc$breaks)

  nbreaks <- max(length(histt$breaks), length(histc$breaks))

  # plot final overlayed histogram
  hist(hc, breaks = nbreaks,
       col = rgb(0, 0, 1, 0.5), xlim = c(xmin, xmax), ylim = c(0, ymax),
       main = paste("Propensity scores in stratum", s),
       xlab = "Propensity Score")
  hist(ht, breaks = nbreaks, col = rgb(1, 0, 0, 0.5), add = TRUE)
  legend("topright", legend = c("treated", "control"),
         fill = c(rgb(1, 0, 0, 0.5), rgb(0, 0, 1, 0.5)))
}

#' Make Fisher-Mill plot
#'
#' Not meant to be called externally.  Helper plot function for \code{strata}
#' object with type = "FM". Produces a Fisher-Mill plot of stratum \code{s}
#'
#' @inheritParams plot.strata
#' @param s the number code of the strata to be plotted
#' @seealso Aikens et al. (preprint) \url{https://arxiv.org/abs/1908.09077} .
#'   Section 3.2 for an explaination of Fisher-Mill plots
make_fm_plot <- function(x, propensity, s){
  if (!is.auto_strata(x)){
    stop("Cannot make Fisher-Mill plots on manually stratified data.")
  }

  a_set <- x$analysis_set

  if (!is.element(s, unique(a_set$stratum))){
    stop("Stratum number does not exist in analysis set")
  }

  prop_scores = get_prop_scores(propensity, a_set, x$treat)
  
  plt_data <- a_set %>%
    dplyr::mutate(prop_score = prop_scores,
                  prog_score = x$prognostic_scores) %>%
    dplyr::filter(.data$stratum == s)

  plt_data$color <- ifelse(plt_data[[x$treat]] == 1, "red", "blue")
  
  propscore_span = max(plt_data$prop_score) - min(plt_data$prop_score)
  progscore_span = max(plt_data$prog_score) - min(plt_data$prog_score)

  plot(plt_data$prop_score, plt_data$prog_score, col = plt_data$color,
       main = paste("Fisher-Mill plot for stratum", s),
       xlab = "Estimated propensity score",
       ylab = "Estimated prognostic score",
       xlim = range(plt_data$prop_score) + c(-0.25, 0.25) * propscore_span,
       ylim = range(plt_data$prog_score) + c(-0.25, 0.25) * progscore_span)
  legend("topleft", legend = c("treated", "control"), fill = c("red", "blue"),
         box.lty = 0)
}

#' Make Residual Plot
#'
#' Not yet implemented.  Not meant to be called externally. Helper plot function
#' for \code{strata} object with type = "residual". Produces the diagnostic
#' plots for the prognostic score model
#'
#' @inheritParams plot.strata
make_resid_plot <- function(x){
  if (!is.auto_strata(x)){
    stop("Prognostic score residual plots are only valid for auto-stratified data.")
  } else {
    if (is.null(x$prognostic_model)){
      stop("Cannot make prognostic model residual plots since prognostic scores were provided.")
    } else{
      return(plot(x$prognostic_model))
    }
  }
}

#' Parse \code{propensity} input to obtain propensity scores
#'
#' the \code{propensity} input to \code{plot.strata} can be propensity scores, a
#' propensity model, or a formula for propensity score.  This function figures
#' out which type \code{propensity} is and returns the propensity scores.
#' Returns the propensity score on the response scale (rather than the linear
#' predictor), so the scores are the predited probabilities of treatment.
#'
#' @param propensity either a vector of propensity scores, a model for
#'   propensity, or a formula for propensity scores
#' @param data, the analysis set data within a stratum
#' @param treat, the name of the treatment assignment column
#'
#' @return vector of propensity scores
get_prop_scores <- function(propensity, data, treat){
  # if it is a vector of propensity scores, check and return it
  if (is.numeric(propensity)){
    if (length(propensity) != dim(data)[1]){
      stop("propensity scores must be the same length as the data")
    }
    return(propensity)
  }

  # if it is a formula
  if (inherits(propensity, "formula")){
    check_prop_formula(propensity, data, treat)
    prop_model <- glm(propensity, data, family = "binomial")
    return(predict(prop_model, type = "response"))
  }

  # if it is a model for propensity, predict on data
  prop_scores <- tryCatch(predict(propensity, 
                                  newdata = data,
                                  type = "response"),
                          error = function(c) {
                            stop("propensity type not recognized")
                            })
  return(prop_scores)
}

#' Check Propensity Formula
#'
#' @inheritParams get_prop_scores
#' @param prop_formula a formula
#' 
#' @return nothing
check_prop_formula <- function(prop_formula, data, treat){
  if (!all(is.element(all.vars(prop_formula), colnames(data)))) {
    stop("not all variables in propensity formula appear in data")
  }
  if (!(treat == all.vars(prop_formula)[1])) {
    stop("propensity formula must model treatment assignment")
  }
}
