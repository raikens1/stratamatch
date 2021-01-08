#---------------------------------------------------------- 
## CONTAINS: Methods for the strata objects for the generics `is`, `print`,
## `summary`, and `plot`
#----------------------------------------------------------

#----------------------------------------------------------
### INHERITANCE
#----------------------------------------------------------

#' Checks \code{strata} class
#'
#' Checks if the target object is a \code{strata} object.
#'
#' @param object any R object
#' @return Returns \code{TRUE} if its argument has \code{strata} among its
#'   classes and \code{FALSE} otherwise.
#' @export
#' @examples
#' dat <- make_sample_data()
#' m.strat <- manual_stratify(dat, treat ~ C1)
#' is.strata(m.strat) # returns TRUE
is.strata <- function(object) {
  inherits(object, "strata")
}

#' Checks \code{auto_strata} class
#'
#' Checks if the target object is an \code{auto_strata} object.
#'
#' @param object any R object
#' @return Returns \code{TRUE} if its argument has \code{auto_strata} among its
#'   classes and \code{FALSE} otherwise.
#' @export
#' @examples
#' dat <- make_sample_data()
#' a.strat <- auto_stratify(dat, "treat", outcome ~ X1 + X2)
#' is.auto_strata(a.strat) # returns TRUE
is.auto_strata <- function(object) {
  inherits(object, "auto_strata")
}


#' Checks \code{manual_strata} class
#'
#' Checks if the target object is a \code{manual_strata} object.
#'
#' @param object any R object
#' @return Returns \code{TRUE} if its argument has \code{manual_strata} among
#'   its classes and \code{FALSE} otherwise.
#' @export
#' @examples
#' dat <- make_sample_data()
#' m.strat <- manual_stratify(dat, treat ~ C1)
#' is.manual_strata(m.strat) # returns TRUE
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
#' @examples
#' dat <- make_sample_data()
#' a.strat <- auto_stratify(dat, "treat", outcome ~ X1 + X2)
#' print(a.strat) # prints information about a.strat
print.auto_strata <- function(x, ...) {
  writeLines("auto_strata object from package stratamatch.\n")

  writeLines("Function call:")
  print(x$call)

  writeLines(paste(
    "\nAnalysis set dimensions:",
    dim(x$analysis_set)[1], "X",
    dim(x$analysis_set)[2]
  ))

  if (!is.null(x$pilot_set)) {
    writeLines(paste(
      "\nPilot set dimensions:",
      dim(x$pilot_set)[1], "X",
      dim(x$pilot_set)[2]
    ))
  }
  if (!is.null(x$prognostic_model)) {
    writeLines("\nPrognostic Score Formula:")
    print(formula(x$prognostic_model))
  } else {
    writeLines("\nPrognostic Scores prespecified.")
  }
}

#' Print Manual Strata
#'
#' Print method for \code{manual_strata} object
#'
#' @param x, a \code{manual_strata} object
#' @param ... other arguments
#' @export
#' @examples
#' dat <- make_sample_data()
#' m.strat <- manual_stratify(dat, treat ~ C1)
#' print(m.strat) # prints information about m.strat
print.manual_strata <- function(x, ...) {
  writeLines("manual_strata object from package stratamatch.\n")

  writeLines("Function call:")
  print(x$call)

  writeLines(paste(
    "\nAnalysis set dimensions:",
    dim(x$analysis_set)[1], "X",
    dim(x$analysis_set)[2]
  ))
}

#----------------------------------------------------------
### SUMMARY
#----------------------------------------------------------

#' Summary for strata object
#'
#' Summarize number and sizes of strata in a \code{strata} object.  Also prints
#' number of strata with potential issues.
#'
#' For more information, access the issue table for your strata object with
#' \code{mystrata$issue_table}.
#'
#' @param object a \code{strata} object
#' @param ... other arguments
#' @export
#' @examples
#' dat <- make_sample_data()
#' m.strat <- manual_stratify(dat, treat ~ C1)
#' summary(m.strat) # Summarizes strata in m.strat
summary.strata <- function(object, ...) {
  writeLines(paste(
    "Number of strata:", dim(object$issue_table)[1],
    "\n\n\tMin size:", min(object$issue_table$Total),
    "\tMax size:", max(object$issue_table$Total)
  ))

  writeLines(paste(
    "\nStrata with Potential Issues:",
    sum(object$issue_table$Potential_Issues != "none")
  ))
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
#' produces a histogram of propensity scores within a stratum \item \code{"AC"}
#' - produces a Assignment-Control plot of individuals within a stratum  (not
#' supported for \code{manual strata} objects) \item \code{"residual"} -
#' produces a residual plot for the prognostic model (not supported for
#' \code{manual strata} objects)}
#'
#' @param x a \code{strata} object returned by \code{\link{auto_stratify}} or
#'   \code{\link{manual_stratify}}
#' @param type string giving the plot type (default = \code{"SR"}).  Other
#'   options are \code{"hist"}, \code{"AC"} and \code{"residual"}
#' @param label ignored unless \code{type = "SR"}. If \code{TRUE}, a clickable
#'   plot is produced. The user may click on any number of strata and press
#'   finish to have those strata labeled.  Note: uses \code{\link{identify}},
#'   which may not be supported on some devices
#' @param stratum ignored unless \code{type = "hist"} or \code{type = "AC"}. A
#'   number specifying which stratum to plot.
#' @param strata_lines default = \code{TRUE}. Ignored unless \code{type = "AC"}.
#'   If TRUE, lines on the plot indicate strata cut points.
#' @param jitter_prognosis ignored unless \code{type = "AC"}.  Amount of uniform
#'   random noise to add to prognostic scores in plot.
#' @param jitter_propensity ignored unless \code{type = "AC"}.  Amount of
#'   uniform random noise to add to propensity scores in plot.
#' @param propensity ignored unless \code{type = "hist"} or \code{type = "AC"}.
#'   Specifies propensity score information for plots where this is required.
#'   Accepts either a vector of propensity scores, a \code{glm} model for
#'   propensity scores, or a formula for fitting a propensity score model.
#' @param ... other arguments
#' @seealso Aikens et al. (preprint) \url{https://arxiv.org/abs/1908.09077} .
#'   Section 3.2 for an explaination of Assignment-Control plots
#' @export
#' @examples
#' dat <- make_sample_data()
#' a.strat <- auto_stratify(dat, "treat", outcome ~ X1 + X2)
#' plot(a.strat) # makes size-ratio scatter plot
#' plot(a.strat, type = "hist", propensity = treat ~ X1, stratum = 1)
#' plot(a.strat, type = "AC", propensity = treat ~ X1, stratum = 1)
#' plot(a.strat, type = "residual")
plot.strata <- function(x, type = "SR", label = FALSE, stratum = "all",
                        strata_lines = TRUE, jitter_prognosis,
                        jitter_propensity, propensity, ...) {
  if (type == "SR") {
    make_SR_plot(x, label)
  } else if (type == "hist") {
    make_hist_plot(x, propensity, stratum)
  } else if (type == "AC") {
    make_ac_plot(
      x, propensity, stratum, strata_lines,
      jitter_prognosis, jitter_propensity
    )
  } else if (type == "residual") {
    make_resid_plot(x)
  } else {
    stop("Not a recognized plot type.")
  }
}

#' Make Size-Ratio plot
#'
#' Not meant to be called externally.  Helper plot function for \code{strata}.
#' Produces a scatter plot of strata by size and control proportion.
#'
#' @inheritParams plot.strata
#' @keywords internal
make_SR_plot <- function(x, label) {
  issue_table <- x$issue_table

  # set parameters
  CONTROL_MIN <- 0.2
  CONTROL_MAX <- 0.8
  SIZE_MIN <- 75
  SIZE_MAX <- 4000

  xmax <- max(issue_table$Total, SIZE_MAX * 1.05)

  plot(c(0, xmax), c(0, 1), type = "n", xlab = "Stratum Size", ylab = "Fraction Control Observations")
  rect(0, 0, xmax, CONTROL_MIN, col = rgb(1, 1, 0, 0.35), border = NA)
  rect(0, CONTROL_MAX, xmax, 1, col = rgb(1, 1, 0, 0.35), border = NA)
  rect(0, 0, SIZE_MIN, 1, col = rgb(1, 0, 0, 0.35), border = NA)
  rect(SIZE_MAX, 0, xmax, 1, col = rgb(1, 0.6, 0, 0.35), border = NA)
  points(x = issue_table$Total, y = issue_table$Control_Proportion, pch = 16)
  legend("bottomright",
         title="Potential Issues",
         legend = c("too few samples", "too many samples", "treat:control imbalance"),
         fill = c(rgb(1, 0, 0, 0.35), rgb(1, 0.6, 0, 0.35), rgb(1, 1, 0, 0.35)),
         box.lty = 0
  )


  if (label == TRUE) {
    identify(
      x = issue_table$Total, y = issue_table$Control_Proportion,
      labels = issue_table$Stratum, offset = 0.5
    )
  }
}

#' Make histogram plot
#'
#' Not meant to be called externally.  Helper plot function for \code{strata}
#' object with \code{type = "hist"}. Produces a histogram of propensity scores
#' within a stratum
#'
#' @inheritParams plot.strata
#' @param strat the number code of the strata to be plotted.  If "all", plots
#'   all strata
#' @keywords internal
make_hist_plot <- function(x, propensity, strat) {
  a_set <- x$analysis_set

  prop_scores <- get_prop_scores(propensity, a_set, x$treat)

  plt_data <- a_set %>%
    dplyr::mutate(prop_score = prop_scores)

  if (strat == "all") {
    title <- "Propensity scores across all strata"
  } else {
    if (!is.element(strat, unique(a_set$stratum))) {
      stop("Stratum number does not exist in analysis set")
    }
    plt_data <- plt_data %>%
      dplyr::filter(.data$stratum == strat)
    title <- paste("Propensity scores in stratum", strat)
  }

  ht <- plt_data[(plt_data[[x$treat]] == 1), ]$prop_score
  hc <- plt_data[(plt_data[[x$treat]] == 0), ]$prop_score

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
  hist(hc,
    breaks = nbreaks,
    col = rgb(0, 0, 1, 0.5), xlim = c(xmin, xmax), ylim = c(0, ymax),
    main = title,
    xlab = "Propensity Score"
  )
  hist(ht, breaks = nbreaks, col = rgb(1, 0, 0, 0.5), add = TRUE)
  legend("topright",
    legend = c("treated", "control"),
    fill = c(rgb(1, 0, 0, 0.5), rgb(0, 0, 1, 0.5))
  )
}

#' Make Assignment-Control plot
#'
#' Not meant to be called externally.  Helper plot function for \code{strata}
#' object with \code{type = "AC"}. Produces a Assignment-Control plot of stratum
#' \code{s}
#'
#' @inheritParams plot.strata
#' @param strat the number code of the stratum to be plotted. If "all", plots
#'   all strata.
#' @seealso Aikens et al. (preprint) \url{https://arxiv.org/abs/1908.09077} .
#'   Section 3.2 for an explaination of Assignment-Control plots
#' @keywords internal
make_ac_plot <- function(x, propensity, strat, strata_lines,
                         jitter_prognosis, jitter_propensity) {
  if (!is.auto_strata(x)) {
    stop("Cannot make Assignment-Control plots on manually stratified data.")
  }

  a_set <- x$analysis_set

  prop_scores <- get_prop_scores(propensity, a_set, x$treat)

  plt_data <- a_set %>%
    dplyr::mutate(
      prop_score = prop_scores,
      prog_score = x$prognostic_scores
    )

  if (strat == "all") {
    title <- "Assignment-Control plot for all strata"
  } else {
    if (!is.element(strat, unique(a_set$stratum))) {
      stop("Stratum number does not exist in analysis set")
    }
    plt_data <- plt_data %>%
      dplyr::filter(.data$stratum == strat)
    title <- paste("Assignment-Control plot for stratum", strat)
  }

  # if jitter arguments supplied, add jitter.
  if (!missing(jitter_propensity)) {
    plt_data <- dplyr::mutate(plt_data,
      prop_score = jitter(.data$prop_score,
        amount = jitter_propensity
      )
    )
  }
  if (!missing(jitter_prognosis)) {
    plt_data <- dplyr::mutate(plt_data,
      prog_score = jitter(.data$prog_score,
        amount = jitter_prognosis
      )
    )
  }

  plt_data$color <- ifelse(plt_data[[x$treat]] == 1, "red", "blue")

  propscore_span <- max(plt_data$prop_score) - min(plt_data$prop_score)
  progscore_span <- max(plt_data$prog_score) - min(plt_data$prog_score)

  plot(plt_data$prop_score, plt_data$prog_score,
    col = plt_data$color,
    main = title,
    xlab = "Estimated propensity score",
    ylab = "Estimated prognostic score",
    xlim = range(plt_data$prop_score) + c(-0.1, 0.1) * propscore_span,
    ylim = range(plt_data$prog_score) + c(-0.1, 0.1) * progscore_span
  )
  if (strata_lines) {
    abline(h = extract_cut_points(x), col = "grey")
    legend("topright",
      legend = "strata cut points", col = "grey", lty = 1,
      box.lty = 0, bg = "transparent"
    )
  }
  legend("topleft",
    legend = c("treated", "control"), fill = c("red", "blue"),
    box.lty = 0
  )
}

#' Make Residual Plot
#'
#' Not yet implemented.  Not meant to be called externally. Helper plot function
#' for \code{strata} object with \code{type = "residual"}. Produces the
#' diagnostic plots for the prognostic score model
#'
#' @inheritParams plot.strata
#' @keywords internal
make_resid_plot <- function(x) {
  if (!is.auto_strata(x)) {
    stop("Prognostic score residual plots are only valid for auto-stratified data.")
  } else {
    if (is.null(x$prognostic_model)) {
      stop("Cannot make prognostic model residual plots since prognostic scores were provided.")
    } else {
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
#' @keywords internal
get_prop_scores <- function(propensity, data, treat) {
  # if it is a vector of propensity scores, check and return it
  if (is.numeric(propensity)) {
    if (length(propensity) != dim(data)[1]) {
      stop("propensity scores must be the same length as the data")
    }
    return(propensity)
  }

  # if it is a formula
  if (inherits(propensity, "formula")) {
    check_prop_formula(propensity, data, treat)
    prop_model <- glm(propensity, data, family = "binomial")
    return(predict(prop_model, type = "response"))
  }

  # if it is a model for propensity, predict on data
  withCallingHandlers(
    {
      prop_scores <- predict(propensity,
        newdata = data,
        type = "response"
      )
    },
    error = function(c) {
      stop("propensity type not recognized")
    }
  )
  return(prop_scores)
}

#' Check Propensity Formula
#'
#' @inheritParams get_prop_scores
#' @param prop_formula a formula
#'
#' @return nothing
#' @keywords internal
check_prop_formula <- function(prop_formula, data, treat) {
  if (!(treat == all.vars(prop_formula)[1])) {
    stop("propensity formula must model treatment assignment")
  }
}

#----------------------------------------------------------
### NEW METHODS
#----------------------------------------------------------

#' Extract cutoffs between strata
#'
#' By default, returns only the internal cut points.  Cutoffs at 0 and 1 are
#' implied.
#'
#' @param x an autostrata object
#'
#' @return a vector of the score values delineating cutoffs between strata
#' @export
#'
#' @examples
#' dat <- make_sample_data()
#' a.strat <- auto_stratify(dat, "treat", outcome ~ X1 + X2)
#' cutoffs <- extract_cut_points(a.strat)
extract_cut_points <- function(x) {
  UseMethod("extract_cut_points")
}

#' Extract cutoffs between strata
#'
#' @inheritParams extract_cut_points
#'
#' @return a vector of the score values delineating cutoffs between strata
#' @export
extract_cut_points.auto_strata <- function(x) {
  bin_str <- x$strata_table$quantile_bin

  bin_str <- sub(".*,", "", bin_str)
  bin_str <- substr(bin_str, 1, nchar(bin_str) - 1)

  cuts <- as.numeric(bin_str)

  if (length(cuts) <= 1) {
    warning("Only one stratum.  Returning NA.")
    return(NA)
  } else {
    return(cuts[-length(cuts)])
  }
}
