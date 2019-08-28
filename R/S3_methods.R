#----------------------------------------------------------
### CONTAINS: 
# Methods for the strata objects for the generics `is`, `print`, `summarize`, and
# `plot`
#----------------------------------------------------------

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
  writeLines("auto_strata object from package big_match.\n")

  writeLines("Function call:")
  print(x$call)

  writeLines(paste("\nAnalysis set dimensions:",
                   dim(x$analysis_set)[1], "X",
                   dim(x$analysis_set)[2]))

  if (!is.null(x$prog_model)) {
    writeLines(paste("\nPilot set dimensions:",
                     dim(x$pilot_set)[1], "X",
                     dim(x$pilot_set)[2]))

    writeLines("\nPrognostic Score Formula:")
    print(x$prog_model$formula)

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
  writeLines("manual_strata object from package big_match.\n")

  writeLines("Function call:")
  print(x$call)

  writeLines(paste("\nAnalysis set dimensions:",
                   dim(x$analysis_set)[1], "X",
                   dim(x$analysis_set)[2]))

  writeLines(paste("\nNumber of strata:", dim(x$issue_table)[1],
                   "\n\n\tMin size:", min(x$issue_table$Total),
                   "\tMax size:", max(x$issue_table$Total)))

  if (dim(x$issue_table)[1] <= 15) {
    writeLines("\nStrata issue table:")
    print(x$issue_table)
  }
}

#----------------------------------------------------------
### SUMMARY METHODS
#----------------------------------------------------------
#' Summary method for strata object
#' 
#' @param object a \code{strata} object
#' @param ... other arguments
#' @export
summary.strata <- function(object, ...){
  strata_summary <- structure(list(call = object$call,
                                   issue_table = object$issue_table,
                                   sum_before = NULL),
                 class = "summary.strata") # necessary to make a new class?

  strata_summary$sum_before <- summarize_balance(object$analysis_set,
                                                 object$treat)

  return(strata_summary)
}

#' Calculate balance measures over a dataframe
#'
#' Still under development. Calculate mean of each covariate over Treat and
#' Control groups, and difference in means between groups.
#'
#' @param data, a \code{data.frame} from a \code{strata} object
#' @param treat, a string giving the name of the treatment assignment column in
#'   the dataframe
#' @return Returns a data.frame of balance measures
summarize_balance <- function(data, treat){

  # remove strata and outcome columns?

  names(data)[names(data) == treat] <- "treat"

  # TODO: rownames for data frames will be deprecated soon,
  # which means this will have to change.
  result <- data %>%
    dplyr::group_by(treat) %>%
    dplyr::summarize_all(mean) %>%
    t()

  colnames(result) <- c("Treat_Mean", "Contol_Mean")

  # TODO: Finish this.
  # May want to split into a treat dataframe and a control data frame

  return(result)
}

#----------------------------------------------------------
### PLOT METHODS
#----------------------------------------------------------

#' Make Scatter Plot
#'
#' Not meant to be called externally. Helper plot function for \code{strata}
#' object with type = "scatter." Generates a scatter plot of all strata by size
#' and treat:control ratio.  Highlights strata which may be problematic.
#'
#' @param issue_table the \code{issue_table} from a \code{strata} object
#' @param label, if true, label problematic strata.  Not recommended if there
#'   are many problematic strata.
#' @seealso \code{\link{make_issue_table}}
#' @return Returns the scatterplot
make_scatter_plot <- function(issue_table, label){

  # set parameters
  CONTROL_MIN <- 0.2
  CONTROL_MAX <- 0.8
  SIZE_MIN <- 75
  SIZE_MAX <- 4000

  # identify strata that are too large/small/imbalanced
  problem_strata <- issue_table %>% dplyr::filter(Potential_Issues != "none")

  xmax <- max(issue_table$Total, SIZE_MAX * 1.05)

  g <- ggplot2::ggplot(issue_table,
                       ggplot2::aes(x = Total, y = Control_Proportion)) +
    ggplot2::labs(x = "Stratum Size",
         y = "Fraction Control Observations") +
    ggplot2::ylim(c(0, 1)) + ggplot2::xlim(c(0, xmax)) +
    ggplot2::geom_rect(ggplot2::aes(ymin = 0, ymax = CONTROL_MIN,
                  xmin = 0, xmax = Inf),
              fill = "lightgoldenrod2", alpha = 0.1) +
    ggplot2::geom_rect(ggplot2::aes(ymin = CONTROL_MAX, ymax = 1,
                  xmin = 0, xmax = Inf),
              fill = "lightgoldenrod2", alpha = 0.1) +
    ggplot2::geom_rect(ggplot2::aes(ymin = 0, ymax = 1,
                  xmin = 0, xmax = SIZE_MIN),
              fill = ggplot2::alpha("firebrick1", 0.1)) +
    ggplot2::geom_rect(ggplot2::aes(ymin = 0, ymax = 1,
                  xmin = SIZE_MAX, xmax = Inf),
              fill = ggplot2::alpha("firebrick1", 0.1)) +
    ggplot2::geom_point() +
    ggplot2::geom_vline(xintercept = SIZE_MAX, colour = "firebrick") +
    ggplot2::geom_vline(xintercept = SIZE_MIN, colour = "firebrick") +
    ggplot2::geom_hline(yintercept = CONTROL_MAX, colour = "goldenrod1") +
    ggplot2::geom_hline(yintercept = CONTROL_MIN, colour = "goldenrod1") +
    ggplot2::geom_hline(yintercept = 0.5, colour = "black", linetype = 2) +
    ggplot2::theme(legend.position = "none")

  if (label){
    g <- g +
      ggrepel::geom_label_repel(data = problem_strata,
                                ggplot2::aes(Total,
                                             Control_Proportion,
                                             label = Stratum),
                                size = 2.5, color = "firebrick")
  }
  print(g)
}

#' Make histogram plot
#'
#' Not meant to be called externally.  Helper plot function for \code{strata}
#' object with type = "hist".  Produces a column plot of prognostic score means
#' across strata and a histogram of prognostic score, colored by strata.  Not
#' recommended if there are many strata.
#' 
#' @param auto_strata an \code{auto_strata} object
#' @return Returns an arrangement of the column plot and histogram.
make_hist_plot <- function(auto_strata){
  plotdata <- auto_strata$analysis_set
  plotdata$prog_scores <- auto_strata$prog_scores

  plot_summary <- plotdata %>%
    dplyr::group_by(stratum) %>%
    dplyr::summarize(prog_mean = mean(prog_scores))

  a <- ggplot2::ggplot(data = plot_summary,
                       ggplot2::aes(x = stratum, y = prog_mean,
                                    fill = as.factor(stratum))) +
    ggplot2::geom_col() +
    ggplot2::labs(y = "Mean Prognistic Score", x = "Stratum") +
    ggplot2::theme(legend.position = "none") +
    ggplot2::scale_fill_brewer(palette = "Blues")

  b <- ggplot2::ggplot(plotdata, ggplot2::aes(x = prog_scores,
                                              group = as.factor(stratum),
                            fill = as.factor(stratum))) +
    ggplot2::geom_histogram() +
    ggplot2::labs(y = "Prognostic Score", "Number of Observations") +
    ggplot2::scale_fill_brewer(name = "Stratum", palette = "Blues")

  return(ggpubr::ggarrange(a, b, ncol = 2, widths = c(1, 1.5)))
}

#' Make Residual Plot
#'
#' Not yet implemented.  Not meant to be called externally. Helper plot function
#' for \code{strata} object with type = "residual"
#'
#' @description Produces partial residual plots for the prognostic score models
#' @param auto_strata an \code{auto_strata} object
#' @return Returns the (partial) residual plot(s)
make_resid_plot <- function(auto_strata){
  # TODO: Implement
  return(plot(1))
}

#' Plot method for \code{strata} object
#'
#' Generates diagnostic plots for the product of a stratification by
#' \code{\link{auto_stratify}} or \code{\link{manual_stratify}}.  There are
#' three plot types: \enumerate{ \item "scatter" (default) - produces a scatter
#' plot of strata by size and treat:control ratio \item "hist" - produces a
#' histogram of stratum size (not supported for \code{manual strata} objects)
#' \item "resid" - produces a residual plot for the prognostic model (not
#' supported for \code{manual strata} objects) }
#'
#' @param x a \code{strata} object returned by \code{\link{auto_stratify}}
#'   or \code{\link{manual_stratify}}
#' @param type string giving the plot type (default = \code{"scatter"}).  Other
#'   options are \code{"hist"} and \code{"residual"}
#' @param label ignored unless \code{type = scatter}. If \code{TRUE}, then
#'   problematic strata are labeled in the scatter plot
#' @param ... other arguments
#' @export
plot.strata <- function(x, type = "scatter", label = FALSE, ...){
  if (type == "scatter"){
    make_scatter_plot(x$issue_table, label)
  }
  else if (type == "hist"){
    if (!("auto_strata" %in% class(x))){
      stop("Prognostic score histograms are only valid for auto-stratified data.")
    } else {
      make_hist_plot(x)
    }
  }
  else if (type == "residual"){
    if (!("auto_strata" %in% class(x))){
      stop("Prognostic score residual plots are only valid for auto-stratified data.")
    } else {
      if (is.null(x$prog_model)){
        stop("Cannot make prognostic score residual plots. Prognostic model is unknown.")
      } else{
        make_resid_plot(x)
      }
    }
  }
   else {
     stop("Not a recognized plot type.")
   }
}
