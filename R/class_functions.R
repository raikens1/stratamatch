#----------------------------------------------------------
### INHERITANCE
#----------------------------------------------------------

#' @title Checks strata class
#' @description Checks if the target object is a \code{strata} object.
#' @param object any R object
#' @return Returns \code{TRUE} if its argument has class "strata" among its classes and
#' \code{FALSE} otherwise.
#' @export
is.strata <- function(object) {
  inherits(object, "strata")
}

#----------------------------------------------------------
### PRINT METHODS
#----------------------------------------------------------

#' @export
print.auto_strata <- function(strata) {
  writeLines("auto_strata object from package big_match.\n")

  writeLines("Function call:")
  print(strata$call)

  writeLines(paste("\nAnalysis set dimensions:",
                   dim(strata$analysis_set)[1], "X",
                   dim(strata$analysis_set)[2]))

  if (!is.null(strata$prog_model)) {
    writeLines(paste("\nPrognostic set dimensions:",
                     dim(strata$prognostic_set)[1], "X",
                     dim(strata$prognostic_set)[2]))

    writeLines("\nPrognostic Score Formula:")
    print(strata$prog_model$formula)

  } else {
    writeLines("\nPrognostic Scores prespecified.")
  }

  writeLines(paste("\nNumber of strata:", dim(strata$issue_table)[1],
                   "\n\n\tMin size:", min(strata$issue_table$Total),
                   "\tMax size:", max(strata$issue_table$Total)))
}

#' @export
print.manual_strata <- function(strata) {
  writeLines("manual_strata object from package big_match.\n")

  writeLines("Function call:")
  print(strata$call)

  writeLines(paste("\nAnalysis set dimensions:",
                   dim(strata$analysis_set)[1], "X",
                   dim(strata$analysis_set)[2]))

  writeLines(paste("\nNumber of strata:", dim(strata$issue_table)[1],
                   "\n\n\tMin size:", min(strata$issue_table$Total),
                   "\tMax size:", max(strata$issue_table$Total)))

  if (dim(strata$issue_table)[1] <= 15) {
    writeLines("\nStrata issue table:")
    print(strata$issue_table)
  }
}

#----------------------------------------------------------
### SUMMARY METHODS
#----------------------------------------------------------
#' @export
summary.strata <- function(strata){
  strata_summary <- structure(list(call = strata$call,
                                   issue_table = strata$issue_table,
                                   sum_before = NULL),
                 class = "summary.strata")

  strata_summary$sum_before <- summarize_balance(strata$analysis_set,
                                                 strata$treat)

  return(strata_summary)
}

#' @title Calculate balance measures over a dataframe
#' @description Calculate Mean over Treat and Control, and difference in Means
#' @param data, a \code{data.frame} from a \code{strata} object
#' @param treat, a string giving the name of the treatment assignment column in the dataframe
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

#' @title Helper plot function for \code{strata} object with type = "scatter"
#' @description Produces a scatterplot of stratum size by control proportion.
#' @param strata a \code{strata} object 
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

#' @title Helper plot function for \code{strata} object with type = "hist"
#' @description Produces a column plot of prognostic score means across strata 
#' and a histogram of prognostic score, colored by strata
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

#' @title Helper plot function for \code{strata} object with type = "residual"
#' @description Produces partial residual plots for the prognostic scores
#' @param auto_strata an \code{auto_strata} object 
#' @return Returns the (partial) residual plot(s)
make_resid_plot <- function(auto_strata){
  # TODO: Implement
  return(plot(1))
}

#' @export
plot.strata <- function(strata, type = "scatter", label = FALSE){
  if (type == "scatter"){
    make_scatter_plot(strata$issue_table, label)
  }
  else if (type == "hist"){
    if (!("auto_strata" %in% class(strata))){
      stop("Prognostic score histograms are only valid for auto-stratified data.")
    } else {
      make_hist_plot(strata)
    }
  }
  else if (type == "residual"){
    if (!("auto_strata" %in% class(strata))){
      stop("Prognostic score residual plots are only valid for auto-stratified data.")
    } else {
      if (is.null(strata$prog_model)){
        stop("Cannot make prognostic score residual plots. Prognostic model is unknown.")
      } else{
        make_resid_plot(strata)
      }
    }
  }
   else {
     stop("Not a recognized plot type.")
   }
}