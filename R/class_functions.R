
#----------------------------------------------------------
### INHERITANCE
#----------------------------------------------------------

#' @title Checks strata class
#' @description Checks if the target object is a \code{strata} object.
#' @param object any R object
#' @return Returns \code{TRUE} if its argument has class "strata" among its classes and
#' \code{FALSE} otherwise.

is.strata <- function(object) {
  inherits(object, "strata")
}

#----------------------------------------------------------
### PRINT METHODS
#----------------------------------------------------------

print.auto_strata <- function(strata) {
  writeLines("auto_strata object from package big_match.\n")
  writeLines("Function call:")
  print(strata$call)
  if(!is.null(strata$prog_model)){
    writeLines("\nPrognostic Score Model:")
    print(strata$prog_model) # TODO: print formula rather than model
  } else {
    writeLines("\nPrognostic Scores prespecified.")
  }
  writeLines("\nStrata Sizes:")
  print(strata$n_table)
}

print.manual_strata <- function(strata) {
  writeLines("manual_strata object from package big_match.\n")
  writeLines("Function call:")
  print(strata$call)
  writeLines("\nStrata Sizes:")
  print(strata$n_table)
}

#----------------------------------------------------------
### PLOT METHODS
#----------------------------------------------------------

#' @title Summarize means across strata
#' @description Summarize means of each covariate across strata in a \code{strata} object.
#' @param strata a \code{strata} object 
#' @return Returns a summary dataframe with means of numeric variables and total count per stratum

summarize_means <- function(data, treat){
  names(data)[names(data) == treat] <- "treat"
  
  means <- data %>% 
    group_by(stratum) %>%
    summarize_if(.predicate = function(x) is.numeric(x),
                  .funs = funs(mean="mean")) 
  
  counts <- data %>% 
    group_by(stratum) %>%
    summarize(stratum_size = n())
  
  return(merge(means, counts, by = "stratum"))
}

#' @title Helper plot function for \code{strata} object with type = "scatter"
#' @description Produces a scatterplot of stratum size by control proportion.
#' @param strata a \code{strata} object 
#' @return Returns the scatterplot

scatter_plot_helper <- function(plot_data){
  # set parameters
  CONTROL_MIN = 0.2
  CONTROL_MAX = 0.8
  SIZE_MIN = 75
  SIZE_MAX = 4000
  
  # identify strata that are too large/small/imbalanced
  problem_strata <- plot_data %>%
    filter(stratum_size > 4000 | stratum_size < 75 | 
             treat_mean < 0.2 | treat_mean > 0.8)
  
  xmax <- max(plot_data$stratum_size, 4250)
  
  ggplot(plot_data, aes(x = stratum_size, y = 1-treat_mean)) + 
    geom_point() +
    labs(x = "Stratum Size", 
         y = "Fraction Control Observations") +
    ylim(c(0,1)) + xlim(c(0, xmax)) + 
    geom_rect(aes(ymin=0, ymax=CONTROL_MIN, xmin = 0, xmax = Inf, fill = "firebrick1"), alpha = 0.05) + 
    geom_rect(aes(ymin=CONTROL_MAX, ymax=1, xmin = 0, xmax = Inf, fill = "firebrick1"), alpha = 0.05) +
    geom_rect(aes(ymin=0, ymax=1, xmin = 0, xmax = SIZE_MIN, fill = "firebrick1"), alpha = 0.05) +
    geom_rect(aes(ymin=0, ymax=1, xmin = SIZE_MAX, xmax = Inf, fill = "firebrick1"), alpha = 0.05) + 
    geom_vline(xintercept= SIZE_MAX, colour = "firebrick") + 
    geom_vline(xintercept= SIZE_MIN, colour = "firebrick") +
    geom_hline(yintercept= CONTROL_MAX, colour = "firebrick") +
    geom_hline(yintercept= CONTROL_MIN, colour = "firebrick") +
    geom_hline(yintercept= 0.5, colour = "black", linetype = 2) + 
    theme(legend.position = "none") + 
    geom_label_repel(data = problem_strata, aes(stratum_size, 1-treat_mean,
                                     label = stratum), size = 2.5, color = "firebrick")
}

#' @title Helper plot function for \code{strata} object with type = "hist"
#' @description Produces a column plot of prognostic score means across strata 
#' and a histogram of prognostic score, colored by strata
#' @param auto_strata an \code{auto_strata} object 
#' @return Returns an arrangement of the column plot and histogram.

hist_plot_helper <- function(auto_strata){
  plotdata <- auto_strata$data
  plotdata$prog_scores <- auto_strata$prog_scores
  
  plot_summary <- plotdata %>% 
    group_by(stratum) %>% 
    summarize(prog_mean = mean(prog_scores))
  
  a <- ggplot(data = plot_summary, aes(x = stratum, y = prog_mean, fill = as.factor(stratum))) + 
    geom_col() +
    labs(y = "Mean Prognistic Score", x = "Stratum") +
    theme(legend.position = "none") +
    scale_fill_brewer(palette = "Blues")
  
  b <- ggplot(plotdata, aes(x = prog_scores, group = as.factor(stratum), 
                            fill = as.factor(stratum))) +
    geom_histogram() + 
    labs(y = "Prognostic Score", "Number of Observations") +
    scale_fill_brewer(name = "Stratum", palette = "Blues") 
  
  return(ggarrange(a,b, ncol = 2, widths = c(1, 1.5)))}

#' @title Helper plot function for \code{strata} object with type = "residual"
#' @description Produces partial residual plots for the prognostic scores
#' @param auto_strata an \code{auto_strata} object 
#' @return Returns the (partial) residual plot(s)

residual_plot_helper <- function(auto_strata){
  # TODO: Implement
  return(plot(1))
}

plot.strata <- function(strata, type = "scatter"){
  if (type == "scatter"){
    scatter_plot_helper(summarize_means(strata$data, strata$treat))
  }
  else if (type == "hist"){
    if (!("auto_strata" %in% class(strata))){
      stop("Prognostic score histograms are only valid for auto-stratified data.")
    } else {
      hist_plot_helper(strata)
    }
  }
  else if (type == "residual"){
    if (!("auto_strata" %in% class(strata))){
      stop("Prognostic score residual plots are only valid for auto-stratified data.")
    } else {
      if (is.null(strata$prog_model)){
        stop("Cannot make prognostic score residual plots. Prognostic model is unknown.")
      } else{
        residual_plot_helper(strata)
      }
    }
  }
   else {
     stop("Not a recognized plot type.")
   }
}