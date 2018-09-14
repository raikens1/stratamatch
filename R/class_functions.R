
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
### PLOT METHODS
#----------------------------------------------------------

#' @title Summarize means across strata
#' @description Summarize means of each covariate across strata in a \code{strata} object.
#' @param strata a \code{strata} object 
#' @return Returns a summary dataframe with means of numeric variables and total count per stratum

summarize_means <- function(strata){
  means <- strata$data %>% 
    group_by(stratum) %>%
    summarize_if(.predicate = function(x) is.numeric(x),
                  .funs = funs(mean="mean")) 
  
  counts <- strata$data %>% 
    group_by(stratum) %>%
    summarize(stratum_size = n())
  
  return(merge(means, counts, by = "stratum"))
}

#' @title Plot function for \code{strata} object
#' @description Produces a scatterplot of stratum size by control proportion.
#' @param strata a \code{strata} object 
#' @return Returns the scatterplot

plot.strata <- function(strata){
  # set parameters
  CONTROL_MIN = 0.2
  CONTROL_MAX = 0.8
  SIZE_MIN = 75
  SIZE_MAX = 4000
  
  # extract strata size and control proportion
  plot_data <- summarize_means(strata)
  
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
    theme(legend.position = "none") + 
    geom_label_repel(data = problem_strata, aes(stratum_size, 1-treat_mean,
                                     label = stratum), size = 2.5, color = "firebrick")
}