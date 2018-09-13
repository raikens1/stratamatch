
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

plot.strata <- function(strata){
  plot_data <- summarize_means(strata)
  
  ggplot(plot_data, aes(x = stratum_size, y = 1-treat_mean)) + 
    geom_point() +
    labs(x = "Stratum Size", 
         y = "Fraction Control Observations") +
    ylim(c(0,1))
}