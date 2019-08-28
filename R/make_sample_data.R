#' Make sample data
#'
#' Makes a simple data frame with treat (binary), outcome (binary), and two
#' covariates: X1 (categorical) and X2 (continuous). Probability outcome = 1 is
#' sigmoid(treat + X2)
#'
#' @param n the size of the desired data set
#' @param p_treat the probability of treatment
#' @examples
#'   dat <- make_sample_data(n = 50, p_treat = 0.3)
#' @export
make_sample_data <- function(n = 16, p_treat = 0.5){
  set.seed(123)

  data.frame(treat = rbinom(n = n, size = 1, p = p_treat),
             X1 = sample(1:4, size = n, replace = T),
             X2 = rnorm(n = n)) %>%
    dplyr::mutate(outcome = rbinom(n = n,
                                   size = 1,
                                   p = 1 / (1 + exp(treat + X2))))
}