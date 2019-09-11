#' Make sample data
#'
#' Makes a simple data frame with treat (binary), outcome (binary), and five
#' covariates: X1 (continuous), X2 (continuous), B1 (binary), B2 (binary), and
#' C1 (categorical). Probability outcome = 1 is sigmoid(treat + X1).
#' Probability treatment = 1 is sigmoid(- 0.2 * X1 + X2 - B1 + 2 * B2)
#'
#' @param n the size of the desired data set
#' @examples
#'   dat <- make_sample_data(n = 50)
#' @export
make_sample_data <- function(n = 100){
  set.seed(123)
  
  # make distribution of C3 depend on C1
  B1_0_sample <- c("a", "b", rep("c", 4))
  B1_1_sample <- c("c", "b", rep("a", 4))

  data.frame(X1 = rnorm(n = n),
             X2 = rexp(n = n),
             B1 = rbinom(n = n, size = 1, prob = 0.5)) %>%
    dplyr::mutate(B2 = rbinom(n, size = 1, prob = ifelse(B1 == 0, 0.1, 0.9)),
                  C1_B11 = sample(B1_1_sample, size = n, replace = T),
                  C1_B10 = sample(B1_0_sample, size = n, replace = T)) %>%
    dplyr::mutate(C1 = ifelse(B1 == 0, C1_B10, C1_B11),
                  treat = rbinom(n = n,
                                 size = 1, 
                                 prob = 1 / (1 + exp(-B1 + X2 - 0.2 * X1 + 2 * B2)))) %>%
    dplyr::select(-c(C1_B11, C1_B10)) %>%
    dplyr::mutate(outcome = rbinom(n = n,
                                   size = 1,
                                   prob = 1 / (1 + exp(treat + X1))))
}
