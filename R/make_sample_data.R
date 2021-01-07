#' Make sample data
#'
#' Makes a simple data frame with treat (binary), outcome (binary), and five
#' covariates: X1 (continuous), X2 (continuous), B1 (binary), B2 (binary), and
#' C1 (categorical). Probability outcome = 1 is sigmoid(treat + X1). Probability
#' treatment = 1 is sigmoid(- 0.2 * X1 + X2 - B1 + 2 * B2)
#'
#' @param n the size of the desired data set
#' @examples
#' # make sample data set of 30 observations
#' dat <- make_sample_data(n = 30)
#' @export
make_sample_data <- function(n = 100) {
  # make distribution of C3 depend on C1
  sample_0 <- c("a", "b", rep("c", 4))
  sample_1 <- c("c", "b", rep("a", 4))

  data.frame(
    X1 = rnorm(n = n),
    X2 = rexp(n = n),
    B1 = rbinom(n = n, size = 1, prob = 0.5)
  ) %>%
    dplyr::mutate(
      B2 = rbinom(n,
        size = 1,
        prob = ifelse(.data$B1 == 0, 0.1, 0.9)
      ),
      C1_1 = sample(sample_1, size = n, replace = T),
      C1_0 = sample(sample_0, size = n, replace = T)
    ) %>%
    dplyr::mutate(
      C1 = ifelse(.data$B1 == 0, .data$C1_0, .data$C1_1),
      treat = rbinom(
        n = n,
        size = 1,
        prob = 1 / (1 + exp(-.data$B1 + .data$X2 - 0.2 * .data$X1 + 2 * .data$B2))
      )
    ) %>%
    dplyr::select(-c(.data$C1_1, .data$C1_0)) %>%
    dplyr::mutate(outcome = rbinom(
      n = n,
      size = 1,
      prob = 1 / (1 + exp(.data$treat + .data$X1))
    ))
}
