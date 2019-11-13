context("Split Pilot Set")

# Load test data
#
# test_dat is a simple data frame with treated, outcomes, and some categorical and
# continuous columns based on make_sample_data. I've added a column named
# "treat" and "outcome" to ensure that nothing weird happens when treat or
# outcome is a real column in the data frame
test_dat <- read.csv("test_data.csv")

test_that("split_pilot_set errors work", {
  # bad base arguments
  # bad data
  expect_error(split_pilot_set(92, treat = "treated"),
               "data must be a data.frame")
  
  # bad treat
  expect_error(split_pilot_set(test_dat, treat = c("treated", "control")),
               "treat must be a single string")
  expect_error(split_pilot_set(test_dat, treat = "zombies"),
               "treat must be the name of a column in data")
  
  # bad treat format
  test_dat$treat_cont <- rnorm(100)
  test_dat$treat_char <- ifelse(test_dat$treated == 0, "a", "b")
  test_dat$treat_12 <- test_dat$treated + 1
  expect_error(split_pilot_set(test_dat, treat = "treat_cont"),
               "treatment column must be binary or logical")
  expect_error(split_pilot_set(test_dat, treat = "treat_char"),
               "treatment column must be binary or logical")
  expect_error(split_pilot_set(test_dat, treat = "treat_12"),
               "treatment column must be binary or logical")
  
  # bad pilot set options
  expect_error(split_pilot_set(test_dat,
                               treat = "treated",
                               pilot_fraction = "socks"),
               "pilot_fraction must be numeric")
  expect_error(split_pilot_set(test_dat,
                               treat = "treated",
                               pilot_fraction = -1),
               "pilot_fraction must be between 0 and 1")
  expect_error(split_pilot_set(test_dat,
                               treat = "treated",
                               pilot_sample = -1),
               "pilot_sample must be a data.frame")
  expect_error(split_pilot_set(test_dat,
                               treat = "treated",
                               group_by_covariates = "socks"),
               "All covariates in group_by_covariates must be columns of the data")
  expect_warning(split_pilot_set(test_dat,
                               treat = "treated",
                               group_by_covariates = "X1"),
               "All covariates in group_by_covariates must be discrete")
  expect_error(split_pilot_set(test_dat,
                                 treat = "treated",
                                 group_by_covariates = 9),
                 "group_by_covariates should be a character vector")
  
})