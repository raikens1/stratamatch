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
               "There are 100 distinct values for X1. Is it continuous?")
  expect_error(split_pilot_set(test_dat,
                                 treat = "treated",
                                 group_by_covariates = 9),
                 "group_by_covariates should be a character vector")
})

test_that("split pilot set with pilot sample specified", {
  pilot_set <- dplyr::sample_frac(test_dat, 0.1)
  
  mysplit <- split_pilot_set(test_dat, "treated", pilot_sample = pilot_set)
  
  expect_equal(pilot_set, mysplit$pilot_set)
  expect_equal(test_dat, mysplit$analysis_set)
})

test_that("split pilot set with pilot_frac and group_by_covariates specified", {
  mysplit <- split_pilot_set(test_dat, "treated", pilot_frac = 0.1, 
                             group_by_covariates = c("B1", "B2"))
  
  # hard to come up with a great way to test that this went correctly
  # for now, check that both have at least one row, that all members of the
  # pilot set are controls, and that the number of rows combined is 100
  expect_true(dim(mysplit$analysis_set)[1] > 0)
  expect_true(dim(mysplit$pilot_set)[1] > 0)
  expect_true(all(mysplit$pilot_set$treated == 0))
  
  expect_equal(dim(mysplit$analysis_set)[1] + dim(mysplit$pilot_set)[1], 100)
})