context("Splitting Pilot Set")

# Load test data
#
# test_dat is a simple data frame with treated, outcomes, and some categorical and
# continuous columns based on make_sample_data. I've added a column named
# "treat" and "outcome" to ensure that nothing weird happens when treat or
# outcome is a real column in the data frame
test_dat <- read.csv("test_data.csv")

#----------------------------------------------------------
### ERRORS
#----------------------------------------------------------

test_that("split_pilot_set errors work", {

  # BASIC INPUTS: data and treat  -----------

  # bad data
  expect_error(
    split_pilot_set(92,
      treat = "treated"
    ),
    "data must be a data.frame"
  )

  # bad treat
  expect_error(
    split_pilot_set(test_dat,
      treat = c("treated", "control")
    ),
    "treat must be a single string"
  )

  expect_error(
    split_pilot_set(test_dat,
      treat = "zombies"
    ),
    "treat must be the name of a column in data"
  )

  # bad treat format
  test_dat$treat_cont <- rnorm(100)
  test_dat$treat_char <- ifelse(test_dat$treated == 0, "a", "b")
  test_dat$treat_12 <- test_dat$treated + 1
  expect_error(
    split_pilot_set(test_dat,
      treat = "treat_cont"
    ),
    "treatment column must be binary or logical"
  )
  expect_error(
    split_pilot_set(test_dat,
      treat = "treat_char"
    ),
    "treatment column must be binary or logical"
  )
  expect_error(
    split_pilot_set(test_dat,
      treat = "treat_12"
    ),
    "treatment column must be binary or logical"
  )

  # PILOT SET OPTIONS ----------------------------
  expect_error(
    split_pilot_set(test_dat,
      treat = "treated",
      pilot_fraction = "socks"
    ),
    "pilot_fraction must be numeric"
  )
  expect_error(
    split_pilot_set(test_dat,
      treat = "treated",
      pilot_fraction = -1
    ),
    "pilot_fraction must be between 0 and 1"
  )
  expect_error(
    split_pilot_set(test_dat,
      treat = "treated",
      pilot_size = 0
    ),
    "pilot_size must be greater than 1"
  )
  expect_error(
    split_pilot_set(test_dat,
      treat = "treated",
      pilot_size = "socks"
    ),
    "pilot_size must be numeric"
  )
  expect_error(
    split_pilot_set(test_dat,
      treat = "treated",
      pilot_size = 101
    ),
    "Requested pilot size outnumbers control sample"
  )
  expect_warning(
    split_pilot_set(test_dat,
      treat = "treated",
      pilot_size = 82
    ),
    "Requested pilot size requires more than 70% of all controls"
  )
  expect_error(
    split_pilot_set(test_dat,
      treat = "treated",
      group_by_covariates = "socks"
    ),
    "All covariates in group_by_covariates must be columns of the data"
  )
  expect_error(
    split_pilot_set(test_dat,
      treat = "treated",
      group_by_covariates = 9
    ),
    "group_by_covariates should be a character vector"
  )
})

#----------------------------------------------------------
### CORRECT INPUTS
#----------------------------------------------------------

test_that("split_pilot_set with pilot_fraction works", {
  mysplit <- split_pilot_set(test_dat,
    treat = "treated",
    pilot_fraction = 0.2,
    group_by_covariates = c("B1", "B2")
  )

  expect_true(dim(mysplit$analysis_set)[1] > 0)
  expect_true(dim(mysplit$pilot_set)[1] > 0)
  expect_true(all(mysplit$pilot_set$treated == 0))

  expect_equal(dim(mysplit$analysis_set)[1] + dim(mysplit$pilot_set)[1], 100)
  expect_equal(dim(mysplit$analysis_set)[2], 9)
  expect_equal(dim(mysplit$pilot_set)[2], 9)
})

test_that("split_pilot_set with pilot_size works", {
  mysplit <- split_pilot_set(test_dat,
    treat = "treated",
    pilot_size = 25
  )

  expect_true(dim(mysplit$analysis_set)[1] > 0)
  expect_true(dim(mysplit$pilot_set)[1] > 0)
  expect_true(all(mysplit$pilot_set$treated == 0))

  expect_equal(dim(mysplit$analysis_set)[1] + dim(mysplit$pilot_set)[1], 100)
  expect_equal(dim(mysplit$analysis_set)[2], 9)
  expect_equal(dim(mysplit$pilot_set)[2], 9)
  expect_equal(dim(mysplit$pilot_set)[1], 25)
})
