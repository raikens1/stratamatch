context("Matching with Stratification")

# Load test data
#
# test_dat is a simple data frame with treated, outcomes, and some categorical and
# continuous columns based on make_sample_data. I've added a column named
# "treat" and "outcome" to ensure that nothing weird happens when treat or
# outcome is a real column in the data frame
test_dat <- read.csv("test_data.csv")

test_that("matching works", {
  if (!requireNamespace("optmatch", quietly = TRUE)) {
    skip("optmatch not installed")
  }
  m.strat <- manual_stratify(test_dat, treated ~ C1)

  m.match1 <- strata_match(m.strat, treated ~ X1, k = 1)

  expect_is(m.match1, "optmatch")

  m.strat2 <- manual_stratify(test_dat, treated ~ C1)
  m.match2 <- strata_match(m.strat2, treated ~ X1, k = 2)

  expect_is(m.match2, "optmatch")
})


test_that("matching errors work", {
  if (!requireNamespace("optmatch", quietly = TRUE)) {
    skip("optmatch not installed")
  }
  
  m.strat <- manual_stratify(test_dat, treated ~ C1)

  expect_error(strata_match(m.strat, "soup", k = 1),
               "propensity must be a formula")
  expect_error(strata_match(m.strat, C1 ~ treated + X1, k = 1),
               "propensity formula must model treatment assignment")
  expect_error(strata_match(m.strat, treated ~ C1 + X1, k = "socks"),
               "k must be an integer")
  expect_error(strata_match(m.strat, treated ~ C1 + X1, k = 0),
               "k must be 1 or greater")
  expect_error(strata_match("soup", treated ~ C1 + X1, k = 1),
               "strat must be a strata object")
})
