context("Matching with Stratification")

# Load test data
#
# test_dat is a simple data frame with treated, outcomes, and some categorical and
# continuous columns based on make_sample_data. I've added a column named
# "treat" and "outcome" to ensure that nothing weird happens when treat or
# outcome is a real column in the data frame
test_dat <- read.csv("test_data.csv")

test_that("matching works", {
  set.seed(123)
  if (!requireNamespace("optmatch", quietly = TRUE)) {
    skip("optmatch not installed")
  }
  m.strat <- manual_stratify(test_dat, treated ~ C1)

  m.match1 <- strata_match(m.strat, treated ~ X1, k = 1)
  m.match2 <- strata_match(m.strat, treated ~ X1, k = "full")
  m.match3 <- strata_match(m.strat, treated ~ X1, method = "mahal", k = "full")

  expect_is(m.match1, "optmatch")
  expect_is(m.match2, "optmatch")
  expect_is(m.match3, "optmatch")

  a.strat <- auto_stratify(test_dat,
    treat = "treated",
    prognosis = outcomes ~ X1,
    size = 20
  )

  a.match1 <- strata_match(a.strat, treated ~ X1, k = 1)
  a.match2 <- strata_match(a.strat, treated ~ X1, k = "full")
  a.match3 <- strata_match(a.strat, treated ~ X1, method = "mahal", k = "full")

  expect_is(a.match1, "optmatch")
  expect_is(a.match2, "optmatch")
  expect_is(a.match3, "optmatch")
})


test_that("matching errors work", {
  if (!requireNamespace("optmatch", quietly = TRUE)) {
    skip("optmatch not installed")
  }

  m.strat <- manual_stratify(test_dat, treated ~ C1)

  expect_error(
    strata_match(m.strat, "soup", k = 1),
    "model must be a formula"
  )
  expect_error(
    strata_match(m.strat, C1 ~ treated + X1, k = 1),
    "Model formula must have the format: treated ~ [covariates]",
    fixed = TRUE
  )
  expect_error(
    strata_match(m.strat, treated ~ C1 + X1, k = "socks"),
    "k must be an integer for pair matching, or \"full\" for full matching"
  )
  expect_error(
    strata_match(m.strat, treated ~ C1 + X1, k = 0),
    "When pair matching, k must be 1 or greater"
  )
  expect_error(
    strata_match("soup", treated ~ C1 + X1, k = 1),
    "object must be a strata object"
  )
  expect_error(
    strata_match(m.strat, treated ~ C1 + X1, method = "CEM"),
    "Method CEM not suppported.",
    fixed = TRUE
  )
})
