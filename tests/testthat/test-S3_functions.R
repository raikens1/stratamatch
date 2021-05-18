context("S3 methods for Strata objects")

# Load test data
#
# test_dat is a simple data frame with treated, outcomes, and some categorical and
# continuous columns based on make_sample_data. I've added a column named
# "treat" and "outcome" to ensure that nothing weird happens when treat or
# outcome is a real column in the data frame
test_dat <- read.csv("test_data.csv")

#----------------------------------------------------------
### PRINT
#----------------------------------------------------------

test_that("Print manual strata works", {
  m.strat <- manual_stratify(test_dat, treated ~ C1)

  expect_known_output(print(m.strat), file = "ref_mstrat_print", update = F)
})

test_that("Print auto strata works", {
  a.strat <- auto_stratify(test_dat,
    "treated",
    prognosis = 1 / (1 + exp(-test_dat$X1)),
    outcome = "outcomes"
  )

  expect_known_output(print(a.strat), file = "ref_astrat_print", update = F)
})


#----------------------------------------------------------
### SUMMARY
#----------------------------------------------------------

test_that("Summary of strata works", {
  m.strat <- manual_stratify(test_dat, treated ~ C1)

  expect_known_output(summary(m.strat), file = "ref_mstrat_summary", update = F)

  a.strat <- auto_stratify(test_dat,
    "treated",
    prognosis = 1 / (1 + exp(-test_dat$X1)),
    outcome = "outcomes"
  )

  expect_known_output(summary(a.strat), file = "ref_astrat_summary", update = F)
})

#----------------------------------------------------------
### PLOT METHODS
#----------------------------------------------------------

test_that("Plot errors work", {
  m.strat <- manual_stratify(test_dat, treated ~ C1)
  a.strat <- auto_stratify(test_dat,
    "treated",
    prognosis = 1 / (1 + exp(-test_dat$X1)),
    outcome = "outcomes"
  )

  # bad plot type
  expect_error(plot(m.strat, type = "qq"))

  # residual plots
  expect_error(
    plot(m.strat, type = "residual"),
    "Cannot make prognostic score residual plot on manually stratified data."
  )
  expect_error(
    plot(a.strat, type = "residual"),
    "Cannot make prognostic model residual plots since prognostic scores were provided."
  )

  # AC plots
  expect_error(
    plot(m.strat,
      type = "AC",
      propensity = treated ~ X1 + X2, stratum = 1
    ),
    "Cannot make Assignment-Control plots on manually stratified data."
  )
  expect_error(
    plot(a.strat,
      type = "AC",
      propensity = treated ~ X1 + X2, stratum = 200
    ),
    "Stratum number does not exist in analysis set"
  )
  expect_error(
    plot(a.strat, type = "AC", propensity = "soup", stratum = 1),
    "propensity type not recognized"
  )

  # overlap plots
  expect_error(
    plot(a.strat,
      type = "hist",
      propensity = treated ~ X1 + X2, stratum = 200
    ),
    "Stratum number does not exist in analysis set"
  )
  expect_error(
    plot(a.strat,
      type = "hist",
      propensity = 1:3, stratum = 1
    ),
    "propensity scores must be the same length as the data"
  )
  expect_error(
    plot(a.strat,
      type = "hist",
      propensity = outcomes ~ X1, stratum = 1
    ),
    "Model formula must have the format: treated ~ [covariates]",
    fixed = TRUE
  )
  expect_error(
    plot(a.strat, type = "hist", propensity = "soup", stratum = 1),
    "propensity type not recognized"
  )
})

#----------------------------------------------------------
### NEW METHODS
#----------------------------------------------------------

test_that("extract_cut_points works", {
  a.strat <- auto_stratify(test_dat,
    "treated",
    prognosis = 1 / (1 + exp(-test_dat$X1)),
    outcome = "outcomes"
  )

  expect_warning(extract_cut_points(a.strat), "Only one stratum.  Returning NA.")

  a.strat2 <- auto_stratify(test_dat,
    "treated",
    prognosis = 1 / (1 + exp(-test_dat$X1)),
    outcome = "outcomes", size = 25
  )
  expect_equal(
    extract_cut_points(a.strat2),
    c(0.380, 0.518, 0.668)
  )
})
