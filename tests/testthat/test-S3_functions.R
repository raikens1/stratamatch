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
                           "treated", prognosis = 1/(1 + exp(-test_dat$X1)),
                           outcome = "outcomes")

  expect_known_output(print(a.strat), file = "ref_astrat_print", update = F)
})

#----------------------------------------------------------
### PLOT METHODS
#----------------------------------------------------------

test_that("Plot errors work", {
  m.strat <- manual_stratify(test_dat, treated ~ C1)
  a.strat <- auto_stratify(test_dat,
                           "treated", prognosis = 1/(1 + exp(-test_dat$X1)),
                           outcome = "outcomes")

  # bad plot type
  expect_error(plot(m.strat, type = "qq"))

  # residual plots
  expect_error(plot(m.strat, type = "residual"),
               "Prognostic score residual plots are only valid for auto-stratified data.")
  expect_error(plot(a.strat, type = "residual"),
               "Cannot make prognostic model residual plots since prognostic scores were provided.")

  # FM plots
  expect_error(plot(m.strat, type = "FM",
                    propensity = treated ~ X1 + X2, stratum = 1),
               "Cannot make Fisher-Mill plots on manually stratified data.")
  expect_error(plot(a.strat, type = "FM",
                    propensity = treated ~ X1 + X2, stratum = 200),
               "Stratum number does not exist in analysis set")
  
  # overlap plots
  expect_error(plot(a.strat, type = "hist",
                    propensity = treated ~ X1 + X2, stratum = 200),
               "Stratum number does not exist in analysis set")
  expect_error(plot(a.strat, type = "hist",
                    propensity = 1:3, stratum = 1),
               "propensity scores must be the same length as the data")
  expect_error(plot(a.strat, type = "hist",
                    propensity = treated ~ socks, stratum = 1),
               "not all variables in propensity formula appear in data")
  expect_error(plot(a.strat, type = "hist",
                    propensity = outcomes ~ X1, stratum = 1),
               "propensity formula must model treatment assignment")
  expect_error(plot(a.strat, type = "hist", propensity = "soup", stratum = 1),
               "propensity type not recognized")
})
