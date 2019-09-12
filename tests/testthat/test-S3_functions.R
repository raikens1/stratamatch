context("S3 methods for Strata objects")

# Make Test Data
#
# Makes a simple data frame with treat, outcome, a categorical and a continouos
# covariate. Outcome = sigmoid(treat + cont)
make_test_data <- function(){
  n <- 16
  set.seed(123)

  data.frame(treat = rep(c(0, 1), n / 2),
             cat = rep(c(0, 1, 2, 3), each = n / 4),
             cont = seq(from = 0, to = 1, length.out = n)) %>%
    dplyr::mutate(outcome = rbinom(n = n,
                                   size = 1,
                                   p = 1 / (1 + exp(treat + cont))))
}

#----------------------------------------------------------
### PRINT
#----------------------------------------------------------

test_that("Print manual strata works", {
  test_dat <- make_test_data()
  m.strat <- manual_stratify(test_dat, treat ~ cat)

  expect_known_output(print(m.strat), file = "ref_mstrat_print", update = F)
})

test_that("Print auto strata works", {
  test_dat <- make_test_data()
  a.strat <- auto_stratify(test_dat,
                           "treat", prognosis = test_dat$cont,
                           outcome = "outcome")

  expect_known_output(print(a.strat), file = "ref_astrat_print", update = F)
})

#----------------------------------------------------------
### SUMMARY METHODS
#----------------------------------------------------------

# TODO: implement this

#----------------------------------------------------------
### PLOT METHODS
#----------------------------------------------------------

test_that("Plot errors work", {
  test_dat <- make_test_data()
  m.strat <- manual_stratify(test_dat, treat ~ cat)
  a.strat <- auto_stratify(test_dat,
                           "treat", prognosis = test_dat$cont,
                           outcome = "outcome")

  # bad plot type
  expect_error(plot(m.strat, type = "qq"))

  # residual plots
  expect_error(plot(m.strat, type = "residual"),
               "Prognostic score residual plots are only valid for auto-stratified data.")
  expect_error(plot(a.strat, type = "residual"),
               "Cannot make prognostic model residual plots since prognostic scores were provided.")

  # FM plots
  expect_error(plot(m.strat, type = "FM",
                    propensity = treat ~ X1 + X2, stratum = 1),
               "Cannot make Fisher-Mill plots on manually stratified data.")
  expect_error(plot(a.strat, type = "FM",
                    propensity = treat ~ X1 + X2, stratum = 200),
               "Stratum number does not exist in analysis set")
  expect_error(plot(a.strat, type = "FM", propensity = "soup", stratum = 1),
               "propensity type not recognized")
  
  # overlap plots
  expect_error(plot(a.strat, type = "hist",
                    propensity = treat ~ X1 + X2, stratum = 200),
               "Stratum number does not exist in analysis set")
  expect_error(plot(a.strat, type = "hist",
                    propensity = 1:3, stratum = 1),
               "propensity scores must be the same length as the data")
  expect_error(plot(a.strat, type = "hist",
                    propensity = treat ~ socks, stratum = 1),
               "not all variables in propensity formula appear in data")
  expect_error(plot(a.strat, type = "hist",
                    propensity = outcome ~ cont, stratum = 1),
               "propensity formula must model treatment assignment")
  expect_error(plot(a.strat, type = "hist", propensity = "soup", stratum = 1),
               "propensity type not recognized")
})
