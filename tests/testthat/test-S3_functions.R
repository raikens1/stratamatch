context("S3 methods for Strata objects")

# Make Test Data
#
# Makes a simple data frame with treat, outcome, a categorical and a continouos
# covariate. Outcomes = sigmoid(treated + cont).  I've added a column named
# "treat" and "outcome" to ensure that nothing weird happens when treat or
# outcome is a real column in the data frame
make_test_data <- function(){
  n <- 16
  set.seed(123)
  
  data.frame(treated = rep(c(0, 1), n / 2),
             treat = rep("gotcha", n),
             cat = rep(c(0, 1, 2, 3), each = n / 4),
             cont = seq(from = 0, to = 1, length.out = n)) %>%
    dplyr::mutate(outcomes = rbinom(n = n,
                                    size = 1,
                                    p = 1 / (1 + exp(treated + cont))),
                  outcome = rep("gotcha", n))
}

#----------------------------------------------------------
### PRINT
#----------------------------------------------------------

test_that("Print manual strata works", {
  test_dat <- make_test_data()
  m.strat <- manual_stratify(test_dat, treated ~ cat)

  expect_known_output(print(m.strat), file = "ref_mstrat_print", update = F)
})

test_that("Print auto strata works", {
  test_dat <- make_test_data()
  a.strat <- auto_stratify(test_dat,
                           "treated", prognosis = test_dat$cont,
                           outcome = "outcomes")

  expect_known_output(print(a.strat), file = "ref_astrat_print", update = F)
})

#----------------------------------------------------------
### PLOT METHODS
#----------------------------------------------------------

test_that("Plot errors work", {
  test_dat <- make_test_data()
  m.strat <- manual_stratify(test_dat, treated ~ cat)
  a.strat <- auto_stratify(test_dat,
                           "treated", prognosis = test_dat$cont,
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
  expect_error(plot(a.strat, type = "FM", propensity = "soup", stratum = 1),
               "propensity type not recognized")
  
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
                    propensity = outcomes ~ cont, stratum = 1),
               "propensity formula must model treatment assignment")
  expect_error(plot(a.strat, type = "hist", propensity = "soup", stratum = 1),
               "propensity type not recognized")
})
