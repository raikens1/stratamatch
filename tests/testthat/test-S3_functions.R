context("S3 methods for Strata objects")

# Make Test Data
#
# Makes a simple data frame with treat, outcome, a categorical and a continouos
# covariate. Outcome = sigmoid(treat + cont)
make_test_data <- function(){
  n <- 16
  set.seed(123)
  
  data.frame(treat = rep(c(0,1), n/2),
             cat = rep(c(0,1,2,3), each = n/4),
             cont = seq(from = 0, to = 1, length.out = n)) %>%
    dplyr::mutate(outcome = rbinom(n = n, size = 1, p = 1/(1+exp(treat + cont))))
}

#----------------------------------------------------------
### PRINT
#----------------------------------------------------------

test_that("Print manual strata works", {
  test_dat <- make_test_data()
  m.strat <- manual_stratify(test_dat, "treat", c("cat"))
  
  expect_known_output(print(m.strat), file = "ref_mstrat_print", update = F)
})

test_that("Print auto strata works", {
  test_dat <- make_test_data()
  a.strat <- auto_stratify(test_dat,
                           "treat",
                           "outcome",
                           prog_scores = test_dat$cont)
  
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
  m.strat <- manual_stratify(test_dat, "treat", c("cat"))
  
  expect_error(plot(m.strat, type = "residual"), 
               "Prognostic score residual plots are only valid for auto-stratified data.")
  a.strat <- auto_stratify(test_dat,
                           "treat",
                           "outcome",
                           prog_scores = test_dat$cont)
  
  expect_error(plot(a.strat, type = "residual"), 
               "Cannot make prognostic score residual plots. Prognostic model is unknown.")
})


test_that("Scatter plot works", {
  test_dat <- make_test_data()
  m.strat <- manual_stratify(test_dat, "treat", c("cat"))
  
  expect_known_output(plot(m.strat), file = "ref_mstrat_scatter_plot", update = F)
})

# TODO: implement other plot methods
