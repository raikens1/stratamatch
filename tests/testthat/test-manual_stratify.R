context("Manual Statification")

# Make Test Data
#
# Makes a simple data frame with treat, outcome, a categorical and a continouos
# covariate. Outcomes = sigmoid(treated + cont).  I've added a column named
# "treat" and "outcome" to ensure that nothing weird happens when treat or
# outcome is a real column in the data frame
make_test_data <- function(){
  n <- 16
  
  dplyr::tibble(treated = rep(c(0, 1), n / 2),
                treat = rep("gotcha", n),
                cat = rep(c(0, 1, 2, 3), each = n / 4),
                cont = seq(from = 0, to = 1, length.out = n)) %>%
    dplyr::mutate(outcomes = c(0,1,0,1,1,0,0,1,0,0,1,0,0,0,0,1),
                  outcome = rep("gotcha", n))
}

#----------------------------------------------------------
### ERRORS
#----------------------------------------------------------

test_that("manual_stratify errors work", {
  test_dat <- make_test_data() %>%
    dplyr::mutate(treated = treated == 1)
  
  expect_error(manual_stratify(test_dat, strata_formula = treated ~ cat + cont),
               "There are 16 distinct values for cont. Is it continuous?")
  
  expect_warning(manual_stratify(test_dat, strata_formula = treated ~ cat + cont,
                                 force = TRUE),
                 "There are 16 distinct values for cont. Is it continuous?")
  
  expect_error(manual_stratify(test_dat, strata_formula = 34),
               "strata_formula must be a formula")
  expect_error(manual_stratify("socks", strata_formula = treated ~ cat),
               "data must be a data.frame")
  expect_error(manual_stratify(test_dat, strata_formula = treated ~ cat,
                               force = "socks"),
               "force must equal either TRUE or FALSE")
  expect_error(manual_stratify(test_dat, strata_formula = zombies ~ cat),
               "not all variables in stat_formula appear in data")
  
  test_dat$treat_cont <- rnorm(16)
  test_dat$treat_char <- ifelse(test_dat$treated == 1, "a", "b")
  test_dat$treat_12 <- test_dat$treated + 1
  expect_error(manual_stratify(test_dat, strata_formula = treat_cont ~ cat),
               "treatment column must be binary or logical")
  expect_error(manual_stratify(test_dat, strata_formula = treat_char ~ cat),
               "treatment column must be binary or logical")
  expect_error(manual_stratify(test_dat, strata_formula = treat_12 ~ cat),
               "treatment column must be binary or logical")
})

test_that("manual stratify with logical treatment works", {
  test_dat <- make_test_data()
  
  m.strat <- manual_stratify(test_dat, treated ~ cat)
  
  expect_is(m.strat, "manual_strata")
  expect_is(m.strat, "strata")
  
  expect_equal(m.strat$analysis_set,
               dplyr::mutate(test_dat, stratum = as.integer(cat + 1)))
  
  expect_equal(m.strat$treat, "treated")
  
  expect_equal(toString(m.strat$call),
               "manual_stratify, test_dat, treated ~ cat")
  
  exp_issue_table <- dplyr::tibble(Stratum = 1:4,
                                   Treat = rep(2, 4),
                                   Control = as.integer(rep(2, 4)),
                                   Total = as.integer(rep(4, 4)),
                                   Control_Proportion = rep(0.5, 4),
                                   Potential_Issues = rep("Too few samples", 4))
  
  expect_equal(m.strat$issue_table, exp_issue_table)
  
  expect_equal(m.strat$covariates, c("cat"))
  
  exp_strata_table <- dplyr::tibble(cat = as.numeric(0:3),
                                    stratum = 1:4,
                                    size = as.integer(rep(4, 4)))
  
  expect_equal(m.strat$strata_table, exp_strata_table)
})

#----------------------------------------------------------
### CORRECT INPUTS
#----------------------------------------------------------

test_that("manual stratify works", {
  test_dat <- make_test_data()
  
  m.strat <- manual_stratify(test_dat, treated ~ cat)
  
  expect_is(m.strat, "manual_strata")
  expect_is(m.strat, "strata")
  
  expect_equal(m.strat$analysis_set,
               dplyr::mutate(test_dat, stratum = as.integer(cat + 1)))
  
  expect_equal(m.strat$treat, "treated")
  
  expect_equal(toString(m.strat$call),
               "manual_stratify, test_dat, treated ~ cat")
  
  exp_issue_table <- dplyr::tibble(Stratum = 1:4,
                                   Treat = rep(2, 4),
                                   Control = as.integer(rep(2, 4)),
                                   Total = as.integer(rep(4, 4)),
                                   Control_Proportion = rep(0.5, 4),
                                   Potential_Issues = rep("Too few samples", 4))
  
  expect_equal(m.strat$issue_table, exp_issue_table)
  
  expect_equal(m.strat$covariates, c("cat"))
  
  exp_strata_table <- dplyr::tibble(cat = as.numeric(0:3),
                                    stratum = 1:4,
                                    size = as.integer(rep(4, 4)))
  
  expect_equal(m.strat$strata_table, exp_strata_table)
})