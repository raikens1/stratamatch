context("Automatic Statification")

# Load test data
#
# test_dat is a simple data frame with treated, outcomes, and some categorical and
# continuous columns based on make_sample_data. I've added a column named
# "treat" and "outcome" to ensure that nothing weird happens when treat or
# outcome is a real column in the data frame
test_dat <- read.csv("test_data.csv")

expect_issue_table_ok <- function(x, a_set_size, n_strat){
  expect_is(x, "data.frame")
  expect_equal(dim(x), c(n_strat, 6))
  expect_equal(colnames(x),
               c("Stratum", "Treat", "Control", "Total", "Control_Proportion", 
                 "Potential_Issues"))
  expect_equal(as.vector(sapply(x, typeof)), 
               c("integer", "integer", "integer", "integer", "double", 
                 "character"))
  
  expect_equal(x$Treat + x$Control, x$Total)
  expect_equal(x$Control/x$Total, x$Control_Proportion)
  expect_equal(sum(x$Total), a_set_size)
}

expect_strata_table_ok <- function(x, a_set_size, n_strat){
  expect_is(x, "data.frame")
  expect_equal(dim(x), c(n_strat, 3))
  expect_equal(colnames(x),
               c("stratum", "quantile_bin", "size"))
  expect_equal(as.vector(sapply(x, typeof)), 
               c("integer", "character", "integer"))
  
  expect_equal(sum(x$size), a_set_size)
}

#----------------------------------------------------------
### ERRORS
#----------------------------------------------------------

test_that("auto_stratify errors work", {

  # bad data
  expect_error(auto_stratify(92,
                             treat = "treated",
                             outcome = "outcomes",
                             prognosis = 1/(1 + exp(-test_dat$X1))),
               "data must be a data.frame")
  
  # bad treat
  expect_error(auto_stratify(test_dat,
                             treat = c("treated", "control"),
                             outcome = "outcomes",
                             prognosis = 1/(1 + exp(-test_dat$X1))),
               "treat must be a single string")
  expect_error(auto_stratify(test_dat,
                             treat = "zombies",
                             outcome = "outcomes",
                             prognosis = 1/(1 + exp(-test_dat$X1))),
               "treat must be the name of a column in data")
  
  # bad prognostic scores
  expect_error(auto_stratify(test_dat,
                             treat = "treated",
                             outcome = "outcomes",
                             prognosis = 0:3),
               "prognostic scores must be the same length as the data")
  expect_error(auto_stratify(test_dat,
                             treat = "treated",
                             prognosis = 1/(1 + exp(-test_dat$X1))),
               "If specifying prognostic scores, outcome must be specified")
  
  # bad prognostic formula
  expect_error(auto_stratify(test_dat,
                             treat = "treated",
                             prognosis = treated ~ X1),
               "prognostic formula must model outcome, not treatment")
  expect_error(auto_stratify(test_dat,
                             treat = "treated",
                             outcome = "outcomes",
                             prognosis = X1 ~ C1),
               "prognostic formula must model outcome")
  expect_error(auto_stratify(test_dat,
                             treat = "treated",
                             prognosis = outcomes ~ X1 + treated),
               "prognostic formula must model the outcome in the absence of treatment; the treatment assignment may not be a predictor for the prognostic score model")
  
  # check that this runs without throwing an error
  smalldata <- dplyr::select(test_dat, X1, outcomes, treated)
  expect_error(auto_stratify(smalldata,
                             treat = "treated",
                             prognosis = outcomes ~ . - treated, 
                             pilot_fraction = 0.2, # way too large in practice
                             size = 25),
               NA)
  
  # bad prognostic model
  expect_error(auto_stratify(test_dat,
                             treat = "treated",
                             prognosis = glm(outcomes ~ X1,
                                             test_dat,
                                             family = "binomial")),
               "If specifying a prognostic score model, outcome must be specified")
  
  # bad arg for prognosis
  expect_error(auto_stratify(test_dat,
                             treat = "treated",
                             prognosis = "rocks"),
               "prognosis type not recognized")
  
  # bad pilot set options
  expect_error(auto_stratify(test_dat,
                             treat = "treated",
                             prognosis = outcomes ~ X1,
                             pilot_fraction = "socks"),
               "pilot_fraction must be numeric")
  expect_error(auto_stratify(test_dat,
                             treat = "treated",
                             prognosis = outcomes ~ X1,
                             pilot_fraction = -1),
               "pilot_fraction must be between 0 and 1")
  expect_error(auto_stratify(test_dat,
                             treat = "treated",
                             prognosis = outcomes ~ X1,
                             pilot_sample = -1),
               "pilot_sample must be a data.frame")
  
  # bad outcome format
  test_dat$outcome_12 <- test_dat$outcomes + 1
  test_dat$outcome_ab <- ifelse(test_dat$outcomes == 0, "a", "b")
  
  expect_warning(auto_stratify(test_dat,
                             treat = "treated",
                             prognosis = outcome_12 ~ X1,
                             pilot_sample = test_dat),
                 "outcome column has only two values. Is it binary?")
  
  expect_error(auto_stratify(test_dat,
                             treat = "treated",
                             prognosis = outcome_ab ~ X1,
                             pilot_sample = test_dat),
               "Outcome was not a recognized type. Must be binary, logical, or numeric")
  
  # bad treat format
  test_dat$treat_cont <- rnorm(100)
  test_dat$treat_char <- ifelse(test_dat$treated == 0, "a", "b")
  test_dat$treat_12 <- test_dat$treated + 1
  expect_error(auto_stratify(test_dat,
                               treat = "treat_cont",
                               prognosis = outcomes ~ X1,
                               pilot_sample = test_dat),
               "treatment column must be binary or logical")
  expect_error(auto_stratify(test_dat,
                             treat = "treat_char",
                             prognosis = outcomes ~ X1,
                             pilot_sample = test_dat),
               "treatment column must be binary or logical")
  expect_error(auto_stratify(test_dat,
                             treat = "treat_12",
                             prognosis = outcomes ~ X1,
                             pilot_sample = test_dat),
               "treatment column must be binary or logical")
})

#----------------------------------------------------------
### CORRECT INPUTS
#----------------------------------------------------------

test_that("auto_stratify with prognostic scores works", {

  a.strat <- auto_stratify(test_dat,
                           treat = "treated",
                           outcome = "outcomes",
                           prognosis =  1/(1 + exp(-test_dat$X1)),
                           size = 20)
  
  expect_is(a.strat, "auto_strata")
  expect_is(a.strat, "strata")

  expect_equal(dplyr::select(a.strat$analysis_set, -stratum), test_dat)
  expect_equal(sort(unique(a.strat$analysis_set$stratum)), 1:5)

  expect_equal(a.strat$treat, "treated")

  expect_equal(toString(a.strat$call),
               "auto_stratify, test_dat, treated, 1/(1 + exp(-test_dat$X1)), outcomes, 20")
  
  expect_issue_table_ok(a.strat$issue_table, 100, 5)
  expect_strata_table_ok(a.strat$strata_table, 100, 5)

  expect_equal(a.strat$outcome, "outcomes")

  expect_equal(a.strat$prognostic_scores, 1/(1 + exp(-test_dat$X1)))
  expect_null(a.strat$prognostic_model)
  expect_null(a.strat$pilot_set)
})


test_that("auto_stratify with prognostic formula + pilot_fraction works", {
  
  a.strat <- auto_stratify(test_dat,
                           treat = "treated",
                           outcome = "outcomes",
                           prognosis = outcomes ~ X1,
                           pilot_fraction = 0.2,
                           size = 25)

  expect_is(a.strat, "auto_strata")
  expect_is(a.strat, "strata")
  
  expect_equal(sort(unique(a.strat$analysis_set$stratum)), 1:4)
  expect_equal(dim(a.strat$analysis_set)[1] + dim(a.strat$pilot_set)[1], 100)
  expect_equal(dim(a.strat$analysis_set)[2], 10)
  expect_equal(dim(a.strat$pilot_set)[2], 9)

  expect_equal(a.strat$treat, "treated")

  expect_equal(toString(a.strat$call),
               "auto_stratify, test_dat, treated, outcomes ~ X1, outcomes, 25, 0.2")

  expect_issue_table_ok(a.strat$issue_table, dim(a.strat$analysis_set)[1], 4)
  expect_strata_table_ok(a.strat$strata_table, dim(a.strat$analysis_set)[1], 4)

  expect_equal(a.strat$outcome, "outcomes")

  exp_prognostic_model <- glm(a.strat$pilot_set,
                        formula = outcomes ~ X1,
                        family = binomial)

  exp_prognostic_scores <- predict(exp_prognostic_model,
                             a.strat$analysis_set, type = "response")

  expect_equal(a.strat$prognostic_scores, exp_prognostic_scores)

  expect_equal(coef(a.strat$prognostic_model), coef(exp_prognostic_model))
})

test_that("auto_stratify with prognostic formula + pilot_sample works", {

  a.strat <- auto_stratify(test_dat,
                           treat = "treated",
                           outcome = "outcomes",
                           prognosis = outcomes ~ X1,
                           pilot_sample = test_dat, # bad practice; ok for tests
                           size = 20)

  expect_is(a.strat, "auto_strata")
  expect_is(a.strat, "strata")

  expect_equal(dplyr::select(a.strat$analysis_set, -stratum), test_dat)
  expect_equal(sort(unique(a.strat$analysis_set$stratum)), 1:5)
  expect_equal(a.strat$treat, "treated")

  expect_equal(toString(a.strat$call),
               "auto_stratify, test_dat, treated, outcomes ~ X1, outcomes, 20, test_dat")

  expect_issue_table_ok(a.strat$issue_table, 100, 5)
  expect_strata_table_ok(a.strat$strata_table, 100, 5)

  expect_equal(a.strat$outcome, "outcomes")

  exp_prognostic_model <- glm(test_dat,
                        formula = outcomes ~ X1,
                        family = binomial)
  exp_prognostic_scores <- predict(exp_prognostic_model,
                             test_dat, type = "response")

  expect_equal(a.strat$prognostic_scores, exp_prognostic_scores)
  expect_equal(coef(a.strat$prognostic_model), coef(exp_prognostic_model))

  expect_equal(a.strat$pilot_set, test_dat)
})


test_that("auto_stratify with prognostic model works", {

  # use "." syntax in formula to check that this is allowed
  progmod <- glm(outcomes ~ X1,
                 test_dat, family = "binomial")

  a.strat <- auto_stratify(test_dat,
                           treat = "treated",
                           outcome = "outcomes",
                           prognosis = progmod,
                           size = 20)

  expect_is(a.strat, "auto_strata")
  expect_is(a.strat, "strata")

  expect_equal(dplyr::select(a.strat$analysis_set, -stratum), test_dat)
  expect_equal(sort(unique(a.strat$analysis_set$stratum)), 1:5)

  expect_equal(a.strat$treat, "treated")

  expect_equal(toString(a.strat$call),
               "auto_stratify, test_dat, treated, progmod, outcomes, 20")

  expect_issue_table_ok(a.strat$issue_table, 100, 5)
  expect_strata_table_ok(a.strat$strata_table, 100, 5)

  expect_equal(a.strat$outcome, "outcomes")

  exp_prognostic_scores <- predict(progmod,
                             test_dat, type = "response")

  expect_equal(a.strat$prognostic_scores, exp_prognostic_scores)

  expect_equal(coef(a.strat$prognostic_model), coef(progmod))

  expect_null(a.strat$pilot_set)
})

test_that("auto_stratify with logical outcome works", {
  test_dat_log_o <- test_dat %>%
    dplyr::mutate(outcomes = outcomes == 1)
  
  a.strat <- auto_stratify(test_dat_log_o,
                           treat = "treated",
                           outcome = "outcomes",
                           prognosis = outcomes ~ X1,
                           pilot_fraction = 0.2,
                           size = 25)

  expect_is(a.strat, "auto_strata")
  expect_is(a.strat, "strata")
  
  expect_equal(sort(unique(a.strat$analysis_set$stratum)), 1:4)
  expect_equal(dim(a.strat$analysis_set)[1] + dim(a.strat$pilot_set)[1], 100)
  expect_equal(dim(a.strat$analysis_set)[2], 10)
  expect_equal(dim(a.strat$pilot_set)[2], 9)
  
  expect_equal(a.strat$treat, "treated")
  
  expect_equal(toString(a.strat$call),
               "auto_stratify, test_dat_log_o, treated, outcomes ~ X1, outcomes, 25, 0.2")
  
  expect_issue_table_ok(a.strat$issue_table, dim(a.strat$analysis_set)[1], 4)
  expect_strata_table_ok(a.strat$strata_table, dim(a.strat$analysis_set)[1], 4)
  
  expect_equal(a.strat$outcome, "outcomes")
  
  exp_prognostic_model <- glm(a.strat$pilot_set,
                              formula = outcomes ~ X1,
                              family = binomial)
  
  exp_prognostic_scores <- predict(exp_prognostic_model,
                                   a.strat$analysis_set, type = "response")
  
  expect_equal(a.strat$prognostic_scores, exp_prognostic_scores)
  
  expect_equal(coef(a.strat$prognostic_model), coef(exp_prognostic_model))
})


test_that("auto_stratify with logical treatment works", {
  test_dat_log_t <- test_dat %>%
    dplyr::mutate(treated = treated == 1)
  
  a.strat <- auto_stratify(test_dat_log_t,
                           treat = "treated",
                           outcome = "outcomes",
                           prognosis = outcomes ~ X1,
                           pilot_fraction = 0.2, # way too large in practice
                           size = 25)
  
  expect_is(a.strat, "auto_strata")
  expect_is(a.strat, "strata")
  
  expect_equal(sort(unique(a.strat$analysis_set$stratum)), 1:4)
  expect_equal(dim(a.strat$analysis_set)[1] + dim(a.strat$pilot_set)[1], 100)
  expect_equal(dim(a.strat$analysis_set)[2], 10)
  expect_equal(dim(a.strat$pilot_set)[2], 9)
  
  expect_equal(a.strat$treat, "treated")
  
  expect_equal(toString(a.strat$call),
               "auto_stratify, test_dat_log_t, treated, outcomes ~ X1, outcomes, 25, 0.2")
  
  expect_issue_table_ok(a.strat$issue_table, dim(a.strat$analysis_set)[1], 4)
  expect_strata_table_ok(a.strat$strata_table, dim(a.strat$analysis_set)[1], 4)
  
  expect_equal(a.strat$outcome, "outcomes")
  
  exp_prognostic_model <- glm(a.strat$pilot_set,
                              formula = outcomes ~ X1,
                              family = binomial)
  
  exp_prognostic_scores <- predict(exp_prognostic_model,
                                   a.strat$analysis_set, type = "response")
  
  expect_equal(a.strat$prognostic_scores, exp_prognostic_scores)
  
  expect_equal(coef(a.strat$prognostic_model), coef(exp_prognostic_model))
})

test_that("auto_stratify with continuous outcome works", {
  test_dat_cont_o <- test_dat %>%
    dplyr::mutate(outcomes = rnorm(100))
  
  a.strat <- auto_stratify(test_dat_cont_o,
                           treat = "treated",
                           outcome = "outcomes",
                           prognosis = outcomes ~ X1,
                           pilot_fraction = 0.2, 
                           size = 25)

  expect_is(a.strat, "auto_strata")
  expect_is(a.strat, "strata")
  
  expect_equal(sort(unique(a.strat$analysis_set$stratum)), 1:4)
  expect_equal(dim(a.strat$analysis_set)[1] + dim(a.strat$pilot_set)[1], 100)
  expect_equal(dim(a.strat$analysis_set)[2], 10)
  expect_equal(dim(a.strat$pilot_set)[2], 9)
  
  expect_equal(a.strat$treat, "treated")
  
  expect_equal(toString(a.strat$call),
               "auto_stratify, test_dat_cont_o, treated, outcomes ~ X1, outcomes, 25, 0.2")
  
  expect_issue_table_ok(a.strat$issue_table, dim(a.strat$analysis_set)[1], 4)
  expect_strata_table_ok(a.strat$strata_table, dim(a.strat$analysis_set)[1], 4)
  
  expect_equal(a.strat$outcome, "outcomes")
  
  exp_prognostic_model <- lm(a.strat$pilot_set,
                              formula = outcomes ~ X1)
  
  exp_prognostic_scores <- predict(exp_prognostic_model,
                                   a.strat$analysis_set, type = "response")
  
  expect_equal(a.strat$prognostic_scores, exp_prognostic_scores)
  
  expect_equal(coef(a.strat$prognostic_model), coef(exp_prognostic_model))
})

