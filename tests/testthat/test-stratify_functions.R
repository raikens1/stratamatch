context("Manual and Automatic Statification")

# Make Test Data
#
# Makes a simple data frame with treat, outcome, a categorical and a continouos
# covariate. Outcome = sigmoid(treat + cont)
make_test_data <- function(){
  n <- 16
  set.seed(123)

  data.frame(treated = rep(c(0, 1), n / 2),
             cat = rep(c(0, 1, 2, 3), each = n / 4),
             cont = seq(from = 0, to = 1, length.out = n)) %>%
    dplyr::mutate(outcome = rbinom(n = n,
                                   size = 1,
                                   p = 1 / (1 + exp(treated + cont))))
}

#----------------------------------------------------------
### MANUAL STRATIFY
#----------------------------------------------------------

test_that("manual_stratify errors work", {
  test_dat <- make_test_data()

  expect_error(manual_stratify(test_dat, strat_formula = treated ~ cat + cont),
               "There are 16 distinct values for cont. Is it continuous?")

  expect_warning(manual_stratify(test_dat, strat_formula = treated ~ cat + cont,
                               force = TRUE),
               "There are 16 distinct values for cont. Is it continuous?")
})

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

  exp_issue_table <- data.frame(Stratum = 1:4,
                            Treat = rep(2, 4),
                            Control = rep(2, 4),
                            Total = as.integer(rep(4, 4)),
                            Control_Proportion = rep(0.5, 4),
                            Potential_Issues = rep("Too few samples", 4),
                            stringsAsFactors = FALSE)

  expect_equal(m.strat$issue_table, exp_issue_table)

  expect_equal(m.strat$covariates, c("cat"))

  exp_strata_table <- dplyr::tibble(cat = as.numeric(0:3),
                             stratum = 1:4,
                             size = as.integer(rep(4, 4)))

  expect_equal(m.strat$strata_table, exp_strata_table)
})

#----------------------------------------------------------
### AUTO STRATIFY
#----------------------------------------------------------

test_that("auto_stratify errors work", {
  test_dat <- make_test_data()

  # bad data
  expect_error(auto_stratify(92,
                             treat = "treated",
                             outcome = "outcome",
                             prognosis = test_dat$cont),
               "data must be a data.frame")
  
  # bad treat
  expect_error(auto_stratify(test_dat,
                             treat = c("treated", "control"),
                             outcome = "outcome",
                             prognosis = test_dat$cont),
               "treat must be a single string")
  expect_error(auto_stratify(test_dat,
                             treat = "zombies",
                             outcome = "outcome",
                             prognosis = test_dat$cont),
               "treat must be the name of a column in data")
  
  # bad prognostic scores
  expect_error(auto_stratify(test_dat,
                             treat = "treated",
                             outcome = "outcome",
                             prognosis = 0:3),
               "prognostic scores must be the same length as the data")
  expect_error(auto_stratify(test_dat,
                             treat = "treated",
                             prognosis = test_dat$cont),
               "If specifying prognostic scores, outcome must be specified")
  
  # bad prognostic formula
  expect_error(auto_stratify(test_dat,
                             treat = "treated",
                             prognosis = outcome ~ socks),
               "not all variables in prognosis formula appear in data")
  expect_error(auto_stratify(test_dat,
                             treat = "treated",
                             prognosis = treated ~ cont),
               "prognostic formula must model outcome, not treatment")
  expect_error(auto_stratify(test_dat,
                             treat = "treated",
                             outcome = "outcome",
                             prognosis = cont ~ cat),
               "prognostic formula must model outcome")
  expect_error(auto_stratify(test_dat,
                             treat = "treated",
                             prognosis = outcome ~ cont + treated),
               "prognostic formula must model the outcome in the absence of treatment; the treatment assignment may not be a predictor for the prognostic score model")
  
  # bad prognostic model
  expect_error(auto_stratify(test_dat,
                             treat = "treated",
                             prognosis = glm(outcome ~ cont,
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
                             prognosis = outcome ~ cont,
                             pilot_fraction = "socks"),
               "pilot_fraction must be numeric")
  expect_error(auto_stratify(test_dat,
                             treat = "treated",
                             prognosis = outcome ~ cont,
                             pilot_fraction = -1),
               "pilot_fraction must be between 0 and 1")
  expect_error(auto_stratify(test_dat,
                             treat = "treated",
                             prognosis = outcome ~ cont,
                             pilot_sample = -1),
               "pilot_sample must be a data.frame")
  expect_error(auto_stratify(test_dat,
                             treat = "treated",
                             prognosis = outcome ~ cont,
                             pilot_sample = dplyr::select(test_dat, -cont)),
               "All variables in prognostic score formula must be in pilot_sample")
})

test_that("auto_stratify with prognostic scores works", {
  test_dat <- make_test_data()

  a.strat <- auto_stratify(test_dat,
                           treat = "treated",
                           outcome = "outcome",
                           prognosis = test_dat$cont,
                           size = 4)
  expect_is(a.strat, "auto_strata")
  expect_is(a.strat, "strata")

  expect_equal(a.strat$analysis_set,
               dplyr::mutate(test_dat,
                             stratum = as.integer(rep(c(1:4), each = 4))))

  expect_equal(a.strat$treat, "treated")

  expect_equal(toString(a.strat$call),
               "auto_stratify, test_dat, treated, test_dat$cont, outcome, 4")

  exp_issue_table <- data.frame(Stratum = 1:4,
                                Treat = rep(2, 4),
                                Control = rep(2, 4),
                                Total = as.integer(rep(4, 4)),
                                Control_Proportion = rep(0.5, 4),
                                Potential_Issues = rep("Too few samples", 4),
                                stringsAsFactors = FALSE)

  expect_equal(a.strat$issue_table, exp_issue_table)

  exp_strata_table <- dplyr::tibble(stratum = 1:4,
                                    quantile_bin = factor(c("[0.000,0.267)",
                                                            "[0.267,0.533)",
                                                            "[0.533,0.800)",
                                                            "[0.800,1.000]")),
                                    size = as.integer(rep(4, 4)))

  expect_equal(a.strat$strata_table, exp_strata_table)

  expect_equal(a.strat$outcome, "outcome")

  expect_equal(a.strat$prog_scores, test_dat$cont)

  expect_equal(a.strat$prog_model, NULL)

  expect_equal(a.strat$pilot_set, NULL)
})


test_that("auto_stratify with prognostic formula + pilot_fraction works", {
  test_dat <- make_test_data()

  set.seed(5)
  a.strat <- auto_stratify(test_dat,
                           treat = "treated",
                           outcome = "outcome",
                           prognosis = outcome ~ cont,
                           pilot_fraction = 0.5, # way too large in practice
                           size = 4)

  pilot_inds <- c(3, 5, 1, 13)

  exp_pilot_set <- test_dat[pilot_inds, ]
  rownames(exp_pilot_set) <- NULL

  exp_analysis_set <- test_dat[-pilot_inds, ] %>%
    dplyr::mutate(stratum = rep(3:1, each = 4))
  rownames(exp_analysis_set) <- NULL

  expect_is(a.strat, "auto_strata")
  expect_is(a.strat, "strata")

  expect_equal(a.strat$analysis_set, exp_analysis_set)

  expect_equal(a.strat$treat, "treated")

  expect_equal(toString(a.strat$call),
               "auto_stratify, test_dat, treated, outcome ~ cont, outcome, 4, 0.5")

  exp_issue_table <- data.frame(Stratum = 1:3,
                                Treat = c(3, 2, 3),
                                Control = c(1, 2, 1),
                                Total = as.integer(rep(4, 3)),
                                Control_Proportion = c(0.25, 0.5, 0.25),
                                Potential_Issues = rep("Too few samples", 3),
                                stringsAsFactors = FALSE)

  expect_equal(a.strat$issue_table, exp_issue_table)

  exp_strata_table <- dplyr::tibble(stratum = 1:3,
                                    quantile_bin = factor(c("[0.189,0.216)",
                                                            "[0.216,0.240)",
                                                            "[0.240,0.271]")),
                                    size = as.integer(rep(4, 3)))

  expect_equal(a.strat$strata_table, exp_strata_table)

  expect_equal(a.strat$outcome, "outcome")

  exp_prog_model <- glm(exp_pilot_set,
                        formula = outcome ~ cont,
                        family = binomial)

  exp_prog_scores <- predict(exp_prog_model,
                             exp_analysis_set, type = "response")

  expect_equal(a.strat$prog_scores, exp_prog_scores)

  expect_equal(coef(a.strat$prog_model), coef(exp_prog_model))

  expect_equal(a.strat$pilot_set, exp_pilot_set)
})

test_that("auto_stratify with prognostic formula + pilot_sample works", {
  test_dat <- make_test_data()

  a.strat <- auto_stratify(test_dat,
                           treat = "treated",
                           outcome = "outcome",
                           prognosis = outcome ~ cont,
                           pilot_sample = test_dat, # bad practice; ok for tests
                           size = 4)

  exp_analysis_set <- test_dat %>%
    dplyr::mutate(stratum = rep(4:1, each = 4))

  expect_is(a.strat, "auto_strata")
  expect_is(a.strat, "strata")

  expect_equal(a.strat$analysis_set, exp_analysis_set)

  expect_equal(a.strat$treat, "treated")

  expect_equal(toString(a.strat$call),
               "auto_stratify, test_dat, treated, outcome ~ cont, outcome, 4, test_dat")

  exp_issue_table <- data.frame(Stratum = 1:4,
                                Treat = rep(2, 4),
                                Control = rep(2, 4),
                                Total = as.integer(rep(4, 4)),
                                Control_Proportion = rep(0.5, 4),
                                Potential_Issues = rep("Too few samples", 4),
                                stringsAsFactors = FALSE)

  expect_equal(a.strat$issue_table, exp_issue_table)

  expect_known_value(a.strat$strata_table,
                     file = "ref_strata_table", update = F)

  expect_equal(a.strat$outcome, "outcome")

  exp_prog_model <- glm(test_dat,
                        formula = outcome ~ cont,
                        family = binomial)

  exp_prog_scores <- predict(exp_prog_model,
                             exp_analysis_set, type = "response")

  expect_equal(a.strat$prog_scores, exp_prog_scores)

  expect_equal(coef(a.strat$prog_model), coef(exp_prog_model))

  expect_equal(a.strat$pilot_set, test_dat)
})


test_that("auto_stratify with prognostic model works", {
  test_dat <- make_test_data()

  progmod <- glm(outcome ~ cont, test_dat, family = "binomial")

  a.strat <- auto_stratify(test_dat,
                           treat = "treated",
                           outcome = "outcome",
                           prognosis = progmod,
                           size = 4)

  exp_analysis_set <- test_dat %>%
    dplyr::mutate(stratum = rep(4:1, each = 4))

  expect_is(a.strat, "auto_strata")
  expect_is(a.strat, "strata")

  expect_equal(a.strat$analysis_set, exp_analysis_set)

  expect_equal(a.strat$treat, "treated")

  expect_equal(toString(a.strat$call),
               "auto_stratify, test_dat, treated, progmod, outcome, 4")

  exp_issue_table <- data.frame(Stratum = 1:4,
                                Treat = rep(2, 4),
                                Control = rep(2, 4),
                                Total = as.integer(rep(4, 4)),
                                Control_Proportion = rep(0.5, 4),
                                Potential_Issues = rep("Too few samples", 4),
                                stringsAsFactors = FALSE)

  expect_equal(a.strat$issue_table, exp_issue_table)

  expect_known_value(a.strat$strata_table,
                     file = "ref_strata_table", update = F)

  expect_equal(a.strat$outcome, "outcome")

  exp_prog_scores <- predict(progmod,
                             exp_analysis_set, type = "response")

  expect_equal(a.strat$prog_scores, exp_prog_scores)

  expect_equal(coef(a.strat$prog_model), coef(progmod))

  expect_equal(a.strat$pilot_set, NULL)
})
