context("Matching with Stratification")

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

test_that("matching works", {
  test_dat <- make_test_data() %>% dplyr::mutate(cat = c(rep(1:3, 5), 1))
  m.strat <- manual_stratify(test_dat, treated ~ cat)

  m.match1 <- strata_match(m.strat, treated ~ cont, k = 1)

  expect_known_value(m.match1, "ref_match1")

  # resample controls to make a data set for 1:2 matching
  # bad practice; just for testing.
  test_dat_big <- rbind(test_dat,
                        dplyr::filter(test_dat, treated == 0),
                        dplyr::filter(test_dat, treated == 0))

  m.strat_big <- manual_stratify(test_dat_big, treated ~ cat)
  m.match2 <- strata_match(m.strat_big, treated ~ cont, k = 2)

  expect_known_value(m.match2, "ref_match2")
})


test_that("matching errors work", {
  test_dat <- make_test_data() %>% dplyr::mutate(cat = c(rep(1:3, 5), 1))
  m.strat <- manual_stratify(test_dat, treated ~ cat)

  expect_error(strata_match(m.strat, "soup", k = 1),
               "propensity must be a formula")
  expect_error(strata_match(m.strat, zombies ~ cat + cont, k = 1),
               "not all variables in propensity formula appear in data")
  expect_error(strata_match(m.strat, cat ~ treated + cont, k = 1),
               "propensity formula must model treatment assignment")
  expect_error(strata_match(m.strat, treated ~ cat + cont, k = "socks"),
               "k must be an integer")
  expect_error(strata_match(m.strat, treated ~ cat + cont, k = 0),
               "k must be 1 or greater")
  expect_error(strata_match("soup", treated ~ cat + cont, k = 1),
               "strat must be a strata object")
})
