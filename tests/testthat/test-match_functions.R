context("Matching with Stratification")

# Make Test Data
#
# Makes a simple data frame with treat, outcome, a categorical and a continouos
# covariate. Outcome = sigmoid(treat + cont)
make_test_data <- function(n = 16){
  set.seed(123)

  data.frame(treat = rep(c(0, 1), n / 2),
             cat = rep(c(0, 1, 2, 3), each = n / 4),
             cont = seq(from = 0, to = 1, length.out = n)) %>%
    dplyr::mutate(outcome = rbinom(n = n,
                                   size = 1,
                                   p = 1 / (1 + exp(treat + cont))))
}

test_that("matching works", {
  test_dat <- make_test_data() %>% dplyr::mutate(cat = c(rep(1:3, 5), 1))
  m.strat <- manual_stratify(test_dat, treat ~ cat)

  m.match1 <- big_match(m.strat, treat ~ cont, k = 1)

  expect_known_value(m.match1, "ref_match1")

  # resample controls to make a data set for 1:2 matching
  # bad practice; just for testing.
  test_dat_big <- rbind(test_dat,
                        dplyr::filter(test_dat, treat == 0),
                        dplyr::filter(test_dat, treat == 0))

  m.strat_big <- manual_stratify(test_dat_big, treat ~ cat)
  m.match2 <- big_match(m.strat_big, treat ~ cont, k = 2)

  expect_known_value(m.match2, "ref_match2")
})


test_that("matching errors work", {
  test_dat <- make_test_data() %>% dplyr::mutate(cat = c(rep(1:3, 5), 1))
  m.strat <- manual_stratify(test_dat, treat ~ cat)

  expect_error(big_match(m.strat, "soup", k = 1),
               "propensity must be a formula")
  expect_error(big_match(m.strat, zombies ~ cat + cont, k = 1),
               "not all variables in propensity formula appear in data")
  expect_error(big_match(m.strat, cat ~ treat + cont, k = 1),
               "propensity formula must model treatment assignment")
  expect_error(big_match(m.strat, treat ~ cat + cont, k = "socks"),
               "k must be an integer")
  expect_error(big_match(m.strat, treat ~ cat + cont, k = 0),
               "k must be 1 or greater")
  expect_error(big_match("soup", treat ~ cat + cont, k = 1),
               "strat must be a strata object")
})
