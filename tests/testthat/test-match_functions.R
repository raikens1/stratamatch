context("Matching with Stratification")

# Make Test Data
#
# Makes a simple data frame with treat, outcome, a categorical and a continouos
# covariate. Outcome = sigmoid(treat + cont)
make_test_data <- function(n = 16){
  set.seed(123)
  
  data.frame(treat = rep(c(0,1), n/2),
             cat = rep(c(0,1,2,3), each = n/4),
             cont = seq(from = 0, to = 1, length.out = n)) %>%
    dplyr::mutate(outcome = rbinom(n = n, size = 1, p = 1/(1+exp(treat + cont))))
}

test_that("matching works", {
  test_dat <- make_test_data() %>% dplyr::mutate(cat = c(rep(1:3, 5), 1))
  m.strat <- manual_stratify(test_dat, "treat", c("cat"))
  
  m.match1 <- big_match(m.strat, treat ~ cat + cont, k = 1)
  
  expect_known_value(m.match1, "ref_match1")
  
  # resample controls to make a data set for 1:2 matching
  # bad practice; just for testing.
  test_dat_big <- rbind(test_dat,
                        dplyr::filter(test_dat, treat == 0), 
                        dplyr::filter(test_dat, treat == 0)) 
  
  m.strat_big <- manual_stratify(test_dat_big, "treat", c("cat"))
  m.match2 <- big_match(m.strat_big, treat ~ cat + cont, k = 2)
  
  expect_known_value(m.match2, "ref_match2")
})
