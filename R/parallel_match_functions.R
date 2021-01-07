#----------------------------------------------------------
### CONTAINS:
# matching functions previously under development which may be useful later
# This file is ignored in .Rbuildignore
#----------------------------------------------------------

#----------------------------------------------------------
### Helpers
#----------------------------------------------------------

#' Match One
#'
#' Not meant to be called externally. Match one dataset using the optmatch
#' package
#'
#' @param dat a data.frame with observations as rows, outcome column masked
#' @param propensity_model a \code{glm} object with the propensity model
#' @param treat string, the name of the treatment assignment column
#' @param k the number of controls to be matched to each treated individual
#' @return a data.frame like dat with pair assignments column
#' @keywords internal
match_one <- function(dat, propensity_model, treat, k) {
  dist_matrix <- make_distance_matrix(dat,
    propensity_model = propensity_model,
    treat = treat
  )
  dat$match_id <- optmatch::pairmatch(dist_matrix, data = dat, controls = k)
  return(dat)
}

#' Make Distance Matrix
#'
#' Not meant to be called externally. Makes the distance matrix to be passed to
#' pairmatch. Similar to \code{match_on.glm} in \code{optmatch} except that the
#' model need not have been fit on the data we are matching.
#'
#' @param dat a data.frame of observations to be matched
#' @param propensity_model a \code{glm} object modeling propensity scores
#' @param treat string, the name of the treatment assignment column
#' @return a matrix of distances to be passed to opmatch::pairmatch()
#' @keywords internal
make_distance_matrix <- function(dat, propensity_model, treat) {
  z <- dat[[treat]]
  print(length(z))
  lp <- predict(propensity_model, dat)
  pooled.sd <- sqrt(((sum(!z) - 1) * mad(lp[!z])^2 +
    (sum(!!z) - 1) * mad(lp[!!z])^2) / (length(lp) - 2))

  return(optmatch::match_on(x = lp / pooled.sd, z = z, rescale = F))
}

#----------------------------------------------------------
### Matching Methods
#----------------------------------------------------------

#' Strata Match Dopar
#'
#' Still in development. Match within strata in parallel by calling match_one
#' with dopar. Doesn't work right now.
#'
#' @inheritParams strata_match
#' @return a data.frame with pair assignments
#' @keywords internal
strata_match_dopar <- function(strat, propensity_formula = NULL, k = 1) {
  check_inputs_matcher(strat, propensity_formula, k)

  if (is.null(propensity_formula)) {
    propensity_formula <- formula(paste(c(
      strat$treat, "~ . -", strat$outcome,
      "- stratum"
    ), collapse = ""))
  }
  # build propensity model
  propensity_model <- glm(propensity_formula,
    data = strat$analysis_set,
    family = binomial()
  )

  # set up cluster for dopar
  num_cores <- parallel::detectCores()
  registerDoParallel(num_cores)

  # just use do for now so we can debug
  foreach(i = as.character(unique(a.strat1$analysis_set$stratum))) %do% {
    dplyr::filter(strat$analysis_set, stratum == i) %>%
      match_one(.,
        propensity_model = propensity_model,
        treat = strat$treat, k
      )
  }
  stopImplicitCluster()
}


#' Strata Match Multidplyr
#'
#' Still in development. Match within strata in parallel by calling match_one
#' with multidplyr.  Currently buggy, but returns a result.
#'
#' @inheritParams strata_match
#'
#' @return a data.frame like dat with pair assignments
#' @keywords internal
strata_match_multidplyr <- function(strat, propensity_formula = NULL, k = 1) {
  check_inputs_matcher(strat, propensity_formula, k)

  t1 <- proc.time()
  if (is.null(propensity_formula)) {
    propensity_formula <- formula(paste(c(
      strat$treat, "~ . -", strat$outcome,
      "- stratum"
    ), collapse = ""))
  }

  print(propensity_formula)

  # build propensity model
  propensity_model <- glm(propensity_formula,
    data = strat$analysis_set,
    family = binomial()
  )

  # set up multidplyr cluster
  num_cores <- parallel::detectCores()
  treat <- strat$treat
  cluster <- multidplyr::new_cluster(num_cores)
  multidplyr::cluster_assign(cluster,
    propensity_model = propensity_model,
    match_on = optmatch::match_on,
    pairmatch = optmatch::pairmatch,
    match_one = match_one,
    make_distance_matrix = make_distance_matrix,
    treat = treat,
    k = k
  )
  # match in parallel
  # result <- strat$analysis_set %>%
  #  group_by(stratum) %>%
  #  partition(cluster = cluster) %>%
  #  do(match_one(., propensity_model = propensity_model, treat = treat, k = k)) %>% collect()

  stp1 <- dplyr::group_by(strat$analysis_set, stratum)

  stp2 <- multidplyr::partition(stp1, cluster = cluster)

  print(is(stp2))
  print(methods(do))

  stp3 <- do(stp2, match_one(.,
    propensity_model = propensity_model,
    treat = treat,
    k = k
  ))

  result <- multidplyr::collect(stp3)

  # parallel::stopCluster(cluster)

  return(result)
}
