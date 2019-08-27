#----------------------------------------------------------
### CONTAINS: 
# Matching methods to be called on strata objects, and their helpers
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
match_one <- function(dat, propensity_model, treat, k = 1){
  dist_matrix <- make_distance_matrix(dat,
                                      propensity_model = propensity_model,
                                      treat = treat)
  dat$match_id <- optmatch::pairmatch(dist_matrix, data = dat, controls = k)
  return(dat)
}

#' Make Distance Matrix
#'
#' Not meant to be called externally. Makes the distance matrix to be passed to
#' pairmatch. Similar to match_on.glm in optmatch except that the model need not
#' have been fit on the data we are matching
#'
#' @param dat a data.frame of observations to be matched
#' @param propensity_model a \code{glm} object modeling propensity scores
#' @param treat string, the name of the treatment assignment column
#' @return a matrix of distances to be passed to opmatch::pairmatch()
make_distance_matrix <- function(dat, propensity_model, treat){
  names(dat)[names(dat) == treat] <- "treat"
  z <- dat$treat
  print(length(z))
  lp <- predict(propensity_model, dat)
  pooled.sd <- sqrt( ( (sum(!z) - 1) * mad(lp[!z]) ^ 2 +
                         (sum(!!z) - 1) * mad(lp[!!z]) ^ 2) / (length(lp) - 2))

  return(optmatch::match_on(x = lp / pooled.sd, z = z, rescale = F))
}

#----------------------------------------------------------
### Matching Methods
#----------------------------------------------------------

#' Big Match Dopar
#'
#' Still in development. Match within strata in parallel by calling match_one
#' with dopar. Doesn't work right now.
#'
#' @param strat a strata object
#' @param propensity_formula a formula for the propensity score.  If
#'   unspecified, all columns except outcome and strata are used.
#' @return a data.frame with pair assignments
big_match_dopar <- function(strat, propensity_formula = NULL) {
  if (is.null(propensity_formula)){
    propensity_formula <- formula(paste(c(strat$treat, "~ . -", strat$outcome,
                                          "- stratum"), collapse = ""))
  }
  # build propensity model
  propensity_model <- glm(propensity_formula,
                          data = strat$analysis_set,
                          family = binomial())

  # set up cluster for dopar
  num_cores <- parallel::detectCores()
  registerDoParallel(num_cores)

  # just use do for now so we can debug
  foreach(i = as.character(unique(a.strat1$analysis_set$stratum))) %do% {
    dplyr::filter(strat$analysis_set, stratum == i) %>%
      match_one(., propensity_model = propensity_model, treat = strat$treat)

  }
  stopImplicitCluster()
}


#' Big Match Multidplyr
#'
#' Still in development. Match within strata in parallel by calling match_one
#' with multidplyr.  Currently buggy, but returns a result.
#'
#' @param strat a strata object
#' @param propensity_formula the formula for the propensity score
#' @return a data.frame like dat with pair assignments
#' @export
big_match_multidplyr <- function(strat, propensity_formula = NULL) {
  library(multidplyr)
  t1 <- proc.time()
  if (is.null(propensity_formula)){
    propensity_formula <- formula(paste(c(strat$treat, "~ . -", strat$outcome,
                                          "- stratum"), collapse = ""))
  }
  
  print(propensity_formula)
  
  # build propensity model
  propensity_model <- glm(propensity_formula,
                          data = strat$analysis_set,
                          family = binomial())

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
                             treat = treat)
  # match in parallel
  #result <- strat$analysis_set %>%
  #  group_by(stratum) %>%
  #  partition(cluster = cluster) %>%
  #  do(match_one(., propensity_model = propensity_model, treat = treat)) %>% collect()
  
  stp1 <- group_by(strat$analysis_set, stratum)
  
  stp2 <- multidplyr::partition(stp1, cluster = cluster)
  
  print(is(stp2))
  print(methods(do))
  
  stp3 <- do(stp2, match_one(., propensity_model = propensity_model, treat = treat))
  
  result <- collect(stp3)
  
  #parallel::stopCluster(cluster)

  return(proc.time()-t1)
}

#' Big Match
#'
#' Match within strata in series using optmatch.  Probably needs to be renamed.
#'
#' @param strat a strata object
#' @param propensity_formula (optional) formula for propensity score
#' @param k numeric, the number of control individuals to be matched to each
#'   treated individual
#' @return a named factor with matching assignments
#' @export
big_match <- function(strat, propensity_formula = NULL, k = 1){
  if (is.null(propensity_formula)){
    # match on all variables, stratified by stratum
    propensity_formula <- formula(paste(strat$treat, "~ . -", strat$outcome,
                                        "- stratum",
                                        "+ strata(stratum)"))
  } else {
    # append phrase to stratify by stratum
    orig_form <- Reduce(paste, deparse(propensity_formula))
    propensity_formula <- formula(paste(orig_form,
                                        "+ strata(stratum)"))
  }

  print(propensity_formula)
  # build propensity model
  propensity_model <- glm(propensity_formula,
                          data = strat$analysis_set,
                          family = binomial())

  return(optmatch::pairmatch(propensity_model,
                             data = strat$analysis_set,
                             controls = k))
}

#' Big Match - Not Stratified
#'
#' Not meant to be called externally, but exported currently for convenience.
#' This function is for performance testing purposes, so that we can compare the
#' speed of matching within strata to matching the entire dataset without
#' stratification.
#'
#' @param strat a strata object
#' @param propensity_formula (optional) formula for propensity score
#' @param k numeric, the number of control individuals to be matched to each
#'   treated individual
#' @return a named factor with matching assignments
#' @export
big_match_nstrat <- function(strat, propensity_formula = NULL, k = 1){
  if (is.null(propensity_formula)){
    # match on all variables, stratified by stratum
    propensity_formula <- formula(paste(strat$treat, "~ . -", strat$outcome,
                                        "- stratum"))
  } else {
    # do not modify original formula to append "+ strata(stratum)"
  }

  print(propensity_formula)
  # build propensity model
  propensity_model <- glm(propensity_formula,
                          data = strat$analysis_set,
                          family = binomial())

  return(optmatch::pairmatch(propensity_model,
                             data = strat$analysis_set,
                             controls = k))
}
