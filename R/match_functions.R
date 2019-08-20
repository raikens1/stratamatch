
#----------------------------------------------------------
### Match
#----------------------------------------------------------

#' @title Match One
#' @description Match one dataset using the optmatch package
#' @param dat a data.frame with observations as rows, outcome column masked
#' @return a data.frame like dat with pair assignments column
match_one <- function(dat, propensity_model, treat, k = 1){
  dist_matrix <- make_distance_matrix(dat,
                                      propensity_model = propensity_model,
                                      treat = treat)
  dat$match_id <- optmatch::pairmatch(dist_matrix, data = dat, controls = k)
  return(dat)
}

#' @title Make distance matrix
#' @description Makes the distance matrix to be passed to pairmatch. Similar to match_on.glm in optmatch
#' except that the model need not have been fit on the data we are matching
#' @param dat a data.frame of observations to be matched
#' @param propensity_model a glm object modeling propensity scores
#' @return a matrix of distances to be passed to pairmatch()
make_distance_matrix <- function(dat, propensity_model, treat){
  names(dat)[names(dat) == treat] <- "treat"
  z <- dat$treat
  print(length(z))
  lp <- predict(propensity_model, dat)
  pooled.sd <- sqrt( ( (sum(!z) - 1) * mad(lp[!z]) ^ 2 +
                         (sum(!!z) - 1) * mad(lp[!!z]) ^ 2) / (length(lp) - 2))

  return(optmatch::match_on(x = lp / pooled.sd, z = z, rescale = F))
}

#' @title Big Match dopar
#' @description Match within strata in parallel by calling match_one with dopar.
#' Doesn't work right now 
#' @param strat a strata object
#' @return a data.frame like dat with pair assignments?
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
  num_cores <- detectCores()
  registerDoParallel(num_cores)

  # just use do for now so we can debug
  foreach(i = as.character(unique(a.strat1$analysis_set$stratum))) %do% {
    dplyr::filter(strat$analysis_set, stratum == i) %>%
      match_one(., propensity_model = propensity_model, treat = strat$treat)

  }
  stopImplicitCluster()
}


#' @title Big Match v2
#' @description Match within strata in parallel by calling match_one
#' @param strat a strata object
#' @param propensity_formula the formula for the propensity score
#' @return a data.frame like dat with pair assignments?
big_match_multidplyr <- function(strat, propensity_formula = NULL) {
  if (is.null(propensity_formula)){
    propensity_formula <- formula(paste(c(strat$treat, "~ . -", strat$outcome,
                                          "- stratum"), collapse = ""))
  }
  # build propensity model
  propensity_model <- glm(propensity_formula,
                          data = strat$analysis_set,
                          family = binomial())

  # set up multidplyr cluster
  num_cores <- detectCores()
  treat <- strat$treat
  cluster <- multidplyr::create_cluster(num_cores)
  multidplyr::cluster_assign_value(cluster,
                                   "propensity_model", propensity_model)
  multidplyr::cluster_assign_value(cluster,
                                   "match_one", match_one)
  multidplyr::cluster_assign_value(cluster,
                                   "make_distance_matrix",
                                   make_distance_matrix)
  multidplyr::cluster_assign_value(cluster,
                                   "match_on", optmatch::match_on)
  multidplyr::cluster_assign_value(cluster,
                                   "pairmatch", optmatch::pairmatch)
  multidplyr::cluster_assign_value(cluster,
                                   "treat", treat)

  # match in parallel
  result <- strat$analysis_set %>%
    partition(stratum, cluster = cluster) %>%
    do(match_one(., propensity_model = propensity_model, treat = treat)) %>%
    collect()

  return(result)
}

#' @title Big Match
#' @description Match within strata in series using optmatch
#' @param strat a strata object
#' @param propensity_formula (optional) formula for propensity score
#' @param k numeric, the number of control individuals to be matched to each treated individual
#' @return a named factor with matching assignments
big_match <- function(strat, propensity_formula = NULL, k = 1){
  if (is.null(propensity_formula)){
    # match on all variables, stratified by stratum
    propensity_formula <- formula(paste(strat$treat, "~ . -", strat$outcome,
                                        "- stratum", "+ strata(stratum)"))
  } else {
    # append phrase to stratify by stratum
    orig_form <- Reduce(paste, deparse(propensity_formula))
    propensity_formula <- formula(paste(orig_form, "+ strata(stratum)"))
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

#' @title Big Match (not stratified)
#' @description For performance testing purposes, match using optmatch but do not stratify
#' @param strat a strata object
#' @param propensity_formula (optional) formula for propensity score
#' @param k numeric, the number of control individuals to be matched to each treated individual
#' @return a named factor with matching assignments
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