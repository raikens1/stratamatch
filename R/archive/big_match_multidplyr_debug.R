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

stp2 <- partition(stp1, cluster = cluster)

print(is(stp2))
print(methods(do))

stp3 <- do(stp2, match_one(., propensity_model = propensity_model, treat = treat))

result <- collect(stp3)

#parallel::stopCluster(cluster)

print(proc.time()-t1)