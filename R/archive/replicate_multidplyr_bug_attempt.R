my_slow_mean <- function(x){
  Sys.sleep(length(x))
  mean(x)
}

big_df <- data.frame(group = rep(1:3, each = 10), X = rnorm(30))

num_cores <- parallel::detectCores()
cluster <- multidplyr::new_cluster(num_cores)
multidplyr::cluster_assign(cluster, my_slow_mean = my_slow_mean)

t1 <- proc.time()
# mean in parallel
result <- big_df %>%
  group_by(group) %>%
  partition(cluster = cluster) %>%
  summarise(m = my_slow_mean(X)) %>% collect()
t2 <- proc.time() - t1

print(t2)

mdplyr_function <- function(big_df){
  t1 <- proc.time()
  # mean in parallel
  result <- big_df %>%
    group_by(group) %>%
    partition(cluster = cluster) %>%
    summarise(m = my_slow_mean(X)) %>% collect()
  t2 <- proc.time() - t1

  print(t2)
}

mdplyr_function(big_df)
