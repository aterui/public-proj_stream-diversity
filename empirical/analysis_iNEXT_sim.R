
# setup -------------------------------------------------------------------

  rm(list = ls(all.names = TRUE))
  pacman::p_load(tidyverse, MuMIn, foreach)
  options(na.action = "na.fail")
  setwd(here::here("empirical"))
  


# simulation function -----------------------------------------------------

  f_sim <- function(n_species, n_site, p_min = 0, p_max = 0.8){
    p <- runif(n = n_species, p_min, p_max)
    freq <- sapply(seq_len(n_species), FUN = function(x) rbinom(n = n_site, prob = p[x], size = 1))
    x <- c(n_site, sort(colSums(freq), decreasing = T))
    
    y <- iNEXT(x, datatype = "incidence_freq")  
    return(y[[3]][,"Estimator"][1])
  }
  

# simulation --------------------------------------------------------------

  n_species <- seq(10, 100, by = 30)
  n_site <- c(seq(5, 20, by = 5), 100)
  n_rep <- 100
  para <- expand.grid(n_species = n_species, n_site = n_site)
  
  sim_re <- foreach(i = seq_len(nrow(para)), .combine = bind_rows) %do% {
    df_set <- foreach(j = seq_len(n_rep), .combine = bind_rows) %do% {
      est <- f_sim(n_species = para$n_species[i], n_site = para$n_site[i])
      df <- tibble(replicate = j,
                   estimate = est,
                   n_species = para$n_species[i],
                   n_site = para$n_site[i])
      return(df)
    }
    return(df_set)
  }
    
  sim_re <- sim_re %>% 
    mutate(bias = 100*((estimate - n_species) / n_species))
  
  sim_summary <- sim_re %>% 
    group_by(n_species, n_site) %>% 
    summarise(median_bias = median(bias),
              iql_bias = quantile(bias, probs = 0.75) - quantile(bias, probs = 0.25))
  

# plot --------------------------------------------------------------------
  
  sim_re %>% 
    ggplot(aes(y = bias, x = factor(n_site))) +
    facet_wrap(facets = ~ n_species) +
    geom_boxplot(alpha = 0.5) +
    geom_jitter(alpha = 0.5, size = 0.5) +
    geom_hline(yintercept = 0)
  