
# setup -------------------------------------------------------------------

  rm(list = ls(all.names = TRUE))
  pacman::p_load(tidyverse, iNEXT, foreach)
  options(na.action = "na.fail")
  setwd(here::here("empirical"))


# simulation function -----------------------------------------------------

  f_sim <- function(n_species, n_site,
                    p_min = 0.3, p_max = 0.8,
                    kappa_min = 0, kappa_max = 1
                    ){
    p <- runif(n = n_species, p_min, p_max)
    m_kappa <- matrix(runif(n = n_species*n_site, min = kappa_min, max = kappa_max), nrow = n_species, ncol = n_site)
    zeta <- c(foreach(i = seq_len(n_species), .combine = rbind) %do% {p[i]*m_kappa[i,]})
    
    incidence <- rbinom(n = n_species*n_site, size = 1, prob = zeta)
    m_incidence <- matrix(incidence, nrow = n_species, ncol = n_site)
    freq <- c(n_site, sort(rowSums(m_incidence), decreasing = T))
    
    
    y <- iNEXT(freq, datatype = "incidence_freq")  
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
  
  write_csv(sim_re, here::here("empirical/result/result_sim_inext.csv"))
  