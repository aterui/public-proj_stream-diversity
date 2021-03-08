
# setup -------------------------------------------------------------------

  rm(list = ls(all.names = TRUE))
  pacman::p_load(tidyverse, knitr, kableExtra)
  setwd(here::here("empirical"))


# average predictive comparison -------------------------------------------

  source("analysis_apcomp.R")
  
  table_output <- ap_est %>% 
    rename(Input = input,
           Estimate = sim_estimate,
           SE = sim_se) %>% 
    mutate(Input = c("Watershed area", "Branching probability"),
           factor = c(1000, 0.1)) %>% 
    mutate(Estimate = round(factor * Estimate, 2),
           SE = round(factor * SE, 2)) %>% 
    mutate(Unit = c("1000 km^2^", "0.1")) %>% 
    select(-factor) %>% 
    kable(format = "markdown")
  