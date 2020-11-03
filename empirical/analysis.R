
# setup -------------------------------------------------------------------

  rm(list = ls(all.names = TRUE))
  pacman::p_load(tidyverse, MuMIn)
  options(na.action = "na.fail")
  setwd(here::here("empirical"))

# read data ---------------------------------------------------------------

  dat_hkd <- read_csv("data_out/data_hkd.csv") %>% mutate(region = "hokkaido")
  dat_mw <- read_csv("data_out/data_mw.csv") %>% mutate(region = "midwest")
  dat <- bind_rows(dat_hkd, dat_mw) %>% 
    rename(gamma = Estimator,
           gamma_lower = '95% Lower',
           gamma_upper = '95% Upper') %>% 
    mutate(beta = gamma/mu_alpha,
           beta_lower = gamma_lower/mu_alpha,
           beta_upper = gamma_upper/mu_alpha)
  
  ## gamma
  fit <- lm(log(gamma) ~ log(area)*region + log(p_branch)*region +
                         scale(mean_temp) + scale(mean_ppt) + frac_forest, dat)
  m <- dredge(fit, rank = "AIC")
  re <- model.avg(object = m, subset = delta < 2)
  summary(re)
  
  ## alpha
  fit <- lm(log(mu_alpha) ~ log(area)*region + log(p_branch)*region +
              scale(mean_temp) + scale(mean_ppt) + frac_forest, dat)
  m <- dredge(fit, rank = "AIC")
  re <- model.avg(object = m, subset = delta < 2)
  summary(re)
  
  ## beta
  fit <- lm(log(beta) ~ log(area)*region + log(p_branch)*region +
              scale(mean_temp) + scale(mean_ppt) + frac_forest, dat)
  m <- dredge(fit, rank = "AIC")
  re <- model.avg(object = m, subset = delta < 2)
  summary(re)
  
  