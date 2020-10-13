
# setup -------------------------------------------------------------------

  library(tidyverse)
  library(here)
  library(MuMIn)
  options(na.action = "na.fail")
  setwd(here("empirical"))

# read data ---------------------------------------------------------------

  dat_hkd <- read_csv("data_out/data_hkd.csv") %>% mutate(region = "hokkaido")
  dat_mw <- read_csv("data_out/data_mw.csv") %>% mutate(region = "midwest")
  dat <- bind_rows(dat_hkd, dat_mw) %>% 
    rename(gamma = Estimator,
           gamma_lower = '95% Lower',
           gamma_upper = '95% Upper') %>% 
    mutate(beta = gamma - mu_alpha,
           beta_lower = gamma_lower - mu_alpha,
           beta_upper = gamma_upper - mu_alpha)
  
  fit <- lm(log(gamma) ~ log(area)*region + p_branch*region + p_branch*log(area) +
                         I(p_urban + p_agri) + scale(mu_temp) + scale(mu_prec), dat)
  
  fit_upper <- lm(log(gamma_upper) ~ log(area)*region + p_branch*region + p_branch*log(area) +
                                     I(p_urban + p_agri) + scale(mu_temp) + scale(mu_prec), dat)
  
  fit_lower <- lm(log(gamma_lower) ~ log(area)*region + p_branch*region + p_branch*log(area) +
                                     I(p_urban + p_agri) + scale(mu_temp) + scale(mu_prec), dat)
  
  m <- dredge(fit)
  re <- model.avg(object = m, subset = delta < 2)
  summary(re)
  
  ggplot(dat) +
    geom_point(aes(x = p_branch, y = log(gamma), color = region))
  
  ggplot(dat) +
    geom_point(aes(x = log(area), y = log(gamma), color = region))
  
  ggplot(dat) +
    geom_point(aes(x = p_branch, y = log(beta), color = region))

  ggplot(dat) +
    geom_point(aes(x = log(area), y = log(beta), color = region))
  