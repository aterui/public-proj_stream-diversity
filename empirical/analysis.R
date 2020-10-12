
# setup -------------------------------------------------------------------

  library(tidyverse)
  library(here)
  library(MuMIn)
  options(na.action = "na.fail")
  setwd(here("empirical"))

# read data ---------------------------------------------------------------

  dat_hkd <- read_csv("data_out/data_hkd.csv") %>% mutate(region = "hokkaido")
  dat_mw <- read_csv("data_out/data_mw.csv") %>% mutate(region = "midwest")
  dat <- bind_rows(dat_hkd, dat_mw)
  
  fit <- lm(log(Estimator) ~ log(area)*region + p_branch*region + p_branch*log(area) +
                             I(p_urban + p_agri) + scale(mu_temp) + scale(mu_prec),
                             dat)
  models <- dredge(fit)  
  
  
  ggplot(dat) +
    geom_point(aes(x = p_branch, y = log(Estimator), color = region))
  
  ggplot(dat) +
    geom_point(aes(x = log(area), y = log(Estimator), color = region))
  
  ggplot(dat) +
    geom_point(aes(x = p_branch, y = log(Estimator - mu_alpha), color = region))
  