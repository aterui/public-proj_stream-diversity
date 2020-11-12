
# setup -------------------------------------------------------------------

  rm(list = ls(all.names = TRUE))
  pacman::p_load(tidyverse, MuMIn, MASS)
  options(na.action = "na.fail")
  setwd(here::here("empirical"))

# read data ---------------------------------------------------------------
  
  dat_hkd <- read_csv("data_out/data_hkd.csv") %>% mutate(region = "hokkaido")
  dat_mw <- read_csv("data_out/data_mw.csv") %>% mutate(region = "midwest")
  
  dat <- bind_rows(dat_hkd, dat_mw) %>% 
    rename(gamma = Estimator,
           alpha = mu_alpha) %>% 
    mutate(beta = gamma/alpha,
           logit_forest = log(frac_forest) - log(1 - frac_forest)) %>% 
    mutate(resid_forest = resid(lm(logit_forest ~ region, data = .)),
           resid_temp = resid(lm(mean_temp ~ region, data = .)),
           resid_ppt = resid(lm(mean_ppt ~ region, data = .)))
  

# model selection ---------------------------------------------------------

  m_compare <- function(response, data) {
    
    fit <<- list(NULL)
    fit[[1]] <<- lm(log(response, 10) ~ log(area, 10) + log(p_branch, 10) + region +
                      scale(resid_temp) + scale(resid_ppt) + scale(resid_forest),
                    data = data)
    
    fit[[2]] <<- lm(log(response, 10) ~ log(area, 10)*region + log(p_branch, 10)*region + region +
                      scale(resid_temp) + scale(resid_ppt) + scale(resid_forest),
                    data = data)
    
    fit[[3]] <<- lm(log(response, 10) ~ 1,
                    data = data)
    
    m <- fit[[which.min(sapply(fit, FUN = BIC))]]
    df <- tibble(model = c("global", "region-specific", "null"),
                 AIC = sapply(fit, FUN = AIC),
                 AICc = sapply(fit, FUN = AICc),
                 BIC = sapply(fit, FUN = BIC))
    
    return(list(model = m, model_table = df))
  }
  
  ## alpha 
  m_alpha <- m_compare(dat$alpha, data = dat)
  
  ## beta
  m_beta <- m_compare(dat$beta, data = dat)
  
  ## gamma
  m_gamma <- m_compare(dat$gamma, data = dat)
  