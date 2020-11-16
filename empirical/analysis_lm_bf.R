
# setup -------------------------------------------------------------------

  rm(list = ls(all.names = TRUE))
  pacman::p_load(tidyverse)
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
    
    mod1 <<- lm(log(response, 10) ~ log(area, 10)*region + log(p_branch, 10)*region + region +
                                    scale(resid_temp) + scale(resid_ppt) + scale(resid_forest),
                data = data)
    
    mod0 <<- lm(log(response, 10) ~ log(area, 10) + log(p_branch, 10) + region +
                                    scale(resid_temp) + scale(resid_ppt) + scale(resid_forest),
                    data = data)
    
    # bf01: bayes factor in favor of mod0 over mod1
    bf01 <- exp(0.5*(BIC(mod1) - BIC(mod0)))
    
    if(bf01 >= 1) m <- mod0 else m <- mod1
    
    return(list(model = m, bayes_factor = bf01))
    
  }
  
  fit <- lapply(dat[,c("alpha", "beta", "gamma")], FUN = m_compare, data = dat)
  