
# setup -------------------------------------------------------------------

  pacman::p_load(tidyverse, MASS)

# read data ---------------------------------------------------------------
  
  dat_hkd <- read_csv(here::here("empirical/data_out/data_hkd.csv")) %>% mutate(region = "hokkaido")
  dat_mw <- read_csv(here::here("empirical/data_out/data_mw.csv")) %>% mutate(region = "midwest")
  
  dat <- bind_rows(dat_hkd, dat_mw) %>% 
    rename(gamma = Estimator,
           alpha = mu_alpha) %>% 
    mutate(beta = gamma/alpha,
           logit_agri = log(frac_agri) - log(1 - frac_agri),
           dam_density = n_dam/area) %>% 
    mutate(resid_agri = resid(lm(logit_agri ~ region, data = .)),
           resid_temp = resid(lm(mean_temp ~ region, data = .)),
           resid_ppt = resid(lm(mean_ppt ~ region, data = .)),
           resid_dam = resid(lm(dam_density ~ region, data = .)))
  

# model selection ---------------------------------------------------------

  m_compare <- function(response, data) {
    
    mod1 <<- rlm(log(response, 10) ~ log(area, 10)*region + log(p_branch, 10)*region + region +
                                     scale(resid_temp) + scale(resid_ppt) + scale(resid_agri) + scale(resid_dam),
                 psi = psi.huber,
                 method = 'M',
                 data = data)
    
    mod0 <<- rlm(log(response, 10) ~ log(area, 10) + log(p_branch, 10) + region +
                                     scale(resid_temp) + scale(resid_ppt) + scale(resid_agri) + scale(resid_dam),
                 method = 'M',
                 psi = psi.huber,
                 data = data)
    
    # bf01: bayes factor in favor of mod0 over mod1
    bf01 <- exp(0.5*(BIC(mod1) - BIC(mod0)))
    
    if(bf01 >= 1) m <- mod0 else m <- mod1
    
    return(list(model = m, bayes_factor = bf01))
    
  }
  
  fit <- lapply(dat[,c("alpha", "beta", "gamma")], FUN = m_compare, data = dat)
  

# detach MASS (to avoid conflicts with tidyverse) -------------------------

  detach('package:MASS', unload = TRUE)
  