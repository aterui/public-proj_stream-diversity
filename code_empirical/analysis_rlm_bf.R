
# setup -------------------------------------------------------------------

pacman::p_load(tidyverse)

# read data ---------------------------------------------------------------

dat_hkd <- read_csv(here::here("code_empirical/data_out/data_hkd.csv")) %>%
  mutate(region = "hokkaido")
dat_mw <- read_csv(here::here("code_empirical/data_out/data_mw.csv")) %>%
  mutate(region = "midwest")

dat <- bind_rows(dat_hkd, dat_mw) %>% 
  rename(gamma = Estimator,
         alpha = mu_alpha) %>% 
  mutate(beta = gamma / alpha,
         dam_density = n_dam / area) %>% 
  mutate(scl_resid_agri = c(scale(resid(lm(frac_agri ~ region, data = .)))),
         scl_resid_temp = c(scale(resid(lm(mean_temp ~ region, data = .)))),
         scl_resid_ppt = c(scale(resid(lm(mean_ppt ~ region, data = .)))),
         scl_resid_dam = c(scale(resid(lm(dam_density ~ region, data = .)))))


# model selection ---------------------------------------------------------

m_compare <- function(response, data) {
  
  mod1 <- MASS::rlm(log(response, 10) ~ log(area, 10) * region + log(p_branch, 10) * region + region +
                      scl_resid_temp + scl_resid_ppt + scl_resid_agri + scl_resid_dam,
                    psi = MASS::psi.huber,
                    method = 'M',
                    data = data)
  
  mod0 <- MASS::rlm(log(response, 10) ~ log(area, 10) + log(p_branch, 10) + region +
                      scl_resid_temp + scl_resid_ppt + scl_resid_agri + scl_resid_dam,
                    psi = MASS::psi.huber,
                    method = 'M',
                    data = data)
  
  # bf01: bayes factor in favor of mod0 over mod1
  bf01 <- exp(0.5*(BIC(mod1) - BIC(mod0)))
  
  if(bf01 >= 1) m <- mod0 else m <- mod1
  
  return(list(model = m, bayes_factor = bf01))
  
}

fit <- lapply(dat[,c("alpha", "beta", "gamma")], FUN = m_compare, data = dat)

