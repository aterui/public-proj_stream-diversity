
# setup -------------------------------------------------------------------

  rm(list = ls(all.names = TRUE))
  setwd(here::here("code_empirical"))
  pacman::p_load(tidyverse, corrplot)
  

# data --------------------------------------------------------------------

  dat_hkd <- read_csv(here::here("code_empirical/data_out/data_hkd.csv")) %>% mutate(region = "hokkaido")
  dat_mw <- read_csv(here::here("code_empirical/data_out/data_mw.csv")) %>% mutate(region = "midwest")
  
  dat <- bind_rows(dat_hkd, dat_mw) %>% 
    rename(gamma = Estimator,
           alpha = mu_alpha) %>% 
    mutate(beta = gamma/alpha,
           dam_density = n_dam/area) %>% 
    mutate(resid_temp = resid(lm(mean_temp ~ region, data = .)),
           resid_ppt = resid(lm(mean_ppt ~ region, data = .)),
           resid_dem = resid(lm(mean_dem ~ region, data = .)),
           resid_forest = resid(lm(frac_forest ~ region, data = .)),
           resid_urban = resid(lm(frac_urban ~ region, data = .)),
           resid_agri = resid(lm(frac_agri ~ region, data = .)),
           resid_dam = resid(lm(dam_density ~ region, data = .)))  
  
  M <- dat %>% 
    select('Watershed area' = area,
           'Branching prob.' = p_branch,
           'Temperature' = resid_temp,
           'Precipitation' = resid_ppt,
           'Elevation' = resid_dem,
           'Fraction of forest' = resid_forest,
           'Fraction of urban' = resid_urban,
           'Fraction of agriculture' = resid_agri,
           'Dam density' = resid_dam) %>% 
    cor()
  
  corrplot(M, type="upper", method = 'color', addCoef.col = "black", diag = FALSE, tl.col = "black")  
  