
# setup -------------------------------------------------------------------

  rm(list = ls(all.names = TRUE))
  pacman::p_load(tidyverse, sf, patchwork)
  setwd(here::here("empirical"))

# fig: gamma --------------------------------------------------------------

  ## read data
  dat_hkd <- read_csv("data_out/data_hkd.csv") %>% mutate(region = "hokkaido")
  dat_mw <- read_csv("data_out/data_mw.csv") %>% mutate(region = "midwest")
  
  dat <- bind_rows(dat_hkd, dat_mw) %>% 
    rename(gamma = Estimator,
           gamma_lower = '95% Lower',
           gamma_upper = '95% Upper') %>% 
    mutate(beta = gamma/mu_alpha,
           beta_lower = gamma_lower/mu_alpha,
           beta_upper = gamma_upper/mu_alpha)
  
  ## set theme
  theme_set(theme_bw())
  
  ## plot
  ggplot(dat, aes(x = p_branch, y = gamma, color = region)) +
    scale_y_continuous(trans='log10') +
    scale_x_continuous(trans='log10') +
    geom_point() +
    geom_smooth(method = "lm", se = F)
  
  ggplot(dat, aes(x = frac_forest, y = gamma, color = region)) +
    scale_y_continuous(trans='log10') +
    geom_point() +
    geom_smooth(method = "lm", se = F)
  
  ggplot(dat, aes(x = area, y = gamma, color = region)) +
    scale_y_continuous(trans='log10') +
    scale_x_continuous(trans='log10') +
    geom_point() +
    geom_smooth(method = "lm", se = F)
  
  ggplot(dat, aes(x = p_branch, y = mu_alpha, color = region)) +
    scale_y_continuous(trans='log10') +
    scale_x_continuous(trans='log10') +
    geom_point() +
    geom_smooth(method = "lm", se = F)
  
  ggplot(dat, aes(x = p_branch, y = beta, color = region)) +
    scale_y_continuous(trans='log10') +
    scale_x_continuous(trans='log10') +
    geom_point() +
    geom_smooth(method = "lm", se = F)
  
