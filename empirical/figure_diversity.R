
# setup -------------------------------------------------------------------

  rm(list = ls(all.names = TRUE))
  pacman::p_load(tidyverse, sf, patchwork)
  setwd(here::here("empirical"))

# set plot theme ----------------------------------------------------------
  
  plt_theme <- theme_bw() + theme(
    plot.background = element_blank(),
    
    panel.background = element_rect(grey(0.99)),
    panel.border = element_rect(),
    
    panel.grid = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    
    strip.background = element_blank(),
    strip.text.x = element_text(size = 10),
    strip.text.y = element_text(size = 10),
    axis.title = element_text(size = 10)
  )
  
  theme_set(plt_theme)
  

# read data ---------------------------------------------------------------
  
  ## model estimate
  source(normalizePath("../empirical/analysis_lm.R"))
  
  ## redefine data.frame
  dat_hkd <- read_csv("data_out/data_hkd.csv") %>% mutate(region = "hokkaido")
  dat_mw <- read_csv("data_out/data_mw.csv") %>% mutate(region = "midwest")
  
  dat <- bind_rows(dat_hkd, dat_mw) %>% 
    rename(gamma = Estimator, alpha = mu_alpha) %>% 
    mutate(beta = gamma/alpha,
           logit_forest = log(frac_forest) - log(1 - frac_forest)) %>% 
    mutate(resid_forest = resid(lm(logit_forest ~ region, data = .)),
           resid_temp = resid(lm(mean_temp ~ region, data = .)),
           resid_ppt = resid(lm(mean_ppt ~ region, data = .))) %>% 
    pivot_longer(cols = c(alpha, beta, gamma),
                 names_to = "metric")
  
  ## data.frame for prediction
  f_area <- 10^mean(log(dat$area, 10))
  x_area <- rep(seq(min(dat$area), max(dat$area), length = 100), times = 2)
  
  f_p_branch = 10^mean(log(dat$p_branch, 10))
  x_p_branch = rep(seq(min(dat$p_branch), max(dat$p_branch), length = 100), times = 2)
  
  dat_base <- data.frame(region = factor(rep(c("hokkaido", "midwest"), each = 100)),
                         resid_temp = mean(dat$resid_temp),
                         resid_ppt = mean(dat$resid_ppt),
                         resid_forest = mean(dat$resid_forest))
  
  dat_area <- data.frame(area = x_area, p_branch = f_p_branch, dat_base)
  dat_bp <- data.frame(area = f_area, p_branch = x_p_branch, dat_base)
  
  ### area
  dat_area$alpha <- 10^predict(m_alpha$model, dat_area)
  dat_area$beta <- 10^predict(m_beta$model, dat_area)
  dat_area$gamma <- 10^predict(m_gamma$model, dat_area)
  dat_area <- dat_area %>%  
    pivot_longer(cols = c(alpha, beta, gamma),
                 names_to = "metric")
  
  ### p_branch
  dat_bp$alpha <- 10^predict(m_alpha$model, dat_bp)
  dat_bp$beta <- 10^predict(m_beta$model, dat_bp)
  dat_bp$gamma <- 10^predict(m_gamma$model, dat_bp)
  dat_bp <- dat_bp %>%  
    pivot_longer(cols = c(alpha, beta, gamma),
                 names_to = "metric")
  
# fig ---------------------------------------------------------------------
  
  g1 <- ggplot(dat, aes(x = area, y = value, color = region)) +
    facet_wrap(facets = ~metric, labeller = label_parsed) +
    geom_point(alpha = 0.25) +
    geom_line(data = dat_area, aes(x = area, y = value)) +
    scale_color_hue(name = "Region", labels = c("Hokkaido", "Midwest")) +
    xlab(expression("Watershed area ("~km^2~")")) +
    scale_y_continuous(trans='log10') +
    scale_x_continuous(trans='log10')
    
  g2 <- ggplot(dat, aes(x = p_branch, y = value, color = region)) +
    facet_wrap(facets = ~metric, labeller = label_parsed) +
    geom_point(alpha = 0.25) +
    geom_line(data = dat_bp, aes(x = p_branch, y = value)) +
    scale_color_hue(name = "Region", labels = c("Hokkaido", "Midwest")) +
    xlab("Branching probability") +
    scale_y_continuous(trans='log10') +
    scale_x_continuous(trans='log10')
  
  print(  
    ((g1 + theme(legend.position = "none") + ylab("Species richness")))/(g2 + ylab("Species richness")) + plot_layout(guides = "collect")
  )  