
# setup -------------------------------------------------------------------

  rm(list = ls(all.names = TRUE))
  pacman::p_load(tidyverse, sf, patchwork)
  setwd(here::here("code_empirical"))

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
  source("analysis_rlm_bf.R")
  
  ## redefine data.frame
  dat_hkd <- read_csv("data_out/data_hkd.csv") %>%
    mutate(region = "hokkaido")
  dat_mw <- read_csv("data_out/data_mw.csv") %>%
    mutate(region = "midwest")
  
  dat <- bind_rows(dat_hkd, dat_mw) %>% 
    rename(gamma = Estimator, alpha = mu_alpha) %>% 
    mutate(beta = gamma/alpha) %>% 
    pivot_longer(cols = c(alpha, beta, gamma),
                 names_to = "metric")
  
  dat_base <- data.frame(region = factor(rep(c("hokkaido", "midwest"), each = 100)),
                         scl_resid_temp = 0,
                         scl_resid_ppt = 0,
                         scl_resid_agri = 0,
                         scl_resid_dam = 0)

# data frame for prediction -----------------------------------------------
  
  f_area <- 10^mean(log(dat$area, 10))
  f_p_branch = 10^mean(log(dat$p_branch, 10))
  
  ## area prediction
  dat_area <- dat %>% 
    group_by(region) %>% 
    summarise(area = seq(min(area), max(area), length = 100)) %>% 
    left_join(dat_base, by = "region") %>% 
    mutate(p_branch = f_p_branch)
  
  dat_bp <- dat %>% 
    group_by(region) %>% 
    summarise(p_branch = seq(min(p_branch), max(p_branch), length = 100)) %>% 
    left_join(dat_base, by = "region") %>% 
    mutate(area = f_area)
  
  ### area
  dat_area$alpha <- 10^predict(fit$alpha$model, newdata = dat_area)
  dat_area$beta <- 10^predict(fit$beta$model, dat_area)
  dat_area$gamma <- 10^predict(fit$gamma$model, dat_area)
  dat_area <- dat_area %>%  
    pivot_longer(cols = c(alpha, beta, gamma), names_to = "metric")
  
  ### p_branch
  dat_bp$alpha <- 10^predict(fit$alpha$model, dat_bp)
  dat_bp$beta <- 10^predict(fit$beta$model, dat_bp)
  dat_bp$gamma <- 10^predict(fit$gamma$model, dat_bp)
  dat_bp <- dat_bp %>%  
    pivot_longer(cols = c(alpha, beta, gamma), names_to = "metric")
  
# fig ---------------------------------------------------------------------
  
  region_label <- c("Hokkaido", "Midwest")
  names(region_label) <- c("hokkaido", "midwest")
  
  g1 <- ggplot(dat, aes(x = area,
                        y = value,
                        color = metric)) +
    facet_wrap(facets = ~ region,
               labeller = labeller(region = region_label),
               ncol = 1) +
    geom_point(alpha = 0.25) +
    geom_line(data = dat_area,
              aes(x = area,
                  y = value)) +
    scale_color_hue(name = NULL,
                    labels = c(expression(alpha~"diversity"),
                               expression(beta~"diversity"),
                               expression(gamma~"diversity"))) +
    xlab(expression("Watershed area ("~km^2~")")) +
    scale_y_continuous(trans='log10') +
    scale_x_continuous(trans='log10') +
    ylab("Species richness") +
    theme(legend.position = "none")
  
  g2 <- ggplot(dat, aes(x = p_branch,
                        y = value,
                        color = metric)) +
    facet_wrap(facets = ~ region,
               labeller = labeller(region = region_label),
               ncol = 1) +
    geom_point(alpha = 0.25) +
    geom_line(data = dat_bp,
              aes(x = p_branch,
                  y = value,
                  linetype = metric)) +
    scale_color_hue(name = NULL,
                    labels = c(expression(alpha~"diversity"),
                               expression(beta~"diversity"),
                               expression(gamma~"diversity"))) +
    scale_linetype_manual(values = c("dashed", "solid", "solid"),
                          guide = "none") +
    xlab("Branching probability") +
    scale_y_continuous(trans='log10') +
    scale_x_continuous(trans='log10') +
    ylab("Species richness") +
    theme(legend.position = "none")
    
  #print(  
  #  (g1 + theme(legend.position = "none") + ylab("Species richness"))/
  #  (g2 + ylab("Species richness")) + plot_layout(guides = "collect") +
  #  plot_annotation(tag_levels = 'A')
  #)  