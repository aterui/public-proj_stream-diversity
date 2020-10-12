

# setup -------------------------------------------------------------------

  library(here)
  library(tidyverse)
  setwd(here("theory"))


# read data ---------------------------------------------------------------

  dat <- read_csv("result/result_sim2020-10-01.csv")
  
  dat %>% 
    filter(K_type %in% "equal",
           sd_env_lon %in% c(0.01,1),
           sd_env_source %in% c(0.01, 1),
           asymmetry_factor == 1.5) %>% 
    group_by(n_patch,
             p_branch,
             p_dispersal,
             max_alpha,
             sd_env_lon,
             sd_env_source) %>% 
    mutate(n = n()) %>% 
    summarise(across(ends_with("div"), .fns = list(mean = mean, sd = sd,
                                                   median = median,
                                                   q25 = ~ quantile(.x, probs = 0.25),
                                                   q75 = ~ quantile(.x, probs = 0.75))), n = unique(n)) %>% 
    mutate(lower_alpha = alpha_div_mean - (alpha_div_sd / sqrt(n)),
           upper_alpha = alpha_div_mean + (alpha_div_sd / sqrt(n)),
           lower_beta = beta_div_mean - (beta_div_sd / sqrt(n)),
           upper_beta = beta_div_mean + (beta_div_sd / sqrt(n)),
           lower_gamma = gamma_div_mean - (gamma_div_sd / sqrt(n)),
           upper_gamma = gamma_div_mean + (gamma_div_sd / sqrt(n))) %>% 
    ungroup() -> dat_plot


# p_branch plot -----------------------------------------------------------

  # alpha
    dat_plot %>% 
      ggplot(aes(x = p_branch, y = alpha_div_mean, color = n_patch, fill = n_patch, group = n_patch)) +
      geom_line(size = 0.5) +
      geom_ribbon(aes(ymin = lower_alpha, ymax = upper_alpha), color = NA, alpha = 0.25) +
      scale_color_viridis_c() +
      scale_fill_viridis_c() +
      facet_grid(rows = vars(max_alpha, sd_env_lon), cols = vars(p_dispersal, sd_env_source),
                 labeller = labeller(.rows = label_both, .cols = label_both))
  
    ggsave("alpha_div.pdf", width = 9, height = 9)
  
  # beta
    dat_plot %>% 
      ggplot(aes(x = p_branch, y = beta_div_mean, color = n_patch, fill = n_patch, group = n_patch)) +
      geom_line(size = 0.5) +
      geom_ribbon(aes(ymin = lower_beta, ymax = upper_beta), color = NA, alpha = 0.25) +
      scale_color_viridis_c() +
      scale_fill_viridis_c() +
      facet_grid(rows = vars(max_alpha, sd_env_lon), cols = vars(p_dispersal, sd_env_source),
                 labeller = labeller(.rows = label_both, .cols = label_both))
    
    ggsave("beta_div.pdf", width = 9, height = 9)
  
  # gamma
    dat_plot %>% 
      ggplot(aes(x = p_branch, y = gamma_div_mean, color = n_patch, fill = n_patch, group = n_patch)) +
      geom_line(size = 0.5) +
      geom_ribbon(aes(ymin = lower_gamma, ymax = upper_gamma), color = NA, alpha = 0.25) +
      scale_color_viridis_c() +
      scale_fill_viridis_c() +
      facet_grid(rows = vars(max_alpha, sd_env_lon), cols = vars(p_dispersal, sd_env_source),
                 labeller = labeller(.rows = label_both, .cols = label_both))
    
    ggsave("gamma_div.pdf", width = 9, height = 9)
  

# n_patch plot ------------------------------------------------------------
    
  # alpha
    dat_plot %>% 
      ggplot(aes(x = n_patch, y = alpha_div_mean, color = p_branch, fill = p_branch, group = p_branch)) +
      geom_line(size = 0.5) +
      geom_ribbon(aes(ymin = lower_alpha, ymax = upper_alpha), color = NA, alpha = 0.25) +
      scale_color_viridis_c() +
      scale_fill_viridis_c() +
      facet_grid(rows = vars(max_alpha, sd_env_lon), cols = vars(p_dispersal, sd_env_source),
                 labeller = labeller(.rows = label_both, .cols = label_both))
    
    ggsave("alpha_div_n_patch.pdf", width = 9, height = 9)
    
    dat_plot %>% 
        ggplot(aes(x = n_patch, y = beta_div_mean, color = p_branch, fill = p_branch, group = p_branch)) +
        geom_line(size = 0.5) +
        geom_ribbon(aes(ymin = lower_beta, ymax = upper_beta), color = NA, alpha = 0.25) +
        scale_color_viridis_c() +
        scale_fill_viridis_c() +
        facet_grid(rows = vars(max_alpha, sd_env_lon), cols = vars(p_dispersal, sd_env_source),
                   labeller = labeller(.rows = label_both, .cols = label_both))
    
    ggsave("beta_div_n_patch.pdf", width = 9, height = 9)
    
    dat_plot %>% 
        ggplot(aes(x = n_patch, y = gamma_div_mean, color = p_branch, fill = p_branch, group = p_branch)) +
        geom_line(size = 0.5) +
        geom_ribbon(aes(ymin = lower_gamma, ymax = upper_gamma), color = NA, alpha = 0.25) +
        scale_color_viridis_c() +
        scale_fill_viridis_c() +
        facet_grid(rows = vars(max_alpha, sd_env_lon), cols = vars(p_dispersal, sd_env_source),
                   labeller = labeller(.rows = label_both, .cols = label_both))
    
    ggsave("gamma_div_n_patch.pdf", width = 9, height = 9)
    