
# library -----------------------------------------------------------------

  rm(list = ls(all.names = TRUE))
  pacman::p_load(here, tidyverse)

  param <- expand.grid(sigma_l = c(0.01, 1), sigma_h = c(1, 0.01))
  
  h <- list(NULL)
  
for(i in seq_len(nrow(param))){

# data --------------------------------------------------------------------

  dat <- read_csv(here::here("theory/result/result_sim_alpha_pattern2021-05-28.csv")) %>% 
    filter(sd_env_source == param$sigma_h[i],
           sd_env_lon == param$sigma_l[i]) %>%
    mutate(competition = recode(max_alpha,
                                `0.75` = sprintf('"Weak competition"~(alpha[max]=="%.2f")',
                                                 max_alpha),
                                `1.5` = sprintf('"Strong competition"~(alpha[max]=="%.2f")',
                                                max_alpha)),
           dispersal = recode(theta,
                              `0.1` = sprintf('"Long-distance dispersal"~(theta=="%.2f")',
                                              theta),
                              `1.0` = sprintf('"Short-distance dispersal"~(theta=="%.2f")',
                                              theta)))
  
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
  
  

# plot --------------------------------------------------------------------

  h[[i]] <- dat %>% 
    ggplot(aes(x = n_patch_upstream, y = alpha_div,
               color = factor(p_dispersal),
               fill = as.character(p_dispersal))) +
    geom_smooth(method = "loess", size = 0.5) +
    scale_y_continuous(trans = "log10") +
    scale_x_continuous(trans = "log10") +
    scale_color_hue(name = 'Dispersal prob.') +
    facet_grid(rows = vars(competition),
               cols = vars(dispersal),
               labeller = label_parsed) +
    labs(x = "Number of upstream habitat patches",
         y = expression("Species richness at each habitat patch")) +
    guides(color = guide_legend(override.aes = list(fill = NA)),
           fill = FALSE)
  
}