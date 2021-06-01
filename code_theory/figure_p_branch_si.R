
# setup -------------------------------------------------------------------
  
  pacman::p_load(tidyverse)
  
  param <- expand.grid(sigma_l = c(0.01, 1), sigma_h = c(1, 0.01), p_d = c(0.1, 0.01)) %>% 
    filter(!(p_d == 0.01 & sigma_h > sigma_l))
  
  g <- list(NULL)
  
for(i in seq_len(nrow(param))){
  
# read data ---------------------------------------------------------------
  
  dat <- read_csv(here::here("code_theory/result/result_sim2021-05-28.csv")) %>% 
    filter(alpha_div > 0 & gamma_div > 0,
           p_dispersal == param$p_d[i],
           sd_env_source == param$sigma_h[i],
           sd_env_lon == param$sigma_l[i]) %>% 
    pivot_longer(cols = c("alpha_div", "beta_div", "gamma_div"),
                 names_to = "metric") %>% 
    mutate(competition = recode(max_alpha,
                                `0.75` = sprintf('"Weak competition"~(alpha[max]=="%.2f")', max_alpha),
                                `1.5` = sprintf('"Strong competition"~(alpha[max]=="%.2f")', max_alpha)),
           dispersal = recode(theta,
                              `0.1` = sprintf('"Long-distance dispersal"~(theta=="%.2f")', theta),
                              `1.0` = sprintf('"Short-distance dispersal"~(theta=="%.2f")', theta)))
  
  labels <- c(expression(alpha~"diversity"),
              expression(beta~"diversity"),
              expression(gamma~"diversity"))


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


# complexity effect -------------------------------------------------------

  g[[i]] <- dat %>% 
    ggplot(aes(x = p_branch, y = value, color = metric, fill = metric)) +
    geom_smooth(method = "loess", size = 0.5) +
    scale_y_continuous(trans = "log10") +
    scale_x_continuous(trans = "log10", breaks = c(0.01, 0.1, 0.5, 1.0)) +
    scale_color_hue(name = NULL, labels = labels) +
    facet_grid(rows = vars(competition), cols = vars(dispersal), labeller = label_parsed) +
    labs(x = "Branching probability",
         y = "Species richness") +
    guides(color = guide_legend(override.aes = list(fill = NA)),
           fill = FALSE)
  
}
