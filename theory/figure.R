

# setup -------------------------------------------------------------------

  library(tidyverse)
  library(broom)
  library(here)
  setwd(here("theory"))

# read data ---------------------------------------------------------------

  dat <- read_csv("result/result_sim2020-10-24.csv") %>% 
    select(-beta_div) %>% 
    filter(alpha_div > 0 & gamma_div > 0,
           sd_env_source == 1,
           sd_env_lon == 0.01) %>% 
    mutate(beta_div = gamma_div/alpha_div) %>% 
    pivot_longer(cols = c("alpha_div", "beta_div", "gamma_div"),
                 names_to = "metric") %>% 
    mutate(competition = recode(max_alpha,
                                `0.75` = "Weak competition",
                                `1.5` = "Strong competition"))

  dat$disp_label <- sprintf('Dispersal prob. = %.2f', dat$p_dispersal)
  dat$disp_label <- factor(dat$disp_label, levels = unique(dat$disp_label))
  
  labels <- c(expression(alpha), expression(beta), expression(gamma))
  

# set plot theme ----------------------------------------------------------

  plt_theme <- theme_bw() + theme(
    plot.background = element_blank(),
    
    panel.background = element_rect(grey(0.95)),
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
  
  
# ecosystem size effect ---------------------------------------------------
    
  dat %>% 
    ggplot(aes(x = n_patch, y = value, color = metric, fill = metric)) +
    geom_smooth(method = "loess", size = 0.3) +
    geom_point(alpha = 0.075, size = 0.3) +
    scale_y_continuous(trans = "log10") +
    scale_x_continuous(trans = "log10") +
    scale_color_hue(name = NULL, labels = labels) +
    facet_grid(rows = vars(competition), cols = vars(disp_label)) +
    labs(x = "Number of habitat patches",
         y = "Species richness") +
    guides(color = guide_legend(override.aes = list(fill = NA)),
           fill = FALSE)
  
  ggsave("fig_n_patch.pdf")
  
# complexity effect -------------------------------------------------------

  dat %>% 
    ggplot(aes(x = p_branch, y = value, color = metric, fill = metric)) +
    geom_smooth(method = "loess", size = 0.3) +
    geom_point(alpha = 0.075, size = 0.3) +
    scale_y_continuous(trans = "log10") +
    scale_x_continuous(trans = "log10") +
    scale_color_hue(name = NULL, labels = labels) +
    facet_grid(rows = vars(competition), cols = vars(disp_label)) +
    labs(x = "Branching probability",
         y = "Species richness") +
    guides(color = guide_legend(override.aes = list(fill = NA)),
           fill = FALSE)
  
  ggsave("fig_p_branch.pdf")
  