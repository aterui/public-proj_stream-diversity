
# setup -------------------------------------------------------------------

  rm(list = ls(all.names = TRUE))
  pacman::p_load(tidyverse)
  pacman::p_load(tidyverse, patchwork)
  
# empirical data ----------------------------------------------------------
  
  source(here::here("code_empirical/figure_diversity.R"))


# theoretical data --------------------------------------------------------

  dat <- read_csv(here::here("code_theory/result/result_sim2021-05-28.csv")) %>% 
    filter(alpha_div > 0 & gamma_div > 0,
           p_dispersal == 0.01,
           sd_env_source == 1,
           sd_env_lon == 0.01) %>% 
    pivot_longer(cols = c("alpha_div", "beta_div", "gamma_div"),
                 names_to = "metric") %>% 
    mutate(competition = recode(max_alpha,
                                `0.75` = sprintf('"Weak competition"~(b[max]=="%.2f")',
                                                 max_alpha),
                                `1.5` = sprintf('"Strong competition"~(b[max]=="%.2f")', 
                                                max_alpha)),
           dispersal = recode(theta,
                              `0.1` = sprintf('"Long-distance dispersal"~(theta=="%.2f")',
                                              theta),
                              `1.0` = sprintf('"Short-distance dispersal"~(theta=="%.2f")',
                                              theta)))
  
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


# ecosystem size effect ---------------------------------------------------

  g <- dat %>% 
    ggplot(aes(x = n_patch, y = value, color = metric, fill = metric)) +
    geom_smooth(method = "loess", size = 0.5) +
    scale_y_continuous(trans = "log10") +
    scale_x_continuous(trans = "log10") +
    scale_color_hue(name = NULL, labels = labels) +
    facet_grid(rows = vars(competition), cols = vars(dispersal), labeller = label_parsed) +
    labs(x = "Number of habitat patches",
         y = "Species richness") +
    guides(color = guide_legend(override.aes = list(fill = NA)),
           fill = FALSE)
  
  print(g + ggtitle("Theoretical prediction") +
        g1 + ggtitle("Empirical observation") +
        plot_annotation(tag_levels = 'A', title = "Figure 2") + 
        plot_layout(guides = "collect", width = c(2,1)))
  
  ggsave(normalizePath("../document_output/figure_02.pdf"),
         width = 10,
         height = 6)