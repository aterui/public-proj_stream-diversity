
# setup -------------------------------------------------------------------

  rm(list = ls(all.names = TRUE))
  pacman::p_load(tidyverse)
  setwd(here::here("code_empirical"))


# plot theme --------------------------------------------------------------

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

  sim_re <- read_csv(here::here("code_empirical/result/result_sim_inext.csv")) %>% 
    mutate(n_species_lab = recode(n_species,
                                  `10` = sprintf('"S"[true]=="%.0f"', n_species),
                                  `40` = sprintf('"S"[true]=="%.0f"', n_species),
                                  `70` = sprintf('"S"[true]=="%.0f"', n_species),
                                  `100` = sprintf('"S"[true]=="%.0f"', n_species))) %>% 
    mutate(n_species_lab = factor(.$n_species_lab,
                                  levels = unique(.$n_species_lab)[order(.$n_species)]))
  
  
  g <- sim_re %>% 
    ggplot(aes(y = bias, x = factor(n_site))) +
    facet_wrap(facets = ~ n_species_lab, labeller = label_parsed) +
    geom_boxplot(alpha = 0.5, outlier.shape = NA) +
    geom_jitter(alpha = 0.5, size = 0.5) +
    geom_hline(yintercept = 0 ,alpha = 0.5) +
    ylab('% bias') +
    xlab('Number of sites')
  
  print(g)
  