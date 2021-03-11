
# setup -------------------------------------------------------------------

  pacman::p_load(tidyverse, knitr, kableExtra)
  dat_hkd_site <- read_csv(here::here('empirical/data_out/data_hkd_site.csv'))

# table -------------------------------------------------------------------

  table_output <- dat_hkd_site %>% 
    mutate(n_site = n_distinct(SiteID)) %>% 
    mutate(Species = str_replace_all(str_to_title(Species), "_", " ")) %>% 
    mutate(Species = str_replace_all(Species, "(spp$)|(spp\\s)", "spp\\.")) %>% 
    mutate(Species = str_replace_all(Species, "(sp$)", "sp\\.")) %>% 
    mutate(Species = str_replace_all(Species, "(subsp)", "subsp\\. ")) %>% 
    mutate(Species = str_replace_all(Species, "(sp\\sme$)", "sp\\. ME")) %>% 
    group_by(Species) %>% 
    summarise('Number of sites present' = n_distinct(SiteID),
              'Occupancy (%)' = round((n_distinct(SiteID)*100)/unique(n_site), 2)) %>% 
    kable(format = 'markdown')
