
# setup -------------------------------------------------------------------

  pacman::p_load(tidyverse, knitr, kableExtra)
  dat_mw_site <- read_csv(here::here('code_empirical/data_out/data_mw_site.csv'))

# table -------------------------------------------------------------------

  sp_mw_latin <- read_csv(here::here('code_empirical/data_out/data_mw_splist_latin.csv'))
  
  table_output <- dat_mw_site %>% 
    mutate(n_site = n_distinct(SiteID)) %>% 
    rename(Common_name = Species) %>% 
    group_by(Common_name) %>% 
    summarise('Number of sites present' = n_distinct(SiteID),
              'Occupancy (%)' = round((n_distinct(SiteID)*100)/unique(n_site), 2)) %>% 
    left_join(sp_mw_latin, by = "Common_name") %>% 
    select(Species = Latin_name,
           'Number of sites present',
           'Occupancy (%)') %>% 
    arrange(Species) %>% 
    kable(format = 'markdown')