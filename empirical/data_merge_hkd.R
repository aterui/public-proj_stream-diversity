
# setup -------------------------------------------------------------------
  
  rm(list = ls(all.names = TRUE))
  setwd(here::here("empirical"))
  pacman::p_load(tidyverse, tidyverse, sf, iNEXT, foreach, magrittr)  
  
# read gis data -----------------------------------------------------------

  watershed <- st_read(dsn = 'data_gis/albers_watershed_hkd_final.gpkg') %>% 
    rename(watershed_id = id) %>% 
    arrange(watershed_id)
  
# merge fish and gis data -------------------------------------------------

  d0 <- read_csv('data_org_hkd/data_fmt_hkd_latest.csv') %>% 
    rename(Species = OTU) %>% 
    st_as_sf(coords = c("Lon", "Lat"), crs = 4326) %>% 
    st_transform(crs = st_crs(watershed)$wkt) %>% 
    filter(Year >= 1990) %>% 
    st_join(watershed)
  
  n_site_stat <- d0 %>%
    as_tibble() %>% 
    group_by(watershed_id) %>% 
    summarise(n_site = n_distinct(SiteID)) %>% 
    right_join(as_tibble(watershed), by = "watershed_id") %>% 
    select(watershed_id, n_site, area, n_branch) %>% 
    mutate(density_site = n_site/area)
  
  ## watershed ID with more than X site
  ## watershed ID with more than X branch
  wsd_candidate <- n_site_stat %>% 
    filter(n_site >= 10 & n_branch >= 10) %>% 
    pull(watershed_id)
  
  dat_fish <- d0 %>% 
    filter(watershed_id %in% wsd_candidate) %>% 
    as_tibble()

  dat_alpha <- dat_fish %>% 
    group_by(watershed_id, SiteID) %>% 
    summarise(alpha_div = n_distinct(Species)) %>% 
    group_by(watershed_id) %>% 
    summarise(mu_alpha = mean(alpha_div))
  

# iNEXT -------------------------------------------------------------------

  ## convert data to frequency
  dat_freq <- dat_fish %>% 
    group_by(watershed_id, Species) %>% 
    summarise(freq = n()) %>% 
    pivot_wider(id_cols = watershed_id,
                names_from = Species,
                values_from = freq,
                values_fill = list(freq = 0))
  watershed_id <- pull(dat_freq, watershed_id) 
  
  dat_freq <- dat_fish %>% 
    group_by(watershed_id) %>% 
    summarise(n_site = n_distinct(SiteID)) %>% 
    left_join(dat_freq, by = "watershed_id") %>% 
    select(-watershed_id)
  
  list_freq <- foreach(i = seq_len(nrow(dat_freq))) %do% {
    x <- as.vector(sort(dat_freq[i,], decreasing = T))
    return(x[x > 0])
  }
    
  names(list_freq) <- watershed_id
  
  ## iNEXT: select watershed with 90% coverage
  full_est <- iNEXT(list_freq, datatype = "incidence_freq")
  
  wsd_subset <- full_est[[1]] %>% 
    filter(SC > 0.9) %>% 
    pull(site)
  
  div_est <- full_est[[3]] %>% 
    filter(Diversity == "Species richness" & Site %in% wsd_subset) %>% 
    mutate(watershed_id = as.numeric(as.character(Site)))
  
# final data --------------------------------------------------------------
  
  dat_hkd <- div_est %>%
    left_join(as_tibble(watershed), by = "watershed_id") %>% 
    left_join(dat_alpha, by = "watershed_id") %>% 
    select(-geom)
  
  write_csv(dat_hkd, "data_out/data_hkd.csv")
  
  ## export species list
  splist <- d0 %>% 
    filter(watershed_id %in% wsd_subset) %>% 
    as_tibble() %>% 
    distinct(Species)
    
  write_csv(splist, "data_out/data_hkd_splist.csv")
  
  ## export watershed polygon
  albers_wsd_subset <- watershed %>% 
    filter(watershed_id %in% wsd_subset)
  
  st_write(albers_wsd_subset, "data_gis/albers_wsd_subset_hkd.gpkg", append = FALSE)
  
  ## export point data
  albers_point <- d0 %>% 
    filter(watershed_id %in% wsd_subset) %>% 
    group_by(SiteID) %>% 
    distinct(SiteID)
  
  st_write(albers_point, "data_gis/albers_point_subset_hkd.gpkg", append = FALSE)
  