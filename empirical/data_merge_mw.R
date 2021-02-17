
# setup -------------------------------------------------------------------
  
  rm(list = ls(all.names = TRUE))
  setwd(here::here("empirical"))
  pacman::p_load(tidyverse, sf, iNEXT, foreach, tmap)  

# read gis data -----------------------------------------------------------

  watershed <- st_read(dsn = 'data_gis/albers_watershed_mw_final.gpkg') %>% 
    rename(watershed_id = id) %>% 
    arrange(watershed_id)
  
# merge fish and gis data -------------------------------------------------
  
  fishdata <- list.files(path = "data_org_mw", full.names = TRUE) %>%
    lapply(read_csv)
  
  ## IA: wgs84; MN, WI, IL: NAD83
  d0 <- do.call(what = bind_rows, args = fishdata) %>% 
    st_as_sf(coords = c("Lon", "Lat"), crs = 4326) %>% 
    st_transform(crs = st_crs(watershed)$wkt) %>% 
    st_join(watershed)
  
  ## checking for CRS definition influence
  #d0_ia <- do.call(what = bind_rows, args = fishdata) %>% 
  #  filter(State == "IA") %>% 
  #  st_as_sf(coords = c("Lon", "Lat"), crs = 4326)
  #  
  #d0_mwi <- do.call(what = bind_rows, args = fishdata) %>% 
  #  filter(State %in% c("MN", "WI", "IL")) %>% 
  #  st_as_sf(coords = c("Lon", "Lat"), crs = 4269) %>% 
  #  st_transform(crs = 4326)
  #  
  #d0 <- bind_rows(d0_ia, d0_mwi) %>% 
  #  st_transform(crs = st_crs(watershed)$wkt) %>% 
  #  st_join(watershed)
  
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
  
  dat_freq <- dat_fish %>% 
    group_by(watershed_id) %>% 
    summarise(n_site = n_distinct(SiteID)) %>% 
    left_join(dat_freq, by = "watershed_id")
  
  list_freq <- foreach(i = seq_len(nrow(dat_freq))) %do% {
    x <- as.vector(sort(dat_freq[i,-which(colnames(dat_freq) == 'watershed_id')],
                        decreasing = T))
    return(x[x > 0])
  }
  
  names(list_freq) <- dat_freq$watershed_id
  all(dat_freq$n_site == unlist(lapply(list_freq, function(x)x[1]))) # check if the first element of the list is n_site
  
  ## iNEXT: select watershed with 90% coverage
  full_est <- iNEXT(list_freq, datatype = "incidence_freq")
  
  wsd_subset <- full_est[[1]] %>% 
    filter(SC > 0.9) %>% 
    pull(site)
  
  div_est <- full_est[[3]] %>% 
    filter(Diversity == "Species richness" & Site %in% wsd_subset) %>% 
    mutate(watershed_id = as.numeric(as.character(Site)))

# final data --------------------------------------------------------------

  ## export diversity estimates
  dat_mw <- div_est %>%
    left_join(as_tibble(watershed), by = "watershed_id") %>% 
    left_join(dat_alpha, by = "watershed_id") %>% 
    select(-geom)
  
  write_csv(dat_mw, "data_out/data_mw.csv")

  ## export site data
  dat_fish %>% 
    filter(watershed_id %in% wsd_subset) %>% 
    write_csv('data_out/dat_mw_site.csv')
  
  ## export species list
  splist <- d0 %>% 
    filter(watershed_id %in% wsd_subset) %>% 
    arrange(Species) %>% 
    as_tibble() %>% 
    distinct(Species)
  
  write_csv(splist, "data_out/data_mw_splist.csv")
  
  ## export watershed polygon
  albers_wsd_subset <- watershed %>% 
    filter(watershed_id %in% wsd_subset)
  
  st_write(albers_wsd_subset, "data_gis/albers_wsd_subset_mw.gpkg", append = FALSE)
  
  ## export point data
  albers_point <- d0 %>% 
    filter(watershed_id %in% wsd_subset) %>% 
    group_by(SiteID) %>% 
    distinct(SiteID)
  
  st_write(albers_point, "data_gis/albers_point_subset_mw.gpkg", append = FALSE)
  