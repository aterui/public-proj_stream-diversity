
# setup -------------------------------------------------------------------
  
  rm(list = ls(all.names = TRUE))
  setwd(here::here("code_empirical"))
  pacman::p_load(tidyverse, sf, iNEXT, foreach, tmap)  

# read gis data -----------------------------------------------------------

  albers_watershed <- st_read(dsn = 'data_gis/albers_watershed_mw_final.gpkg') %>% 
    dplyr::rename(watershed_id = wsd_id) %>% 
    dplyr::arrange(watershed_id)
  
  albers_huc_zone4_7_buff <- st_read(dsn = "data_gis/albers_huc2_zone4_7.gpkg") %>% 
    st_buffer(dist = 500)
  
# merge fish and gis data -------------------------------------------------
  
  fishdata <- list.files(path = "data_org_mw", full.names = TRUE) %>%
    lapply(read_csv)
  
  ## unique site within zone 4 & 7
  albers_point <- do.call(what = bind_rows, args = fishdata) %>% 
    st_as_sf(coords = c("Lon", "Lat"), crs = 4326) %>% 
    dplyr::distinct(SiteID, geometry) %>% 
    st_transform(crs = 5070) %>% 
    st_intersection(albers_huc_zone4_7_buff)
  
  st_write(albers_point,
           dsn = "data_gis/albers_point_mw.gpkg",
           append = FALSE)
  
  site_id_subset <- albers_point %>% 
    dplyr::pull(SiteID)
  
  ## IA: wgs84; MN, WI, IL: NAD83
  d0 <- do.call(what = bind_rows, args = fishdata) %>% 
    st_as_sf(coords = c("Lon", "Lat"), crs = 4326) %>% 
    dplyr::filter(SiteID %in% site_id_subset) %>% 
    st_transform(crs = st_crs(albers_watershed)$wkt) %>% 
    st_join(albers_watershed)
  
  ## checking for CRS definition influence
  #d0_ia <- do.call(what = bind_rows, args = fishdata) %>% 
  #  dplyr::filter(State == "IA") %>% 
  #  st_as_sf(coords = c("Lon", "Lat"), crs = 4326)
  #  
  #d0_mwi <- do.call(what = bind_rows, args = fishdata) %>% 
  #  dplyr::filter(State %in% c("MN", "WI", "IL")) %>% 
  #  st_as_sf(coords = c("Lon", "Lat"), crs = 4269) %>% 
  #  st_transform(crs = 4326)
  #  
  #d0 <- bind_rows(d0_ia, d0_mwi) %>% 
  #  st_transform(crs = st_crs(albers_watershed)$wkt) %>% 
  #  st_join(albers_watershed)
  
  n_site_stat <- d0 %>%
    dplyr::as_tibble() %>% 
    dplyr::group_by(watershed_id) %>% 
    dplyr::summarise(n_site = n_distinct(SiteID)) %>% 
    dplyr::right_join(as_tibble(albers_watershed), by = "watershed_id") %>% 
    dplyr::select(watershed_id, n_site, area, n_branch) %>% 
    dplyr::mutate(density_site = n_site/area)
  
  ## watershed ID with more than X site
  ## watershed ID with more than X branch
  wsd_candidate <- n_site_stat %>% 
    dplyr::filter(n_site >= 10 & n_branch >= 10) %>% 
    dplyr::pull(watershed_id)
  
  dat_fish <- d0 %>% 
    dplyr::filter(watershed_id %in% wsd_candidate) %>% 
    dplyr::as_tibble()
  
  dat_alpha <- dat_fish %>% 
    dplyr::group_by(watershed_id, SiteID) %>% 
    dplyr::summarise(alpha_div = n_distinct(Species)) %>% 
    dplyr::group_by(watershed_id) %>% 
    dplyr::summarise(mu_alpha = mean(alpha_div))

# iNEXT -------------------------------------------------------------------
  
  ## convert data to frequency
  dat_freq <- dat_fish %>% 
    dplyr::group_by(watershed_id, Species) %>% 
    dplyr::summarise(freq = n_distinct(SiteID)) %>% 
    tidyr::pivot_wider(id_cols = watershed_id,
                       names_from = Species,
                       values_from = freq,
                       values_fill = list(freq = 0)) 
  
  dat_freq <- dat_fish %>% 
    dplyr::group_by(watershed_id) %>% 
    dplyr::summarise(n_site = n_distinct(SiteID)) %>% 
    dplyr::left_join(dat_freq, by = "watershed_id")
  
  list_freq <- foreach(i = seq_len(nrow(dat_freq))) %do% {
    x <- as.vector(sort(unlist(dat_freq[i,-which(colnames(dat_freq) == 'watershed_id')]),
                        decreasing = T))
    return(x[x > 0])
  }
  
  names(list_freq) <- dat_freq$watershed_id
  all(dat_freq$n_site == unlist(lapply(list_freq, function(x)x[1]))) # check if the first element of the list is n_site
  
  ## iNEXT: select watershed with 90% coverage
  full_est <- iNEXT(list_freq, 
                    datatype = "incidence_freq")
  
  wsd_subset <- full_est[[1]] %>% 
    dplyr::filter(SC > 0.9) %>% 
    dplyr::pull(site)
  
  div_est <- full_est[[3]] %>% 
    dplyr::filter(Diversity == "Species richness" & Site %in% wsd_subset) %>% 
    dplyr::mutate(watershed_id = as.numeric(as.character(Site)))

# final data --------------------------------------------------------------

  ## export diversity estimates
  dat_mw <- div_est %>%
    dplyr::left_join(as_tibble(albers_watershed), by = "watershed_id") %>% 
    dplyr::left_join(dat_alpha, by = "watershed_id") %>% 
    dplyr::select(-geom)
  
  write_csv(dat_mw, "data_out/data_mw.csv")

  ## export site data
  dat_fish %>% 
    dplyr::filter(watershed_id %in% wsd_subset) %>% 
    dplyr::write_csv('data_out/data_mw_site.csv')
  
  ## export species list
  splist <- d0 %>% 
    dplyr::filter(watershed_id %in% wsd_subset) %>% 
    dplyr::arrange(Species) %>% 
    dplyr::as_tibble() %>% 
    dplyr::distinct(Species)
  
  write_csv(splist, "data_out/data_mw_splist.csv")
  
  ## export watershed polygon
  albers_wsd_subset <- albers_watershed %>% 
    dplyr::filter(watershed_id %in% wsd_subset)
  
  st_write(albers_wsd_subset,
           "data_gis/albers_wsd_subset_mw.gpkg",
           append = FALSE)
  
  ## export point data
  albers_point_subset <- d0 %>% 
    dplyr::filter(watershed_id %in% wsd_subset) %>% 
    dplyr::distinct(SiteID, geometry)
  
  st_write(albers_point_subset,
           "data_gis/albers_point_subset_mw.gpkg",
           append = FALSE)
  