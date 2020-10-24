
# setup -------------------------------------------------------------------

  library(tidyverse)
  library(sf)
  library(iNEXT)
  library(foreach)
  library(here)
  setwd(here("empirical"))

# read gis data -----------------------------------------------------------

  channel <- st_read(dsn = 'data_gis', layer = 'albers_mw_arcgis_channel')
  watershed <- st_read(dsn = 'data_gis', layer = 'albers_mw_watershed')

  watershed <- watershed %>% 
    select(
      mu_elev = Elevationm,
      mu_temp = AirTempera,
      mu_prec = Precipitat,
      p_forest = Percent_Fo,
      p_agri = Percent_Ag,
      p_urban = Percent_Ur,
      p_wetland = Percent_we
    ) %>% 
    mutate(watershedID = paste0("w", seq_len(nrow(watershed))),
           area = units::set_units(st_area(watershed), km^2))
  
  channel <- st_join(x = channel, y = watershed, join = st_within)
  channel <- channel %>%
    mutate(length = units::set_units(st_length(channel), km)) %>% 
    select(watershedID, length)
  
  p_branch <- channel %>% 
    group_by(watershedID) %>% 
    mutate(n_branch = n()) %>% 
    filter(n_branch > 1) %>% 
    mutate(rate = fitdistrplus::fitdist(as.numeric(length), "exp")$estimate) %>% 
    summarise(rate = unique(rate), n_branch = unique(n_branch)) %>% 
    mutate(p_branch = pexp(q = 1, rate = rate)) %>% 
    as_tibble()

# merge fish and gis data -------------------------------------------------

  fishdata <- list.files(path = "data_org_mw", full.names = TRUE) %>%
    lapply(read_csv)

  d0 <- do.call(what = bind_rows, args = fishdata)
  
  d0_ia <- d0 %>% 
    filter(State == "IA") %>% 
    st_as_sf(coords = c("Lon", "Lat"), crs = 4326)
  
  d0_wi_mn_il <- d0 %>% 
    filter(State %in% c("MN", "IL", "WI")) %>% 
    st_as_sf(coords = c("Lon", "Lat"), crs = 4269) %>% 
    st_transform(crs = st_crs(d0_ia))
  
  d0 <- bind_rows(d0_ia, d0_wi_mn_il) %>% 
    st_transform(crs = st_crs(watershed)) %>% 
    st_join(watershed)
  
  ## watershed ID with more than 10 sampling sites
  wsd10 <- d0 %>% 
    group_by(watershedID) %>% 
    summarise(n_site = n_distinct(SiteID)) %>% 
    filter(n_site >= 10) %>% 
    drop_na(watershedID) %>% 
    pull(watershedID)
  
  dat_fish <- d0 %>% 
    filter(watershedID %in% wsd10) %>% 
    as_tibble()

  dat_alpha <- dat_fish %>% 
    group_by(watershedID, SiteID) %>% 
    summarise(alpha_div = n_distinct(Species)) %>% 
    group_by(watershedID) %>% 
    summarise(mu_alpha = mean(alpha_div))
  
# rarefaction -------------------------------------------------------------

  dat_freq <- dat_fish %>% 
    group_by(watershedID, Species) %>% 
    summarise(freq = n()) %>% 
    pivot_wider(id_cols = watershedID,
                names_from = Species,
                values_from = freq,
                values_fill = list(freq = 0))
  watershedID <- pull(dat_freq, watershedID) 
  
  dat_freq <- dat_fish %>% 
    group_by(watershedID) %>% 
    summarise(n_site = n_distinct(SiteID)) %>% 
    left_join(dat_freq, by = "watershedID") %>% 
    select(-watershedID)
  
  list_freq <- foreach(i = seq_len(nrow(dat_freq))) %do% {
    x <- as.vector(sort(dat_freq[i,], decreasing = T))
    return(x[x > 0])
  }
  
  names(list_freq) <- watershedID
  
  div_est <- ChaoRichness(list_freq, datatype = "incidence_freq")
  div_est <- mutate(div_est, watershedID = rownames(div_est))

# final data --------------------------------------------------------------

  dat_mw <- div_est %>%
    left_join(as_tibble(watershed), by = "watershedID") %>% 
    left_join(p_branch, by = "watershedID") %>% 
    left_join(dat_alpha, by = "watershedID") %>% 
    select(-geometry.x, -geometry.y)
  
  write_csv(dat_mw, "data_out/data_mw.csv")
