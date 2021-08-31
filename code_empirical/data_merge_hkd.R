
# setup -------------------------------------------------------------------

rm(list = ls(all.names = TRUE))
setwd(here::here("code_empirical"))
pacman::p_load(tidyverse, 
               sf, 
               iNEXT, 
               foreach)  

source("gis_crs_fmt.R")

# read gis data -----------------------------------------------------------

albers_watershed <- st_read(dsn = 'data_gis/albers_watershed_hkd_final.gpkg',
                            crs = wkt_jgd_albers) %>% 
  dplyr::rename(watershed_id = wsd_id) %>% 
  dplyr::arrange(watershed_id)

# merge fish and gis data -------------------------------------------------

d0 <- read_csv('data_org_hkd/data_fmt_hkd_latest.csv') %>% 
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326) %>% 
  st_transform(crs = st_crs(albers_watershed)$wkt) %>% 
  dplyr::filter(Year >= 1990) %>% 
  st_join(albers_watershed)

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
dat_hkd <- div_est %>%
  dplyr::left_join(as_tibble(albers_watershed), by = "watershed_id") %>% 
  dplyr::left_join(dat_alpha, by = "watershed_id") %>% 
  dplyr::select(-geom)

readr::write_csv(dat_hkd, "data_out/data_hkd.csv")

## export site data
dat_fish %>% 
  dplyr::filter(watershed_id %in% wsd_subset) %>% 
  readr::write_csv('data_out/data_hkd_site.csv')

## export species list
splist <- d0 %>% 
  dplyr::filter(watershed_id %in% wsd_subset) %>% 
  dplyr::arrange(Species) %>% 
  dplyr::as_tibble() %>% 
  dplyr::distinct(Species)

readr::write_csv(splist, "data_out/data_hkd_splist.csv")

## export watershed polygon
albers_wsd_subset <- albers_watershed %>% 
  dplyr::filter(watershed_id %in% wsd_subset)

st_write(albers_wsd_subset,
         "data_gis/albers_wsd_subset_hkd.gpkg",
         append = FALSE)

## export point data
albers_point <- d0 %>% 
  dplyr::distinct(SiteID, geometry)

st_write(albers_point,
         "data_gis/albers_point_hkd.gpkg",
         append = FALSE)

albers_point_subset <- d0 %>% 
  dplyr::filter(watershed_id %in% wsd_subset) %>% 
  dplyr::distinct(SiteID, geometry)

st_write(albers_point_subset,
         "data_gis/albers_point_subset_hkd.gpkg",
         append = FALSE)
