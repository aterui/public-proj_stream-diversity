

# setup -------------------------------------------------------------------
  
  rm(list = ls(all.names = TRUE))
  pacman::p_load(tidyverse, sf, patchwork)
  setwd(here::here("empirical"))

# fig: map ----------------------------------------------------------------

  ## read data
  filename <- list.files(path = 'data_gis', full.names = T)
  wsd_subset <- lapply(filename[str_detect(filename, "wsd_subset")], st_read)
  point_subset <- lapply(filename[str_detect(filename, "point_subset")], st_read)
  shape <- lapply(filename[str_detect(filename, "shape")], st_read)
  shape[[1]] <- st_set_crs(shape[[1]], st_crs(wsd_subset[[1]]))
  
  ## set theme
  theme_set(theme_bw())
  
  ## hokkaido
  hkd <- ggplot() +
    geom_sf(data = shape[[1]], fill = grey(0.99), size = 0.01) +
    geom_sf(data = wsd_subset[[1]], fill = alpha("steelblue", alpha = 0.1), size = 0.01) +
    geom_sf(data = point_subset[[1]], size = 0.001, color = alpha("salmon", 0.5)) + 
    ggtitle(paste0("Hokkaido, Japan\n", eval(nrow(wsd_subset[[1]])), " watersheds"))
  
  ## midwest
  mw <- ggplot() +
    geom_sf(data = shape[[2]], fill = grey(0.99), size = 0.01) +
    geom_sf(data = wsd_subset[[2]], fill = alpha("steelblue", alpha = 0.1), size = 0.01) +
    geom_sf(data = point_subset[[2]], size = 0.001, color = alpha("salmon", 0.5)) + 
    ggtitle(paste0("Midwest, US\n", eval(nrow(wsd_subset[[2]])), " watersheds"))
  
  print(mw + hkd)
  
  
