

# setup -------------------------------------------------------------------
  
  rm(list = ls(all.names = TRUE))
  pacman::p_load(tidyverse, sf, patchwork, mcbrnet, igraph, ggraph, foreach)
  setwd(here::here("empirical"))


# fig: network ------------------------------------------------------------
  
  ## set theme
  plt_theme <- theme_bw() + theme(
    plot.background = element_blank(),
    
    panel.background = element_blank(),
    panel.border = element_blank(),
    
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
  
  ## network generation
  n_patch <- 50
  p_branch <- seq(0.2, 0.8, by = 0.3)
  
  set.seed(111)
  net <- lapply(p_branch, FUN = function(x) brnet(n_patch = n_patch, p_branch = x, plot= FALSE,
                                                  sd_env_lon = 0.01, sd_env_source = 1))
  v_env <- unlist(lapply(seq_len(length(net)), FUN = function(x) net[[x]]$df_patch$environment))
  
  colvalue <- data.frame(color = viridis::viridis(length(v_env)),
                         value = sort(v_env))
  
  
  ng <- foreach(i = seq_len(length(net))) %do% {
    adj <-  net[[i]]$adjacency_matrix %>% graph.adjacency()
    V(adj)$env <- net[[i]]$df_patch$environment
    
    adj %>% 
      ggraph(layout = layout_as_tree(., flip.y = FALSE, root = 1)) +
      geom_edge_link(color = alpha("steelblue", 0.5)) +
      geom_node_point(shape = 21,
                      fill = colvalue$color[match(V(adj)$env, colvalue$value)],
                      color = grey(0.5),
                      size = 2) +
      labs(subtitle = paste("Branching prob. =", p_branch[i]))
  }
  
  
# fig: map ----------------------------------------------------------------

  ## read data
  filename <- list.files(path = 'data_gis', full.names = T)
  wsd_subset <- lapply(filename[str_detect(filename, "wsd_subset")], st_read, quiet = TRUE)
  point_subset <- lapply(filename[str_detect(filename, "point_subset")], st_read, quiet = TRUE)
  shape <- lapply(filename[str_detect(filename, "shape")], st_read, quiet = TRUE)
  shape[[1]] <- st_set_crs(shape[[1]], st_crs(wsd_subset[[1]])) %>% 
    st_cast("POLYGON") %>% 
    mutate(area = st_area(.)) %>% 
    filter(area == max(.$area))
  
  ## join number of sampling sites
  wsd_subset <- foreach(i = seq_len(length(wsd_subset))) %do% {
    n_site <- st_join(point_subset[[i]], wsd_subset[[i]]) %>% 
      group_by(watershed_id) %>% 
      mutate(n_site = n_distinct(SiteID)) %>% 
      summarise(n_site = unique(n_site)) %>% 
      as_tibble() %>% 
      select(watershed_id, n_site)
    
    re <- wsd_subset[[i]] %>% 
      left_join(n_site, by = "watershed_id") %>% 
      mutate(category = case_when(n_site <= 50 ~ "10 to 50",
                                  between(n_site, 50, 99) ~ "51 to 100",
                                  n_site > 99 ~ "> 100",
                                  TRUE ~ as.character(n_site))) %>% 
      mutate(category = factor(category, levels = c("> 100", "51 to 100", "10 to 50")))
    return(re)
  }
  
  ## hokkaido
  hkd <- ggplot() +
    geom_sf(data = shape[[1]], fill = grey(0.99), size = 0.1) +
    geom_sf(data = wsd_subset[[1]],
            aes(fill = category),
            color = grey(0.65),
            size = 0.1) +
    scale_fill_manual(values = c(grey(0.2), grey(0.5), grey(0.8))) +
    labs(subtitle = paste0("Hokkaido, Japan\n", eval(nrow(wsd_subset[[1]])), " watersheds"),
         fill = "Number of sites") +
    theme_bw()
  
  ## midwest
  mw <- ggplot() +
    geom_sf(data = shape[[2]], fill = grey(0.99), size = 0.1) +
    geom_sf(data = wsd_subset[[2]],
            aes(fill = category),
            color = grey(0.65),
            size = 0.1) +
    scale_fill_manual(values = c(grey(0.2), grey(0.5), grey(0.8))) +
    labs(subtitle = paste0("Midwest, US\n", eval(nrow(wsd_subset[[2]])), " watersheds"),
         fill = "Number of sites") +
    theme_bw() +
    theme(legend.position = "none")
  

# plot assembly -----------------------------------------------------------

  patch <- (ng[[1]] + labs(title = "A")) + ng[[2]] + ng[[3]]
  print(patch / ((mw + labs(title = "B")) + hkd))
  
  
