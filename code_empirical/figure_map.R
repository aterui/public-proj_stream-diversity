

# setup -------------------------------------------------------------------

rm(list = ls(all.names = TRUE))
pacman::p_load(tidyverse, 
               sf, 
               patchwork, 
               mcbrnet, 
               igraph, 
               ggraph, 
               foreach)
setwd(here::here("code_empirical"))


# set theme ---------------------------------------------------------------

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


# fig: network ------------------------------------------------------------

## network generation
n_patch <- 30
p_branch <- c(0.2, 0.8)
sd_env_lon <- c(0.01, 1)
sd_env_source <- c(0.01, 1)
para <- expand.grid(n_patch = n_patch,
                    p_branch = p_branch,
                    sd_env_source = sd_env_source,
                    sd_env_lon = sd_env_lon) %>% 
  filter(!(sd_env_lon == sd_env_source))


net <- foreach(i = seq_len(nrow(para))) %do% {
  set.seed(122)
  brnet(n_patch = para$n_patch[i],
        p_branch = para$p_branch[i],
        sd_env_source = para$sd_env_source[i],
        sd_env_lon = para$sd_env_lon[i],
        plot = FALSE)
}

v_env <- unlist(lapply(seq_len(length(net)),
                       FUN = function(x) net[[x]]$df_patch$environment))

colvalue <- data.frame(color = viridis::viridis(length(v_env)),
                       value = sort(v_env))


ng <- foreach(i = seq_len(length(net))) %do% {
  adj <-  net[[i]]$adjacency_matrix %>% graph.adjacency()
  V(adj)$env <- net[[i]]$df_patch$environment
  
  adj %>% 
    ggraph(layout = layout_as_tree(.,
                                   flip.y = FALSE,
                                   root = 1)) +
    geom_edge_link(color = alpha("steelblue", 0.5)) +
    geom_node_point(shape = 21,
                    fill = colvalue$color[match(V(adj)$env, colvalue$value)],
                    color = grey(0.5),
                    size = 3) +
    labs(subtitle = paste("Branching prob. =", para$p_branch[i]))
}


# fig: map ----------------------------------------------------------------

## read data ####
filename <- list.files(path = 'data_gis', full.names = T)

wsd_subset <- lapply(filename[str_detect(filename, "wsd_subset")],
                     st_read,
                     quiet = TRUE)

point_subset <- lapply(filename[str_detect(filename, "point_subset")],
                       st_read,
                       quiet = TRUE)

channel_hkd <- st_read("data_gis/albers_channel_hkd.gpkg",
                       quiet = TRUE)

shape <- lapply(filename[str_detect(filename, "shape")],
                st_read,
                quiet = TRUE)

shape[[1]] <- st_set_crs(shape[[1]],
                         st_crs(wsd_subset[[1]])) %>% 
  st_cast("POLYGON") %>% 
  dplyr::mutate(area = st_area(.)) %>% 
  dplyr::filter(area == max(.$area))

## join number of sampling sites ####
wsd_subset <- foreach(i = seq_len(length(wsd_subset))) %do% {
  n_site <- st_join(point_subset[[i]],
                    wsd_subset[[i]]) %>% 
    dplyr::group_by(watershed_id) %>% 
    dplyr::mutate(n_site = n_distinct(SiteID)) %>% 
    dplyr::summarise(n_site = unique(n_site)) %>% 
    dplyr::as_tibble() %>% 
    dplyr::select(watershed_id, n_site)
  
  re <- wsd_subset[[i]] %>% 
    left_join(n_site, by = "watershed_id") %>% 
    dplyr::mutate(category = case_when(n_site <= 50 ~ "10 to 50",
                                       between(n_site, 51, 100) ~ "51 to 100",
                                       n_site > 100 ~ "> 100",
                                       TRUE ~ as.character(n_site))) %>% 
    dplyr::mutate(category = factor(category,
                                    levels = c("> 100",
                                               "51 to 100",
                                               "10 to 50")))
  return(re)
}

## example channel ####
channel_eg <- channel_hkd %>% 
  dplyr::filter(wsd_id == 1348)

polygon_eg <- wsd_subset[[1]] %>% 
  dplyr::filter(watershed_id == 1348)

point_eg <- st_join(point_subset[[1]],
                    wsd_subset[[1]]) %>% 
  dplyr::filter(watershed_id == 1348)

eg <- ggplot() +
  geom_sf(data = polygon_eg,
          fill = grey(0.99)) +
  geom_sf(data = channel_eg,
          size = 0.05) +
  geom_sf(data = point_eg,
          color = alpha("salmon", 0.8),
          size = 0.001) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

## hokkaido ####
hkd <- ggplot() +
  geom_sf(data = shape[[1]],
          fill = grey(0.99),
          size = 0.1) +
  geom_sf(data = wsd_subset[[1]],
          aes(fill = category),
          color = grey(0.65),
          size = 0.1) +
  geom_sf(data = polygon_eg,
          color = "salmon",
          fill = alpha("white", 0),
          size = 0.3) +
  scale_fill_manual(values = c(grey(0.2), grey(0.5), grey(0.8))) +
  labs(subtitle = paste0("Hokkaido, Japan\n",
                         eval(nrow(wsd_subset[[1]])),
                         " watersheds"),
       fill = "Number of sites") +
  theme_bw() + theme(axis.text = element_text(size = 7))

## midwest ####
mw <- ggplot() +
  geom_sf(data = shape[[2]],
          fill = grey(0.99),
          size = 0.1) +
  geom_sf(data = wsd_subset[[2]],
          aes(fill = category),
          color = grey(0.65),
          size = 0.1) +
  scale_fill_manual(values = c(grey(0.2), grey(0.5), grey(0.8))) +
  labs(subtitle = paste0("Midwest, US\n",
                         eval(nrow(wsd_subset[[2]])),
                         " watersheds"),
       fill = "Number of sites") +
  theme_bw() + theme(axis.text = element_text(size = 7))

# plot assembly -----------------------------------------------------------

network1 <- (ng[[1]] + labs(title = "A")) + ng[[2]]
network2 <- (ng[[3]] + labs(title = "B")) + ng[[4]]
print((network1 | network2) / ((eg + labs(title = "C")) + 
                                 (hkd + theme(legend.position = "none") + 
                                    labs(title = "D")) + mw))

ggsave(normalizePath("../document_output/figure_01.pdf"),
       width = 10,
       height = 6)
