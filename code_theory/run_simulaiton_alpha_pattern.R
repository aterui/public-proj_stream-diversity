
# library -----------------------------------------------------------------

  rm(list = ls(all.names = TRUE))
  pacman::p_load(here, tidyverse, foreach, mcbrnet, doParallel, doSNOW)


# parallel setup ----------------------------------------------------------

  cl <- makeCluster(detectCores())
  registerDoSNOW(cl)

# parameter setup ---------------------------------------------------------

  para <- expand.grid(n_species = 50,
                      mean_env_source = 0,
                      sd_env_source = c(0.01, 1),
                      rho = 1,
                      asymmetry_factor = 1,
                      min_optim = -1,
                      max_optim = 1,
                      sd_env_lon = c(0.01, 1),
                      theta = c(0.1, 1),
                      K0 = 100,
                      sd_env = 0.1,
                      spatial_env_cor = TRUE,
                      phi = 0.05,
                      min_niche_width = 0.1,
                      max_niche_width = 1,
                      niche_cost = 1,
                      p_dispersal = c(0.01, 0.1),
                      min_alpha = 0,
                      max_alpha = c(0.75, 1.5))
  
  n_set <- nrow(para)
  n_rep <- 10
  n_patch <- 100
  p_branch <- 0.5

# run simulation ----------------------------------------------------------

  pb <- txtProgressBar(max = n_set, style = 3)
  fun_progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = fun_progress)
  
  result <- foreach(x = iter(para, by = 'row'),
                    .combine = "bind_rows",
                    .packages = c("mcbrnet", "foreach", "dplyr"),
                    .options.snow = opts) %dopar% {
                      
                      # within-parameter replicate
                      df_set <- foreach(j = seq_len(n_rep),
                                        .combine = "bind_rows") %do% {
                        
                        ## network generation
                        net <- brnet(n_patch = n_patch,
                                     p_branch = p_branch,
                                     mean_env_source = x$mean_env_source,
                                     sd_env_source = x$sd_env_source,
                                     rho = x$rho,
                                     sd_env_lon = x$sd_env_lon,
                                     asymmetry_factor = x$asymmetry_factor,
                                     plot = FALSE)
                        
                        K <- x$K0 * net$df_patch$n_patch_upstream
                        
                        ## metacommunity simulation                                                 
                        mc <- mcsim(n_patch = n_patch,
                                    n_species = x$n_species,
                                    
                                    distance_matrix = net$distance_matrix,
                                    weighted_distance_matrix = net$weighted_distance_matrix,
                                    theta = x$theta,
                                    
                                    mean_env = net$df_patch$environment,
                                    carrying_capacity = K,
                                    sd_env = x$sd_env,
                                    spatial_env_cor = TRUE,
                                    phi = x$phi,
                                    
                                    min_optim = x$min_optim,
                                    max_optim = x$max_optim,
                                    min_niche_width = x$min_niche_width,
                                    max_niche_width = x$max_niche_width,
                                    niche_cost = x$niche_cost,
                                    p_dispersal = x$p_dispersal,
                                    
                                    interaction_type = "random",
                                    min_alpha = x$min_alpha,
                                    max_alpha = x$max_alpha)
                        
                        df_patch <- left_join(mc$df_patch,
                                              net$df_patch,
                                              by = "patch_id")
                        
                        df <- tibble(n_rep = j,
                                     mc_capacity = sum(K),
                                     n_patch = n_patch,
                                     p_branch = p_branch,
                                     x,
                                     df_patch)
                        return(df)
                      }
                      
                      return(df_set)
                    }
  

# return ------------------------------------------------------------------

  stopCluster(cl)
  write.csv(result, file = paste0(here("theory/result/result_sim_alpha_pattern"),
                                  Sys.Date(),
                                  ".csv"))
  