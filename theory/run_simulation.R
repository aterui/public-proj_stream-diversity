
# library -----------------------------------------------------------------

  library(tidyverse)
  library(foreach)
  library(mcbrnet)
  library(doParallel)
  library(doSNOW)

# parallel set up ---------------------------------------------------------

  cl <- makeCluster(detectCores())
  registerDoSNOW(cl)
  
# parameter setup ---------------------------------------------------------
  
  para <- expand.grid(n_species = 30,
                      n_patch = seq(from = 10, to = 150, by = 20),
                      p_branch = seq(from = 0, to = 0.9, by = 0.1),
                      mean_env_source = 0,
                      sd_env_source = c(0.01, 1),
                      rho = 1,
                      asymmetry_factor = c(1, 1.5),
                      min_optim = -1,
                      max_optim = 1,
                      sd_env_lon = c(0.01, 1),
                      theta = 1,
                      K_type = c("variable", "equal"),
                      K0 = 100,
                      sd_env = 0.1,
                      spatial_env_cor = TRUE,
                      phi = 0.05,
                      min_niche_width = 0.1,
                      max_niche_width = 1,
                      niche_cost = 1,
                      p_dispersal = c(0.001, 0.01),
                      min_alpha = 0,
                      max_alpha = c(0.5, 1.5))
  
  n_set <- nrow(para)
  n_rep <- 50
  z <- 0.78 # a power parameter for the discharge-watershed-area relationship
  
# run simulation ----------------------------------------------------------

  pb <- txtProgressBar(max = n_set, style = 3)
  fun_progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = fun_progress)
  
  result <- foreach(x = iter(para, by = 'row'), .combine = "bind_rows",
                    .packages = c("mcbrnet", "foreach", "dplyr"),
                    .options.snow = opts) %dopar% {
                    
                    # within-parameter replicate
                      df_set <- foreach(j = seq_len(n_rep), .combine = "bind_rows") %do% {
                                            
                        ## network generation
                        net <- brnet(n_patch = x$n_patch,
                                     p_branch = x$p_branch,
                                     mean_env_source = x$mean_env_source,
                                     sd_env_source = x$sd_env_source,
                                     rho = x$rho,
                                     sd_env_lon = x$sd_env_lon,
                                     asymmetry_factor = x$asymmetry_factor,
                                     plot = FALSE)
                        
                        if (x$K_type == "variable") {
                          K <- x$K0 * net$df_patch$n_patch_upstream^z
                        } else {
                          K <- rep(x = sum(x$K0 *net$df_patch$n_patch_upstream^z) / x$n_patch, times = x$n_patch)
                        }
                                            
                        ## metacommunity simulation                                                 
                        mc <- mcsim(n_patch = x$n_patch,
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
                         
                        df <- tibble(n_rep = j,
                                     mc_capacity = sum(K),
                                     x,
                                     mc$df_diversity)
                        return(df)
                      }
                    
                    return(df_set)
                    }
      
  close(pb)
  

# return ------------------------------------------------------------------
  
  stopCluster(cl)
  result <- 2
  write.csv(result, file = paste0("theory/result/result_sim", Sys.Date(), ".csv"))
  