
# setup -------------------------------------------------------------------

  pacman::p_load(tidyverse, avpc, foreach)
  source(here::here('empirical/analysis_rlm_bf.R'))


# average predictive comparisons ------------------------------------------

  u <- colnames(model.matrix(fit$gamma$model))[c(2,3)]
  ap_est <- foreach(i = seq_len(length(u)), .combine = bind_rows) %do% {
    set.seed(123)
    y <- apcomp(m = fit$gamma$model, u = u[i],
                y_scale = "log10",
                u_scale = "log10")
    
    re <- tibble(input = u[i],
                 sim_estimate = y$sim_estimate,
                 sim_se = y$sim_se)
    
    return(re)
  }