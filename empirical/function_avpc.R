
# setup -------------------------------------------------------------------

  pacman::p_load(tidyverse, foreach)
  source(here::here('empirical/analysis_lm_bf.R'))


# simulation --------------------------------------------------------------
  
  m <- fit$gamma$model
  u <- 'log(p_branch, 10)'
  v <- c('log(area, 10)', 'scale(resid_temp)', 'scale(resid_ppt)', 'region')
  

# extract model data frame ------------------------------------------------
  
  if(class(m) %in% c('lm', 'rlm')) {
    m <- m$model
  }
  
  if(class(m) %in% 'lmerMod') {
   m <- m@frame  
  }
  
  # extract character variables and convert them to numeric values
  m_chr <- m %>% 
    summarise(across(.cols = is.character,
                     .fns = ~ as.numeric(as.factor(.x)) - 1)) %>% 
    mutate(id = as.numeric(rownames(m)))
  
  # extract numeric variables
  m_dbl <- m %>% 
    summarise(across(.cols = is.numeric)) %>% 
    mutate(id = as.numeric(rownames(m)))
  
  # combine
  mod <- m_chr %>% 
    left_join(m_dbl, by = 'id')


# get pairs for u and v ---------------------------------------------------
  
  if(!is.null(v)) {
    X1 <- X2 <- mod %>% select(v)
  } else {
    X1 <- X2 <- mod %>% select(-id, -v)
  }
  
  X <- X1 %>% mutate(id = as.numeric(rownames(.)))
  
  # mahalanobis distance for a set of v variables
  m_cov <- cov(X1)
  m_dist <- apply(X1, 1, function(row_i) mahalanobis(X2, row_i, m_cov))
  df_v <-  tibble(sq_distance = c(m_dist),
                  row_id = rep(seq_len(nrow(m_dist)), times = ncol(m_dist)),
                  col_id = rep(seq_len(ncol(m_dist)), each = nrow(m_dist))) %>% 
    mutate(weight = 1/(1 + sq_distance))
  
  # combine with input u
  m_u <- mod %>%
    select(u) %>% 
    rename(u_input = u) %>% 
    mutate(id = as.numeric(rownames(.)))
  
  df_uv <- df_v %>% 
    left_join(m_u, by = c('row_id' = 'id')) %>% 
    rename(u1 = u_input) %>% 
    left_join(m_u, by = c('col_id' = 'id')) %>% 
    rename(u2 = u_input) %>% 
    mutate(sign = ifelse(u2 - u1 >= 0, 1, -1)) %>% 
    left_join(X, by = c('row_id' = 'id'), suffix = c('_v1', '_v2')) %>% 
    left_join(X, by = c('col_id' = 'id'), suffix = c('_v1', '_v2'))
  