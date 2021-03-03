
# setup -------------------------------------------------------------------

  pacman::p_load(tidyverse, foreach)
  source(here::here('empirical/analysis_rlm_bf.R'))


# simulation --------------------------------------------------------------
  
  m <- fit$gamma$model
  u <- 'log(p_branch, 10)'
  v <- c('log(area, 10)', 'region', 'scale(resid_ppt)')
  link <- 'log'


#avpc <- function(u, v = NULL, link = NULL) {  
  
# define function ---------------------------------------------------------

  ilogit <- function(x) {
    1 / (1 + exp(-x))
  }
    
  
# extract model data frame ------------------------------------------------
  
  if(any(class(m) %in% c('lm', 'rlm', 'glm'))) {
    m_frame <- m$model
    if(is.null(v)) {
      v_var_names <- attributes(m$terms)$term.labels
      v <- v_var_names[!(v_var_names %in% u)]
    }
  }
  
  if(any(class(m) %in% 'lmerMod')) {
    m_frame <- m@frame
    if(is.null(v)) {
      v_var_names <- attributes(terms(m))$term.labels
      v <- v_var_names[!(v_var_names %in% u)]
    }
  }
  
  if(ncol(m_frame %>% summarise(across(.cols = where(is.character)))) > 0) {
    
    # character variable(s) exist
    
    ## extract character variables and convert them to numeric values
    ## stop if there are > 2 character levels in any character variable
    n_levels <- m_frame %>% 
      summarise(across(.cols = where(is.character),
                       .fns = ~ n_distinct(.x))) 
  
    if(any(n_levels > 2)) stop('Currently, this function does not support categorical variables with > 2 levels. Consider converting the variable(s) to dummy binary variables (0, 1)')
    
    m_chr <- m_frame %>% 
      summarise(across(.cols = where(is.character),
                       .fns = ~ as.numeric(as.factor(.x)) - 1)) %>% 
      mutate(id = as.numeric(rownames(.)))
    
    ## extract numeric variables
    m_dbl <- m_frame %>% 
      summarise(across(.cols = where(is.numeric))) %>% 
      mutate(id = as.numeric(rownames(.)))
    
    ## combine numeric and character variables
    mod <- m_chr %>% 
      left_join(m_dbl, by = 'id') %>% 
      select(-id) %>% 
      tibble()
    
    message('Character variable(s) detected in the data. These variables were coverted to dummy binary variables (0, 1)')
    
  } else {

    # no character variable
    
    mod <- tibble(m_frame)
    
  }


# get pairs for u and v ---------------------------------------------------
  
  # frame for v variables
  X1 <- X2 <- mod %>% select(all_of(v))
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
    select(all_of(u)) %>% 
    rename(u_input = all_of(u)) %>% 
    mutate(id = as.numeric(rownames(.)))
  
  df_uv <- df_v %>% 
    left_join(m_u, by = c('row_id' = 'id')) %>% 
    rename(u1 = u_input) %>% 
    left_join(m_u, by = c('col_id' = 'id')) %>% 
    rename(u2 = u_input) %>% 
    mutate(sign = ifelse(u2 - u1 >= 0, 1, -1)) %>% 
    left_join(X, by = c('row_id' = 'id'), suffix = c('_v1', '_v2')) %>% 
    left_join(X, by = c('col_id' = 'id'), suffix = c('_v1', '_v2'))
  

# average predictive comparison -------------------------------------------
  
  # input low
  u1 <- df_uv %>% pull(u1)
  df_v1 <- df_uv %>% summarize(across(ends_with('v1')))
  df_uv1 <- tibble(u1 = u1, df_v1)
  colnames(df_uv1) <- c(u, v)
  df_uv1 <- tibble(Intercept = 1, df_uv1)
  
  # input high
  u2 <- df_uv %>% pull(u2)
  df_v2 <- df_uv %>% summarize(across(ends_with('v2')))
  df_uv2 <- tibble(u2 = u2, df_v2)
  colnames(df_uv2) <- c(u, v)
  df_uv2 <- tibble(Intercept = 1, df_uv2)
  
  # get link function from the model object if link 'null'
  if(is.null(link)) {
    model_family <- family(m)
    link <- model_family$link
  }
  
  if(!any(link %in% c('log', 'logit', 'identity'))) stop('Link function must be either log, logit or identity')
  
  
  # for model classes lm, rlm, glm
  if(class(m) %in% c('lm', 'rlm', 'glm')) {
    
    names(m$coefficients) <- c('Intercept', attributes(m$terms)$term.labels)
    v_var_id <- match(names(m$coefficients), names(df_uv1)) %>% 
      na.omit() %>% 
      c()
    
    # vector of regression coefs
    v_b <- m$coefficients[names(m$coefficients) %in% c('Intercept', u, v)] %>% 
      data.matrix()
    
    # matrix of input and other variables
    m_uv1 <- data.matrix(df_uv1[, v_var_id])
    m_uv2 <- data.matrix(df_uv2[, v_var_id])
    
    if(any(rownames(v_b) != colnames(m_uv1))) stop('error in matrix organization')
    if(any(rownames(v_b) != colnames(m_uv2))) stop('error in matrix organization')
    
    # division by link function types
    if(link == 'identity') {
      e_y1 <- m_uv1 %*% v_b
      e_y2 <- m_uv2 %*% v_b
      numerator <- sum(df_uv$weight * (e_y2 - e_y1) * df_uv$sign)
      denominator <- sum(df_uv$weight * (u2 - u1) * df_uv$sign)
      est <- numerator / denominator
    }
    
    if(link == 'log') {
      e_y1 <- exp(m_uv1 %*% v_b)
      e_y2 <- exp(m_uv2 %*% v_b)
      numerator <- sum(df_uv$weight * (e_y2 - e_y1) * df_uv$sign)
      denominator <- sum(df_uv$weight * (u2 - u1) * df_uv$sign)
      est <- numerator / denominator
    }
    
    if(link == 'logit') {
      e_y1 <- ilogit(m_uv1 %*% v_b)
      e_y2 <- ilogit(m_uv2 %*% v_b)
      numerator <- sum(df_uv$weight * (e_y2 - e_y1) * df_uv$sign)
      denominator <- sum(df_uv$weight * (u2 - u1) * df_uv$sign)
      est <- numerator / denominator
    }
    
  }
  
  # for model class lmerMod
  if(class(m) %in% 'lmerMod') {
    
    names(m@beta) <- c('Intercept', attributes(terms(m))$term.labels)
    v_var_id <- match(names(m@beta), names(df_uv1)) %>% 
      na.omit() %>% 
      c()
    
    # vector of regression coefs
    v_b <- m@beta[names(m@beta) %in% c('Intercept', u, v)] %>% 
      data.matrix()

    # matrix of input and other variables
    m_uv1 <- data.matrix(df_uv1[, v_var_id])
    m_uv2 <- data.matrix(df_uv2[, v_var_id])

    if(any(rownames(v_b) != colnames(m_uv1))) stop('error in matrix organization')
    if(any(rownames(v_b) != colnames(m_uv2))) stop('error in matrix organization')
        
    # division by link function types
    if(link == 'identity') {
      e_y1 <- m_uv1 %*% v_b
      e_y2 <- m_uv2 %*% v_b
      numerator <- sum(df_uv$weight * (e_y2 - e_y1) * df_uv$sign)
      denominator <- sum(df_uv$weight * (u2 - u1) * df_uv$sign)
      est <- numerator / denominator
    }
    
    if(link == 'log') {
      e_y1 <- exp(m_uv1 %*% v_b)
      e_y2 <- exp(m_uv2 %*% v_b)
      numerator <- sum(df_uv$weight * (e_y2 - e_y1) * df_uv$sign)
      denominator <- sum(df_uv$weight * (u2 - u1) * df_uv$sign)
      est <- numerator / denominator
    }
    
    if(link == 'logit') {
      e_y1 <- ilogit(m_uv1 %*% v_b)
      e_y2 <- ilogit(m_uv2 %*% v_b)
      numerator <- sum(df_uv$weight * (e_y2 - e_y1) * df_uv$sign)
      denominator <- sum(df_uv$weight * (u2 - u1) * df_uv$sign)
      est <- numerator / denominator
    }
    
  }
  
  #return(list(estimate = est, df_uv = df_uv))
  
#}
