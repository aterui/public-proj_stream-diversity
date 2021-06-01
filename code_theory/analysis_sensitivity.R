
# setup -------------------------------------------------------------------

  pacman::p_load(tidyverse, broom)


# data --------------------------------------------------------------------

  dat_slope <- read_csv(here::here('code_theory/result/result_sensitivity.csv')) %>% 
      filter(gamma_div > 0 & alpha_div > 0) %>% 
      mutate(beta_div = gamma_div/alpha_div) %>% 
      mutate(group_id = group_indices(., sd_env_source)) %>% 
      group_by(group_id) %>% 
      do(slope_alpha_area = c(coef(lm(log(alpha_div, 10) ~ log(n_patch, 10) + log(p_branch, 10), data = .))[2]),
         slope_beta_area = c(coef(lm(log(beta_div, 10) ~ log(n_patch, 10) + log(p_branch, 10), data = .))[2]),
         slope_gamma_area = c(coef(lm(log(gamma_div, 10) ~ log(n_patch, 10) + log(p_branch, 10), data = .))[2]),
         slope_alpha_bp = c(coef(lm(log(alpha_div, 10) ~ log(n_patch, 10) + log(p_branch, 10), data = .))[3]),
         slope_beta_bp = c(coef(lm(log(beta_div, 10) ~ log(n_patch, 10) + log(p_branch, 10), data = .))[3]),
         slope_gamma_bp = c(coef(lm(log(gamma_div, 10) ~ log(n_patch, 10) + log(p_branch, 10), data = .))[3])) %>% 
      summarize(across(everything(), as.numeric))
    
  dat_para <- read_csv(here::here('code_theory/result/result_sensitivity.csv')) %>% 
    mutate(group_id = group_indices(., sd_env_source)) %>% 
    group_by(group_id) %>% 
    summarize(sigma_h = unique(sd_env_source),
              sigma_l = unique(sd_env_lon),
              sigma_z = unique(sd_env),
              phi = unique(phi),
              nu = unique(niche_cost),
              max_alpha = unique(max_alpha),
              theta = unique(theta),
              p_d = unique(p_dispersal))
      
  dat_sense <- dat_para %>%
    left_join(dat_slope, by = 'group_id') %>% 
    pivot_longer(cols = starts_with('slope'),
                 names_to = 'response',
                 values_to = 'slope')
  
  fit_sense <- dat_sense %>% 
    summarize(across(.cols = c(-group_id,
                               -response,
                               -slope),
                     .fns = function(x) (x - mean(x))/sd(x)),
              group_id = group_id,
              response = response,
              slope = slope) %>% 
    group_by(response) %>% 
    do(fit = lm(slope ~ sigma_h + sigma_l + sigma_z + phi + nu + max_alpha + theta + p_d, data = .))
