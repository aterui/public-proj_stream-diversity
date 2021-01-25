
# setup -------------------------------------------------------------------

  pacman::p_load(tidyverse, knitr, kableExtra)
  dat_sim <- read_csv(here::here('/theory/result/result_sim2020-11-10.csv'))

# table -------------------------------------------------------------------

  dat_sim %>% 
    select(sd_env_source,
           sd_env_lon,
           sd_env,
           rho,
           phi,
           niche_cost,
           max_alpha,
           theta,
           p_dispersal
    ) %>% 
    summarise(across(everything(), unique)) %>% 
    mutate(r0 = 4) %>% 
    pivot_longer(cols = everything(),
                 names_to = "Parameter",
                 values_to = "Value") %>% 
    group_by(Parameter) %>% 
    summarize(Value = list(unique(Value))) %>% 
    mutate(id = case_when(Parameter == 'sd_env_source' ~ 'a',
                          Parameter == 'sd_env_lon' ~ 'b',
                          Parameter == 'sd_env' ~ 'c',
                          Parameter == 'rho' ~ 'd',
                          Parameter == 'phi' ~ 'e',
                          Parameter == 'niche_cost' ~ 'f',
                          Parameter == 'max_alpha' ~ 'g',
                          Parameter == 'theta' ~ 'h',
                          Parameter == 'p_dispersal' ~ 'i',
                          Parameter == 'r0' ~ 'j',
                          TRUE ~ as.character(Parameter)),
           Interpretation = case_when(Parameter == 'sd_env_source' ~ 'Environmental variation at headwaters',
                                      Parameter == 'sd_env_lon' ~ 'Degree of local environmental noise',
                                      Parameter == 'sd_env' ~ 'Temporal environmental variability',
                                      Parameter == 'rho' ~ 'Strength of spatial autocorrelation in mean environmental condition',
                                      Parameter == 'phi' ~ 'Extent of spatial autocorrelation in temporal environmental variation',
                                      Parameter == 'niche_cost' ~ 'Cost of a wider niche',
                                      Parameter == 'max_alpha' ~ 'Maximum value of interspecific competition coefficient',
                                      Parameter == 'theta' ~ 'Rate parameter of an exponential dispersal kernel',
                                      Parameter == 'p_dispersal' ~ 'Dispersal probability',
                                      Parameter == 'r0' ~ 'Maxiumum reproductive rate',
                                      TRUE ~ as.character(Parameter)),
           Parameter = case_when(Parameter == 'sd_env_source' ~ '$\\sigma_{h}$',
                                 Parameter == 'sd_env_lon' ~ '$\\sigma_{l}$',
                                 Parameter == 'sd_env' ~ '$\\sigma_{z}$',
                                 Parameter == 'rho' ~ '$\\rho$',
                                 Parameter == 'phi' ~ '$\\phi$',
                                 Parameter == 'niche_cost' ~ '$\\nu$',
                                 Parameter == 'max_alpha' ~ '$\\alpha_{max}$',
                                 Parameter == 'theta' ~ '$\\theta$',
                                 Parameter == 'p_dispersal' ~ '$p_{d}$',
                                 Parameter == 'r0' ~ '$r_{0,i}$',
                                 TRUE ~ as.character(Parameter))
    ) %>% 
    arrange(id) %>% 
    select(-id) %>% 
    kable(format = 'markdown')
