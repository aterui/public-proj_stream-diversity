
# setup -------------------------------------------------------------------

  pacman::p_load(tidyverse, knitr, kableExtra)
  source(here::here("code_theory/analysis_sensitivity.R"))


# sensitivity tables ------------------------------------------------------

  ## ecosystem size
  m_area <- fit_sense %>% 
    mutate(area = str_detect(response, "area")) %>% 
    filter(area == TRUE)
  
  star_area <- stargazer::stargazer(m_area$fit[1],
                                    m_area$fit[2],
                                    m_area$fit[3],
                                    header = FALSE,
                                    type = "latex",
                                    covariate.labels = c("$\\sigma_{h}$",
                                                         "$\\sigma_{l}$",
                                                         "$\\sigma_{z}$",
                                                         "$\\phi$",
                                                         "$\\nu$",
                                                         "$b_{max}$",
                                                         "$\\theta$",
                                                         "$p_{d}$",
                                                         "Intercept"),
                                    single.row = FALSE,
                                    digits = 3,
                                    dep.var.caption  = "Response variable",
                                    dep.var.labels.include = FALSE,
                                    column.labels = c("Effect of $N_{p}$ on $\\alpha$ diversity",
                                                      "Effect of $N_{p}$ on $\\beta$ diversity",
                                                      "Effect of $N_{p}$ on $\\gamma$ diversity"),
                                    report = "vcs",
                                    omit.table.layout = "ns",
                                    model.numbers = FALSE)
  
  star_area <- sub('^.+\\caption.+$','', star_area)
  
  ## Ecosystem complexity
  m_bp <- fit_sense %>% 
    mutate(area = str_detect(response, "area")) %>% 
    filter(area == FALSE)
  
  star_bp <- stargazer::stargazer(m_bp$fit[1],
                                  m_bp$fit[2],
                                  m_bp$fit[3],
                                  header = FALSE,
                                  type = "latex",
                                  covariate.labels = c("$\\sigma_{h}$",
                                                       "$\\sigma_{l}$",
                                                       "$\\sigma_{z}$",
                                                       "$\\phi$",
                                                       "$\\nu$",
                                                       "$b_{max}$",
                                                       "$\\theta$",
                                                       "$p_{d}$",
                                                       "Intercept"),
                                  single.row = FALSE,
                                  digits = 3,
                                  dep.var.caption  = "Response variable",
                                  dep.var.labels.include = FALSE,
                                  column.labels = c("Effect of $P_{b}$ on $\\alpha$ diversity",
                                                    "Effect of $P_{b}$ on $\\beta$ diversity",
                                                    "Effect of $P_{b}$ on $\\gamma$ diversity"),
                                  report = "vcs",
                                  omit.table.layout = "ns",
                                  model.numbers = FALSE)
  
  star_bp <- sub('^.+\\caption.+$','', star_bp)
