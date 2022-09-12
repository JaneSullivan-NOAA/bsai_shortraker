# BSAI shortraker bridge ADMB to TMB

# set up ----

# assessment year
YEAR <- 2022

libs <- c('readr', 'dplyr', 'tidyr', 'ggplot2', 'cowplot')
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

# install.packages("devtools")
# devtools::install_github("afsc-assessments/rema", dependencies = TRUE, build_vignettes = TRUE)
library(rema)

# folder set up
dat_path <- paste0("data/", YEAR); dir.create(dat_path)
out_path <- paste0("results/", YEAR); dir.create(out_path)

ggplot2::theme_set(cowplot::theme_cowplot(font_size = 12) +
                     cowplot::background_grid() +
                     cowplot::panel_border())

# AI ----

admb_ai <- read_admb_re(filename = 'data/original_admb/SR_AI2018.rep',
                        model_name = 'ADMB AI',
                        biomass_strata_names = 'AI')

input <- prepare_rema_input(model_name = 'TMB AI',
                            admb_re = admb_ai)
mod_ai <- fit_rema(input)
out_ai <- tidy_rema(mod_ai)

compare_ai <- compare_rema_models(list(mod_ai), admb_re = admb_ai)
p1 <- compare_ai$plots$biomass_by_strata
p1

# SBS ----

admb_sbs <- read_admb_re(filename = 'data/original_admb/SR_SBS2018.rep',
                        model_name = 'ADMB SBS',
                        biomass_strata_names = 'SBS')

input <- prepare_rema_input(model_name = 'TMB SBS',
                            admb_re = admb_sbs)
mod_sbs <- fit_rema(input)
out_sbs <- tidy_rema(mod_sbs)

compare_sbs <- compare_rema_models(list(mod_sbs), admb_re = admb_sbs)
p2 <- compare_sbs$plots$biomass_by_strata
p2

# EBS Slope ----

admb_slope <- read_admb_re(filename = 'data/original_admb/SR_slope2018_thru2018.rep',
                        model_name = 'ADMB EBS SLOPE',
                        biomass_strata_names = 'EBS Slope')

input <- prepare_rema_input(model_name = 'TMB EBS SLOPE',
                            admb_re = admb_slope)
mod_slope <- fit_rema(input)
out_slope <- tidy_rema(mod_slope)

compare_slope <- compare_rema_models(list(mod_slope), admb_re = admb_slope)
p3 <- compare_slope$plots$biomass_by_strata
p3

# plot -----

cowplot::plot_grid(p1, p2, p3, ncol = 1)
ggsave(paste0(out_path, '/bridge_admb_model_v1.png'), units = 'in', bg = 'white',
       height = 12, width = 8, dpi = 300)

# check data ----

biomass_dat <- read_csv(paste0(dat_path, "/bsai_shortraker_biomass_", YEAR, ".csv")) 
cpue_dat <- read_csv(paste0(dat_path, "/bsai_shortraker_rpw_", YEAR, ".csv")) 

unique(biomass_dat$strata)
check_data <- biomass_dat %>% 
  left_join(admb_ai$biomass_dat %>% 
              bind_rows(admb_sbs$biomass_dat) %>% 
              bind_rows(admb_slope$biomass_dat) %>% 
              rename(old_biomass = biomass, old_cv = cv))

check_data %>% 
  mutate(tst_biomass = biomass - old_biomass) %>% 
  filter(tst_biomass > 0.001)

check_data %>% 
  mutate(tst_cv = cv - old_cv) %>% 
  filter(tst_cv > 0.001)

# save output for full model comparisons ----
biom_compare <- out_ai$biomass_by_strata %>% 
  bind_rows(out_sbs$biomass_by_strata) %>% 
  bind_rows(out_slope$biomass_by_strata) %>% 
  mutate(model_name = 'Model 18.a') %>% 
  bind_rows(admb_ai$admb_re_results$biomass_by_strata %>% 
              bind_rows(admb_sbs$admb_re_results$biomass_by_strata) %>% 
              bind_rows(admb_slope$admb_re_results$biomass_by_strata) %>% 
              mutate(model_name = 'Model 18')) %>% 
  write_csv(paste0(out_path, '/M18_M18a_biomass_by_strata.csv'))

biom_compare %>% 
  group_by(model_name, year) %>% 
  summarize(biomass = sum(pred)) %>% 
  pivot_wider(id_cols = year, names_from = model_name, values_from = biomass) %>% 
  print(n = Inf)

out_ai$parameter_estimates %>%
  mutate(parameter = 'AI process error') %>% 
  bind_rows(out_sbs$parameter_estimates %>%
              mutate(parameter = 'SBS process error')) %>% 
  bind_rows(out_slope$parameter_estimates %>%
              mutate(parameter = 'EBS Slope process error')) %>% 
  mutate(model_name = 'Model 18.a') %>% 
  write_csv(paste0(out_path, '/M18a_parameter_estimates.csv'))