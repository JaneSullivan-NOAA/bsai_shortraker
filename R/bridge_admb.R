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

# EAI ----

admb_eai <- read_admb_re(filename = 'data/original_admb/EAI_SR.rep',
                        model_name = 'ADMB EAI',
                        biomass_strata_names = 'Eastern AI')

input <- prepare_rema_input(model_name = 'TMB EAI',
                            admb_re = admb_eai)
mod_eai <- fit_rema(input)

compare_eai <- compare_rema_models(list(mod_eai), admb_re = admb_eai)
p1 <- compare_eai$plots$biomass_by_strata

# CAI ----

admb_cai <- read_admb_re(filename = 'data/original_admb/CAI_SR.rep',
                        model_name = 'ADMB CAI',
                        biomass_strata_names = 'Central AI')

input <- prepare_rema_input(model_name = 'TMB CAI',
                            admb_re = admb_cai)
mod_cai <- fit_rema(input)

compare_cai <- compare_rema_models(list(mod_cai), admb_re = admb_cai)
p2 <- compare_cai$plots$biomass_by_strata

# WAI ----

admb_wai <- read_admb_re(filename = 'data/original_admb/WAI_SR.rep',
                        model_name = 'ADMB WAI',
                        biomass_strata_names = 'Western AI')

input <- prepare_rema_input(model_name = 'TMB WAI',
                            admb_re = admb_wai)
mod_wai <- fit_rema(input)

compare_wai <- compare_rema_models(list(mod_wai), admb_re = admb_wai)
p3 <- compare_wai$plots$biomass_by_strata

# SBS ----

admb_sbs <- read_admb_re(filename = 'data/original_admb/SR_SBS2018.rep',
                        model_name = 'ADMB SBS',
                        biomass_strata_names = 'SBS')

input <- prepare_rema_input(model_name = 'TMB SBS',
                            admb_re = admb_sbs)
mod_sbs <- fit_rema(input)

compare_sbs <- compare_rema_models(list(mod_sbs), admb_re = admb_sbs)
p4 <- compare_sbs$plots$biomass_by_strata

# EBS Slope ----

admb_slope <- read_admb_re(filename = 'data/original_admb/SR_slope2018.rep',
                        model_name = 'ADMB EBS SLOPE',
                        biomass_strata_names = 'EBS Slope')

input <- prepare_rema_input(model_name = 'TMB EBS SLOPE',
                            admb_re = admb_slope)
mod_slope <- fit_rema(input)

compare_slope <- compare_rema_models(list(mod_slope), admb_re = admb_slope)
p5 <- compare_slope$plots$biomass_by_strata


# plot -----

cowplot::plot_grid(p1, p2, p3, p4, p5, ncol = 1)
ggsave(paste0(out_path, '/bridge_admb_model.png'), units = 'in', bg = 'white',
       height = 12, width = 8, dpi = 300)


# check data ---

biomass_dat <- read_csv(paste0(dat_path, "/bsai_shortraker_biomass_", YEAR, ".csv")) 
cpue_dat <- read_csv(paste0(dat_path, "/bsai_shortraker_rpw_", YEAR, ".csv")) 

unique(biomass_dat$strata)
check_data <- biomass_dat %>% 
  left_join(admb_cai$biomass_dat %>% 
              bind_rows(admb_eai$biomass_dat) %>% 
              bind_rows(admb_wai$biomass_dat) %>% 
              bind_rows(admb_sbs$biomass_dat) %>% 
              bind_rows(admb_slope$biomass_dat) %>% 
              rename(old_biomass = biomass, old_cv = cv))

check_data %>% 
  mutate(tst_biomass = biomass - old_biomass) %>% 
  filter(tst_biomass > 0.001)

check_data %>% 
  mutate(tst_cv = cv - old_cv) %>% 
  filter(tst_cv > 0.001)
