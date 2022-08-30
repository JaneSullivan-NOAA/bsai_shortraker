
# BSAI shortraker models for September 2022 GPT

# Model 18 = ADMB models (5 separate RE models)
# Model 18.a = TMB model (now single REM)
# Model 22 = Add LLS estimate strata-specific PE and q (EDA showed shared PE
# and q were not statistically supported)

# Not enough data to estimate additional parameters unless PE is shared, which
# is not supported statistically:
# Model 22.2.a = Model 22.1 estimate extra biomass obs error
# Model 22.2.b = Model 22.1 estimate extra cpue obs error
# Model 22.2.c = Model 22.1 estimate extra biomass + cpue obs error

# set up ----

# assessment year
YEAR <- 2022

libs <- c('readr', 'dplyr', 'tidyr', 'ggplot2', 'cowplot')
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

# install.packages("devtools")
# devtools::install_github("JaneSullivan-NOAA/rema", dependencies = TRUE)
library(rema)

# folder set up
dat_path <- paste0("data/", YEAR); dir.create(dat_path)
out_path <- paste0("results/", YEAR); dir.create(out_path)

ggplot2::theme_set(cowplot::theme_cowplot(font_size = 12) +
                     cowplot::background_grid() +
                     cowplot::panel_border())

biomass_dat <- read_csv(paste0(dat_path, "/bsai_shortraker_biomass_", YEAR, ".csv")) 
cpue_dat <- read_csv(paste0(dat_path, "/bsai_shortraker_rpw_", YEAR, ".csv")) 

# BSAI shortraker biomass estimation

# M 18.a: BTS only as REM ----

input <- prepare_rema_input(model_name = 'Model 18.a',
                            biomass_dat = biomass_dat,
                            zeros = list(assumption = 'NA'),
                            end_year = YEAR)

m1 <- fit_rema(input)

output1 <- tidy_rema(rema_model = m1)
output1$parameter_estimates # estimated fixed effects parameters

plots1 <- plot_rema(tidy_rema = output1,
                   # optional y-axis label
                   biomass_ylab = 'Biomass (t)')
plots1$biomass_by_strata

# M 22.1 BTS + LLS ----

input <- prepare_rema_input(model_name = 'Model 22.1',
                            multi_survey = 1, # fit to CPUE data? yes = 1)
                            biomass_dat = biomass_dat,
                            cpue_dat = cpue_dat,
                            sum_cpue_index = 1, # is the CPUE index summable (yes = 1, RPWs are summable)
                            zeros = list(assumption = 'NA'),
                            end_year = YEAR,
                            # sort(unique(biomass_dat$strata)) = "Central AI"
                            # "Eastern AI" "EBS Slope" "SBS" "Western AI". EBS
                            # Slope is stratum 3 for the biomass so we put it in
                            # the third position of the
                            # pointer_biomass_cpue_strata object and use NAs for
                            # the other 4 strata. It is the first (and only)
                            # stratum for the LLS, so we use the
                            # value of 1. See Details in ?prepare_rema_input
                            q_options = list(pointer_biomass_cpue_strata = c(NA, NA, 1, NA, NA)))

m2 <- fit_rema(input)

output2 <- tidy_rema(rema_model = m2)
output2$parameter_estimates # estimated fixed effects parameters
plots2 <- plot_rema(tidy_rema = output2,
                   biomass_ylab = 'Biomass (t)',
                   cpue_ylab = 'Relative population weights')
plots2$biomass_by_strata
plots2$cpue_by_strata

# Model comparison ----
compare <- compare_rema_models(list(m1, m2), biomass_ylab = 'Biomass (t)', cpue_ylab = 'RPW')
compare$plots$biomass_by_strata + 
  facet_wrap(~strata, scales = 'free_y', ncol = 1) +
  ggplot2::scale_fill_viridis_d(direction = -1) +
  ggplot2::scale_colour_viridis_d(direction = -1)
ggsave(paste0(out_path, '/M18.a_M22.1_biomass_fits.png'), units = 'in', bg = 'white',
       height = 12, width = 8, dpi = 300)

compare$plots$total_predicted_biomass +
  ggplot2::scale_fill_viridis_d(direction = -1) +
  ggplot2::scale_colour_viridis_d(direction = -1)
ggsave(paste0(out_path, '/M18.a_M22.1_total_biomass.png'), units = 'in', bg = 'white',
       height = 3.5, width = 7, dpi = 300)

compare <- compare_rema_models(list(m2), biomass_ylab = 'Biomass (t)', cpue_ylab = 'RPW')
compare$plots$cpue_by_strata + 
  facet_wrap(~strata, scales = 'free_y', ncol = 1)
ggsave(paste0(out_path, '/M22.1_rpw_fit_slope.png'), units = 'in', bg = 'white',
       height = 3.5, width = 7, dpi = 300)

# Model 22.2.a extra biomass variance ----

input <- prepare_rema_input(model_name = 'Model 22.2.a',
                            multi_survey = 1,
                            biomass_dat = biomass_dat,
                            cpue_dat = cpue_dat,
                            sum_cpue_index = 1,
                            zeros = list(assumption = 'NA'),
                            end_year = YEAR,
                            PE_options = list(pointer_PE_biomass = c(1, 1, 1, 1, 1)),
                            q_options = list(pointer_biomass_cpue_strata = c(NA, NA, 1, NA, NA)),
                            extra_biomass_cv = list(assumption = 'extra_cv'))

m3 <- fit_rema(input)
output3 <- tidy_rema(m3)
output3$parameter_estimates

# Model 22.2.b extra cpue variance ----

input <- prepare_rema_input(model_name = 'Model 22.2.b',
                            multi_survey = 1, 
                            biomass_dat = biomass_dat,
                            cpue_dat = cpue_dat,
                            sum_cpue_index = 1, 
                            zeros = list(assumption = 'NA'),
                            end_year = YEAR,
                            PE_options = list(pointer_PE_biomass = c(1, 1, 1, 1, 1)),
                            q_options = list(pointer_biomass_cpue_strata = c(NA, NA, 1, NA, NA)),
                            extra_cpue_cv = list(assumption = 'extra_cv'))

m4 <- fit_rema(input)
output4 <- tidy_rema(m4)
output4$parameter_estimates

# Model 22.2.c extra biomass + cpue variance ----

input <- prepare_rema_input(model_name = 'Model 22.2.c',
                            multi_survey = 1, 
                            biomass_dat = biomass_dat,
                            cpue_dat = cpue_dat,
                            sum_cpue_index = 1, 
                            zeros = list(assumption = 'NA'),
                            end_year = YEAR,
                            PE_options = list(pointer_PE_biomass = c(1, 1, 1, 1, 1)),
                            q_options = list(pointer_biomass_cpue_strata = c(NA, NA, 1, NA, NA)),
                            extra_biomass_cv = list(assumption = 'extra_cv'),
                            extra_cpue_cv = list(assumption = 'extra_cv'))

m5 <- fit_rema(input)
output5 <- tidy_rema(m5)
output5$parameter_estimates

compare <- compare_rema_models(list(m2, m3, m4, m5), biomass_ylab = 'Biomass (t)', cpue_ylab = 'RPW')
compare$aic
compare$aic %>% write_csv(paste0(out_path, '/aic.csv'))
compare$plots$biomass_by_strata + 
  facet_wrap(~strata, ncol = 1, scales = 'free_y')
ggsave(paste0(out_path, '/M22.1_M22.2.abc_biomass_fits.png'), units = 'in', bg = 'white',
       height = 12, width = 8, dpi = 300)

compare$plots$cpue_by_strata + 
  facet_wrap(~strata, ncol = 1, scales = 'free_y')
ggsave(paste0(out_path, '/M22.1_M22.2.abc_cpue_fits.png'), units = 'in', bg = 'white',
       height = 3.5, width = 7, dpi = 300)

compare <- compare_rema_models(list(m1, m2, m3, m4, m5), biomass_ylab = 'Biomass (t)', cpue_ylab = 'RPW')
compare$plots$total_predicted_biomass +
  geom_line(size = 0.8)
ggsave(paste0(out_path, '/M18.a_M22.1_M22.2.abc_total_biomass.png'), units = 'in', bg = 'white',
       height = 3.5, width = 7, dpi = 300)
