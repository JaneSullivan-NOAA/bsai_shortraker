
# BSAI shortraker models for September 2022 GPT

# Model 18 = ADMB models (3 separate RE models, 1 for AI, SBS, and EBS slope)
# Model 18.a = TMB model (3 separate RE models)
# Model 18.b = TMB model (now single REM)
# Model 22 = Add LLS estimate strata-specific PE and EBS slope q

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
# devtools::install_github("afsc-assessments/rema", dependencies = TRUE, build_vignettes = TRUE)
library(rema)

# folder set up
dat_path <- paste0("data/", YEAR); dir.create(dat_path)
out_path <- paste0("results/", YEAR); dir.create(out_path)

ggplot2::theme_set(cowplot::theme_cowplot(font_size = 12) +
                     cowplot::background_grid() +
                     cowplot::panel_border())

biomass_dat <- read_csv(paste0(dat_path, "/bsai_shortraker_biomass_", YEAR, ".csv")) 
cpue_dat <- read_csv(paste0(dat_path, "/bsai_shortraker_rpw_", YEAR, ".csv")) 

YEAR <- 2018

# BSAI shortraker biomass estimation

# M 18 and M 18.a results ----

# from bridge_admb.R
bridge_res <- read_csv(paste0(out_path, '/M18_M18a_biomass_by_strata.csv'))
m18a_pars <-  read_csv(paste0(out_path, '/M18a_parameter_estimates.csv'))

# M 18.b: BTS only as REM ----

input <- prepare_rema_input(model_name = 'Model 18.b',
                            biomass_dat = biomass_dat,
                            zeros = list(assumption = 'NA'),
                            end_year = YEAR)

m1 <- fit_rema(input)

output1 <- tidy_rema(rema_model = m1)
output1$parameter_estimates # estimated fixed effects parameters

# compare bridged mods ----

bridge_res %>% 
  bind_rows(output1$biomass_by_strata) %>% 
  ggplot(aes(x = year, y = pred,
                 col = model_name)) +
  geom_ribbon(aes(ymin = pred_lci, ymax = pred_uci,
                  fill = model_name), col = NA,
              alpha = 0.25) +
  geom_line(aes(lty = model_name), size = 0.8) +
  facet_wrap(~strata, ncol = 1, scales = 'free_y') +
  geom_point(aes(x = year, y = obs), col = 'black') +
  geom_errorbar(aes(x = year, ymin = obs_lci, ymax = obs_uci), col = 'black') +
  scale_y_continuous(labels = scales::comma, expand = c(0.01, 0), limits = c(0, NA)) +
  labs(x = NULL, y = 'Biomass (t)',
       fill = NULL, colour = NULL, shape = NULL, lty = NULL) +
  ggplot2::scale_fill_viridis_d(direction = 1) +
  ggplot2::scale_colour_viridis_d(direction = 1)

ggsave(paste0(out_path, '/bridge_admb_model.png'), units = 'in', bg = 'white',
       height = 7, width = 6.5, dpi = 300)

# check that we're getting the same ABCs! whew.
bridge_res %>% 
  bind_rows(output1$biomass_by_strata) %>% 
  group_by(model_name, year) %>% 
  dplyr::summarise(biomass = sum(pred)) %>% 
  filter(year == 2018)

# M 22 (22.1) BTS + LLS ----

input <- prepare_rema_input(model_name = 'Model 22',
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
                            q_options = list(pointer_biomass_cpue_strata = c(NA, 1, NA)))

m2 <- fit_rema(input)

output2 <- tidy_rema(rema_model = m2)
output2$parameter_estimates # estimated fixed effects parameters
plots2 <- plot_rema(tidy_rema = output2,
                   biomass_ylab = 'Biomass (t)',
                   cpue_ylab = 'Relative population weights')
plots2$biomass_by_strata

# Model comparison ----
compare <- compare_rema_models(list(m1, m2), biomass_ylab = 'Biomass (t)', cpue_ylab = 'RPW')
p1 <- compare$plots$biomass_by_strata + 
  facet_wrap(~strata, scales = 'free_y', ncol = 1) +
  labs(title = 'Bottom trawl survey (BTS) biomass (t) by region')

p1

ggsave(paste0(out_path, '/M18.b_M22_biomass_fits.png'), units = 'in', bg = 'white',
       height = 7.5, width = 6.5, dpi = 300)

compare <- compare_rema_models(list(m2), biomass_ylab = 'Biomass (t)', cpue_ylab = 'RPW')
p2 <- compare$plots$cpue_by_strata + 
  facet_wrap(~strata, scales = 'free_y', ncol = 1)+
  ggplot2::scale_fill_viridis_d(direction = -1) +
  ggplot2::scale_colour_viridis_d(direction = -1) +
  labs(title = 'Longline survey (LLS) relative population weights (RPW)')
p2
ggsave(paste0(out_path, '/M22_rpw_fit_slope.png'), units = 'in', bg = 'white',
       height = 3.5, width = 7, dpi = 300)

cowplot::plot_grid(p1, p2, ncol = 1, rel_heights = c(0.7, 0.3))
ggsave(paste0(out_path, '/M18b_M22_bts_lls.png'), units = 'in', bg = 'white',
       height = 7.5, width = 7, dpi = 300)

# total biomass ----
compare <- compare_rema_models(list(m1, m2))
compare$output$total_predicted_biomass

biom <- bridge_res %>% 
  group_by(model_name, year) %>% 
  summarize(biomass = sum(pred)) %>% 
  ungroup() %>% 
  bind_rows(compare$output$total_predicted_biomass %>% 
              select(model_name, year, biomass = pred)) 
biom %>% 
  pivot_wider(id_cols = year, names_from = model_name, values_from = biomass) %>% 
  write_csv(paste0(out_path, '/total_biomass.csv'))

sumtable <- biom %>% 
  filter(year == 2018) %>% 
  mutate(M = 0.03,
         OFL = M * biomass,
         maxABC = 0.75 * M * biomass)

statquo_biomass <- sumtable %>% filter(model_name == 'Model 18') %>% pull(biomass)
statquo_OFL <- sumtable %>% filter(model_name == 'Model 18') %>% pull(OFL)
statquo_maxABC <- sumtable %>% filter(model_name == 'Model 18') %>% pull(maxABC)

sumtable %>%
  mutate(biomass_percent_change = (biomass - statquo_biomass)/statquo_biomass * 100,
         OFL_maxABC_percent_change = (OFL - statquo_OFL)/statquo_OFL * 100) %>%
  write_csv(paste0(out_path, '/abc_ofl_summary.csv'))

compare$output$parameter_estimates %>% 
  bind_rows(m18a_pars) %>% 
  arrange(model_name) %>% 
  write_csv(paste0(out_path, '/parameters_estimates.csv'))

# STOP -----

# Model 22.2.a extra biomass variance ----

input <- prepare_rema_input(model_name = 'Model 22.2.a',
                            multi_survey = 1,
                            biomass_dat = biomass_dat,
                            cpue_dat = cpue_dat,
                            sum_cpue_index = 1,
                            zeros = list(assumption = 'NA'),
                            end_year = YEAR,
                            PE_options = list(pointer_PE_biomass = c(1, 1, 1)),
                            q_options = list(pointer_biomass_cpue_strata = c(NA, 1, NA)),
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
                            PE_options = list(pointer_PE_biomass = c(1, 1, 1)),
                            q_options = list(pointer_biomass_cpue_strata = c(NA, 1, NA)),
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
                            PE_options = list(pointer_PE_biomass = c(1, 1, 1)),
                            q_options = list(pointer_biomass_cpue_strata = c(NA, 1, NA)),
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
       height = 7.5, width = 6.5, dpi = 300)

compare$plots$cpue_by_strata + 
  facet_wrap(~strata, ncol = 1, scales = 'free_y')
ggsave(paste0(out_path, '/M22.1_M22.2.abc_cpue_fits.png'), units = 'in', bg = 'white',
       height = 3.5, width = 7, dpi = 300)

compare <- compare_rema_models(list(m1, m2, m3, m4, m5), biomass_ylab = 'Biomass (t)', cpue_ylab = 'RPW')
compare$plots$total_predicted_biomass +
  geom_line(size = 0.8)
ggsave(paste0(out_path, '/M18.b_M22.1_M22.2.abc_total_biomass.png'), units = 'in', bg = 'white',
       height = 3.5, width = 7, dpi = 300)

compare$output$total_predicted_biomass %>% 
  pivot_wider(id_cols = year, names_from = model_name, values_from = pred) %>% 
  write_csv(paste0(out_path, '/M18.b_M22.1_M22.2.abc_total_biomass.csv'))

compare$output$parameter_estimates %>% 
  write_csv(paste0(out_path, '/M18.b_M22.1_M22.2.abc_parameter_estimates.csv'))

# save data reformatted ----
biomass_dat %>%
  tidyr::expand(year = min(biomass_dat$year):(YEAR-1),
                strata) %>%
  left_join(biomass_dat %>%
              mutate(value = ifelse(is.na(biomass), NA,
                                    paste0(prettyNum(round(biomass, 0), big.mark = ','), ' (',
                                           format(round(cv, 3), nsmall = 3, trim = TRUE), ')')))) %>%
  pivot_wider(id_cols = c(year), names_from = strata, values_from = value) %>%
  arrange(year) %>%
  select(Year = year, `Eastern AI`, `Central AI`, `Western AI`, SBS, `EBS Slope`) %>% 
  write_csv(paste0(out_path, '/biomass_data_wide.csv'))

cpue_dat %>%
  tidyr::expand(year = min(cpue_dat$year):(YEAR-1),
                strata) %>%
  left_join(cpue_dat %>%
              mutate(value = ifelse(is.na(cpue), NA,
                                    paste0(prettyNum(round(cpue, 0), big.mark = ','), ' (',
                                           format(round(cv, 3), nsmall = 3, trim = TRUE), ')')))) %>%
  pivot_wider(id_cols = c(year), names_from = strata, values_from = value) %>%
  arrange(year) %>%
  select(Year = year, `EBS Slope LLS Relative Population Weight (CV)` = `EBS Slope`) %>% 
  write_csv(paste0(out_path, '/cpue_data_wide.csv'))

# percent changes -----

cpue_dat %>% filter(year %in% c(2019,2021)) %>%
  pivot_wider(id_cols = c(strata), names_from = year, values_from = cpue) %>%
  mutate(percent_change = (`2021`-`2019`)/`2019`)



old <- out1$total_predicted_biomass %>% filter(year == 2022) %>% pull(pred)
new <- out2$total_predicted_biomass %>% filter(year == 2022) %>% pull(pred)
(new-old)/old

full_sumtable <- compare$output$total_predicted_biomass %>%
  rename(total_biomass = pred) %>% 
  filter(year == YEAR) %>%
  mutate(natmat = 0.03,
         OFL = natmat * total_biomass,
         maxABC = 0.75 * natmat * total_biomass,
         ABC = maxABC)
sumtable <- full_sumtable %>%
  distinct(model_name, year, biomass = total_biomass, OFL, maxABC) %>%
  select(model_name, year, biomass, OFL, maxABC)

statquo_biomass <- sumtable %>% filter(model_name == 'Model 18.b') %>% pull(biomass)
# statquo_OFL <- sumtable %>% filter(model_name == 'Model 18.b') %>% pull(OFL)
# statquo_maxABC <- sumtable %>% filter(model_name == 'Model 18.b') %>% pull(maxABC)

sumtable %>%
  mutate(percent_change = (biomass - statquo_biomass)/statquo_biomass * 100) %>% 
  write_csv(paste0(out_path, '/summary_table.csv'))

compare$output$total_predicted_biomass %>%
  filter(year %in% c(2018, 2022)) %>%
  pivot_wider(id_cols = model_name, names_from = year, values_from = pred) %>%
  mutate(percent_change = (`2022`-`2018`)/`2018` * 100)
