
# BSAI shortraker models for November 2024 GPT
# Model 22 = Add LLS estimate strata-specific PE and EBS slope q, named M22_2024
# Model 24, same as 2022 but with new data, named M22_2024

# set up ----

# assessment year
YEAR <- 2024

libs <- c('readr', 'dplyr', 'tidyr', 'ggplot2', 'cowplot')
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

# install.packages("devtools")
# devtools::install_github("afsc-assessments/rema", dependencies = TRUE, build_vignettes = TRUE)
# install.packages("pak")
# pak::pkg_install("afsc-assessments/rema")
library(pak)
library(rema)

# folder set up
dat_path <- paste0("data/", YEAR); dir.create(dat_path)
out_path <- paste0("results/", YEAR); dir.create(out_path)

ggplot2::theme_set(cowplot::theme_cowplot(font_size = 12) +
                     cowplot::background_grid() +
                     cowplot::panel_border())

biomass_dat <- read_csv(paste0(dat_path, "/bsai_shortraker_biomass_", YEAR, ".csv")) 
cpue_dat <- read_csv(paste0(dat_path, "/bsai_shortraker_rpw_", YEAR, ".csv")) 

biomass_dat <- biomass_dat %>% 
  dplyr::filter(!(strata=="AI" & year < 1991))%>%
  dplyr::filter(!(strata=="SBS" & year < 1991))

# BSAI shortraker biomass estimation,
#note zero treated as failed survey in model note that in table

# M 22: BTS + LLS ----

input <- rema::prepare_rema_input(model_name = 'M22_2024',
                            multi_survey = 1, # fit to CPUE data? yes = 1)
                            biomass_dat = biomass_dat,
                            cpue_dat = cpue_dat,
                            sum_cpue_index = 1, # is the CPUE index summable (yes = 1, RPWs are summable)
                            zeros = list(assumption = 'NA'),
                            end_year = YEAR,
                            # sort(unique(biomass_dat$strata)) = 
                            # "AI" "EBS Slope" "SBS" 
                            # EBS Slope is stratum 2 for the biomass so we put it in
                            # the second position of the
                            # pointer_biomass_cpue_strata object and use NAs for
                            # the other 2 strata. It is the first (and only)
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
plots2$cpue_by_strata

# M22_2022

biomass_dat_22 <- read_csv(paste0("data/2022/bsai_shortraker_biomass_", YEAR-2, ".csv")) 
cpue_dat_22 <- read_csv(paste0("data/2022/bsai_shortraker_rpw_", YEAR-2, ".csv")) 

biomass_dat_22 <- biomass_dat_22 %>% 
  dplyr::filter(!(strata=="AI" & year < 1991))%>%
  dplyr::filter(!(strata=="SBS" & year < 1991))

input2 <- rema::prepare_rema_input(model_name = 'M22_2022',
                                  multi_survey = 1, # fit to CPUE data? yes = 1)
                                  biomass_dat = biomass_dat_22,
                                  cpue_dat = cpue_dat_22,
                                  sum_cpue_index = 1, # is the CPUE index summable (yes = 1, RPWs are summable)
                                  zeros = list(assumption = 'NA'),
                                  end_year = YEAR-2,
                                  # sort(unique(biomass_dat$strata)) = 
                                  # "AI" "EBS Slope" "SBS" 
                                  # EBS Slope is stratum 2 for the biomass so we put it in
                                  # the second position of the
                                  # pointer_biomass_cpue_strata object and use NAs for
                                  # the other 2 strata. It is the first (and only)
                                  # stratum for the LLS, so we use the
                                  # value of 1. See Details in ?prepare_rema_input
                                  q_options = list(pointer_biomass_cpue_strata = c(NA, 1, NA)))

m22 <- fit_rema(input2)


# Model comparison ---- takes results from multiple models for model comparison
compare <- compare_rema_models(rema_models=list(m2, m22), biomass_ylab = 'Biomass (t)', cpue_ylab = 'RPW')
p1 <- compare$plots$biomass_by_strata + 
  facet_wrap(~strata, scales = 'free_y', ncol = 1) +
  ggplot2::scale_fill_viridis_d(direction = -1, option = "E") +
  ggplot2::scale_colour_viridis_d(direction = -1, option = "E") +
  labs(title = 'Bottom trawl survey (BTS) biomass (t) by region') +
  theme_bw()

p1

ggsave(paste0(out_path, '/M22_M24_biomass_fits_bw.png'), units = 'in', bg = 'white',
       height = 7.5, width = 6.5, dpi = 300)

p2 <- compare$plots$cpue_by_strata + 
  facet_wrap(~strata, scales = 'free_y', ncol = 1)+
  ggplot2::scale_fill_viridis_d(direction = -1, option = "E") +
  ggplot2::scale_colour_viridis_d(direction = -1, option = "E") +
  labs(title = 'Longline survey (LLS) relative population weights (RPW)') +
  theme_bw()
p2
ggsave(paste0(out_path, '/M22_M24_fit_slope_bw.png'), units = 'in', bg = 'white',
       height = 3.5, width = 7, dpi = 300)

cowplot::plot_grid(p1, p2, ncol = 1, rel_heights = c(0.7, 0.3),align="v")
ggsave(paste0(out_path, '/M22_M24_bts_lls_bw.png'), units = 'in', bg = 'white',
       height = 7.5, width = 7, dpi = 300)

# total biomass and biomass by strata ----
compare$output$total_predicted_biomass %>% 
  write_csv(paste0(out_path, '/total_biomass_M22_M24.csv'))
compare$output$biomass_by_strata %>% 
  write_csv(paste0(out_path, '/biomass_by_strata_M22_M24.csv'))

compare$plots$total_predicted_biomass +
  ggplot2::scale_fill_viridis_d(direction = -1, option = "E") +
  ggplot2::scale_colour_viridis_d(direction = -1, option = "E") +
  expand_limits(y=0)+
  theme_bw()
ggsave(paste0(out_path, '/M22_M24_total_biomass_bw.png'), units = 'in', bg = 'white',
       height = 4, width = 7, dpi = 300)

biom <- output2$total_predicted_biomass %>% 
              select(model_name, year, biomass = pred) 
#biom %>% 
#  pivot_wider(id_cols = year, names_from = model_name, values_from = biomass) %>% 
#  write_csv(paste0(out_path, '/total_biomass_wide.csv'))

sumtable <- biom %>% 
  filter(year == YEAR) %>% 
  mutate(M = 0.03,
         OFL = M * biomass,
         maxABC = 0.75 * M * biomass)

sumtable %>%
  write_csv(paste0(out_path, '/abc_ofl_summary.csv'))

output2$parameter_estimates %>% 
  arrange(model_name) %>%
  mutate(strata=c("AI","EBS_SLOPE","SBS","EBS_SLOPE")) %>% 
  relocate(strata, .before = parameter) %>% 
  write_csv(paste0(out_path, '/parameters_estimates.csv'))

# save data reformatted ----
biomass_dat %>%
  tidyr::expand(year = min(biomass_dat$year):(YEAR),
                strata) %>%
  left_join(biomass_dat %>%
              mutate(value = ifelse(is.na(biomass), NA,
                                    paste0(prettyNum(round(biomass, 0), big.mark = ','), ' (',
                                           format(round(cv, 3), nsmall = 3, trim = TRUE), ')')))) %>%
  pivot_wider(id_cols = c(year), names_from = strata, values_from = value) %>%
  arrange(year) %>%
  select(Year = year, `AI`, SBS, `EBS Slope`) %>% 
  write_csv(paste0(out_path, '/biomass_data_wide.csv'))

cpue_dat %>%
  tidyr::expand(year = min(cpue_dat$year):(YEAR),
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

cpue_dat %>% filter(year %in% c(2021,2023)) %>%
  pivot_wider(id_cols = c(strata), names_from = year, values_from = cpue) %>%
  mutate(percent_change = (`2023`-`2021`)/`2021`)

full_sumtable <- output2$total_predicted_biomass %>%
  rename(total_biomass = pred) %>% 
  filter(year == YEAR) %>%
  mutate(natmat = 0.03,
         OFL = natmat * total_biomass,
         maxABC = 0.75 * natmat * total_biomass,
         ABC = maxABC,
         year = YEAR+1) %>%
  write_csv(paste0(out_path, '/summary_table.csv'))

output2$total_predicted_biomass %>%
  filter(year %in% c(2022, 2024)) %>%
  pivot_wider(id_cols = model_name, names_from = year, values_from = pred) %>%
  mutate(percent_change = (`2024`-`2022`)/`2022` * 100)
