# BSAI shortraker biomass estimation using a single survey index (BTS) and two
# survey indices (BTS + LLS)

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

# read data ----

# bottom trawl survey
biomass_dat <- read_csv(paste0(dat_path, "/bsai_shortraker_biomass_", YEAR, ".csv")) 
cpue_dat <- read_csv(paste0(dat_path, "/bsai_shortraker_rpw_", YEAR, ".csv")) 

# BSAI shortraker biomass estimation

# Mod 1: BTS only ----

# (1) Prepare REMA model inputs
# ?prepare_rema_input # note alternative methods for bringing in survey data observations

# status quo model
input <- prepare_rema_input(model_name = 'M1: BTS only',
                            biomass_dat = biomass_dat,
                            zeros = list(assumption = 'NA'),
                            end_year = YEAR)

# (2) Fit REMA model
# ?fit_rema
m <- fit_rema(input)

# (3) Get tidied data.frames from the REMA model output
# ?tidy_rema
output <- tidy_rema(rema_model = m)
names(output)
output$parameter_estimates # estimated fixed effects parameters
output$biomass_by_strata # data.frame of predicted and observed biomass by stratum
output$total_predicted_biomass # total predicted biomass (same as biomass_by_strata for univariate models)

# (4) Generate model plots
# ?plot_rema
plots <- plot_rema(tidy_rema = output,
                   # optional y-axis label
                   biomass_ylab = 'Biomass (t)')
plots$biomass_by_strata

# BTS + LLS ----

input <- prepare_rema_input(model_name = 'M2: BTS + LLS strata PE',
                            multi_survey = 1, # fit to CPUE data? yes = 1)
                            biomass_dat = biomass_dat,
                            cpue_dat = cpue_dat,
                            sum_cpue_index = 1, # is the CPUE index summable (yes = 1, RPWs are summable)
                            zeros = list(assumption = 'NA'),
                            end_year = YEAR,
                            # sort(unique(biomass_dat$strata)) = "AI" "EBS
                            # Slope" "SBS". EBS Slope is stratum 2 for the
                            # biomass so we put it in the second position of the
                            # pointer_biomass_cpue_strata object and use NAs for
                            # the other 2 strata. It is the first (and only)
                            # stratum for the LLS, so we use the value of 1. See
                            # Details in ?prepare_rema_input
                            q_options = list(pointer_biomass_cpue_strata = c(NA, 1, NA)))

m2 <- fit_rema(input)

output <- tidy_rema(rema_model = m2)
output$parameter_estimates # estimated fixed effects parameters
output$biomass_by_strata # data.frame of predicted and observed biomass by stratum
output$cpue_by_strata # data.frame of predicted and observed RPW by stratum
output$total_predicted_biomass # total predicted biomass (same as biomass_by_strata for univariate models)

plots <- plot_rema(tidy_rema = output,
                   # optional y-axis label
                   biomass_ylab = 'Biomass (t)',
                   cpue_ylab = 'Relative population weights')
plots$biomass_by_strata
plots$cpue_by_strata

# alternative models ----

# share PE
input <- prepare_rema_input(model_name = 'M3: BTS + LLS share PE',
                            multi_survey = 1, 
                            biomass_dat = biomass_dat,
                            cpue_dat = cpue_dat,
                            sum_cpue_index = 1, 
                            zeros = list(assumption = 'NA'),
                            end_year = YEAR,
                            PE_options = list(pointer_PE_biomass = c(1, 1, 1)),
                            # also tried by survey/FMP, strata-specific PEs were
                            # all best by AIC
                            # PE_options = list(pointer_PE_biomass = c(1, 2, 1)),
                            # PE_options = list(pointer_PE_biomass = c(1, 2, 2)),
                            q_options = list(pointer_biomass_cpue_strata = c(NA, 1, NA)))

m3 <- fit_rema(input)
tidy_rema(m3)$parameter_estimates

compare_rema_models(list(m2, m3))$aic

# extra biomass variance ----

input <- prepare_rema_input(model_name = 'M4: BTS + LLS share PE, xtra biom var',
                            multi_survey = 1, 
                            biomass_dat = biomass_dat,
                            cpue_dat = cpue_dat,
                            sum_cpue_index = 1, 
                            zeros = list(assumption = 'NA'),
                            end_year = YEAR,
                            # wouldn't converge with strata-specific PEs
                            PE_options = list(pointer_PE_biomass = c(1, 1, 1)),
                            q_options = list(pointer_biomass_cpue_strata = c(NA, 1, NA)),
                            extra_biomass_cv = list(assumption = 'extra_cv'))

m4 <- fit_rema(input)

# extra cpue variance ----

input <- prepare_rema_input(model_name = 'M5: BTS + LLS xtra cpue var, strata PE',
                            multi_survey = 1, 
                            biomass_dat = biomass_dat,
                            cpue_dat = cpue_dat,
                            sum_cpue_index = 1, 
                            zeros = list(assumption = 'NA'),
                            end_year = YEAR,
                            q_options = list(pointer_biomass_cpue_strata = c(NA, 1, NA)),
                            extra_cpue_cv = list(assumption = 'extra_cv'))

m5 <- fit_rema(input)

input <- prepare_rema_input(model_name = 'M6: BTS + LLS xtra cpue var, share PE',
                            multi_survey = 1, 
                            biomass_dat = biomass_dat,
                            cpue_dat = cpue_dat,
                            sum_cpue_index = 1, 
                            zeros = list(assumption = 'NA'),
                            end_year = YEAR,
                            PE_options = list(pointer_PE_biomass = c(1, 1, 1)),
                            q_options = list(pointer_biomass_cpue_strata = c(NA, 1, NA)),
                            extra_cpue_cv = list(assumption = 'extra_cv'))

m6 <- fit_rema(input)

# extra biomass + cpue variance ----

input <- prepare_rema_input(model_name = 'M7: BTS + LLS xtra biom & cpue var, share PE',
                            multi_survey = 1, 
                            biomass_dat = biomass_dat,
                            cpue_dat = cpue_dat,
                            sum_cpue_index = 1, 
                            zeros = list(assumption = 'NA'),
                            end_year = YEAR,
                            # did not converge with strata PE
                            PE_options = list(pointer_PE_biomass = c(1, 1, 1)),
                            q_options = list(pointer_biomass_cpue_strata = c( NA, 1, NA)),
                            extra_biomass_cv = list(assumption = 'extra_cv'),
                            extra_cpue_cv = list(assumption = 'extra_cv'))

m7 <- fit_rema(input)

compare <- compare_rema_models(list(m2, m3, m4, m5, m6, m7))
compare$aic
compare$plots$biomass_by_strata

compare <- compare_rema_models(list(m2, m5, m4))
compare$plots$biomass_by_strata + facet_wrap(~strata, ncol = 1, scales = 'free_y')
compare$plots$cpue_by_strata + facet_wrap(~strata, ncol = 1, scales = 'free_y')
compare$plots$total_predicted_biomass

out5 <- tidy_rema(m5)
out5$parameter_estimates

# Model comparisons -----

compare <- compare_rema_models(list(m,m2, m3))
compare$plots$biomass_by_strata
compare$plots$total_predicted_biomass
compare$output$parameter_estimates
