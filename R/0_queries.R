# BSAI shortraker queries
# Sebastes borealis
# RACE species code 30576

# set up ----

# assessment year
YEAR <- 2022

libs <- c('readr', 'dplyr', 'tidyr', 'RODBC', 'ggplot2')
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

# folder set up
dat_path <- paste0("data/", YEAR); dir.create(dat_path)
raw_path <- paste0(dat_path, "/raw"); dir.create(raw_path)
out_path <- paste0("results/", YEAR); dir.create(out_path)

# database connection ----

# Enter your username and password for the AKFIN database. Note that these
# credentials are different than what you may use to access AKFIN Answer.
# Contact AKFIN for more information.
username_akfin = 'kshotwell'
password_akfin = 'rapt4mol'
channel_akfin <- odbcConnect("akfin", uid = username_akfin, pwd = password_akfin, believeNRows=FALSE)

# INPFC_AREA look up ----

# International North Pacific Fisheries Commission (helps pull out S. Bering Sea
# area from the AI BTS)

query <- "select distinct   survey, inpfc_area as area, summary_area as area_code
          from              afsc.race_goastrataaigoa
          where             survey = 'AI'"

areas <- sqlQuery(channel_akfin, query) %>% rename_all(tolower) 

# survey  area              area_code
# AI   Western Aleutians    299
# AI   Central Aleutians    3499
# AI   Eastern Aleutians    5699
# AI   Southern Bering Sea  799

# AI Survey biomass ----

query <- "select   survey, year, summary_area as area_code, species_code, 
                   area_biomass as biomass, biomass_var as var
          from     afsc.race_biomassinpfcaigoa
          where    species_code in ('30576') and
                   survey = 'AI'"

ai <- sqlQuery(channel_akfin, query) %>% 
  write_csv(paste0(raw_path, "/ai_biomass_raw_", YEAR, ".csv"))

ai <- ai %>% 
  rename_all(tolower) %>% 
  left_join(areas) %>% 
  arrange(species_code, year) %>% 
  mutate(area = case_when(area == "Southern Bering Sea" ~ "SBS",
                          area == "Eastern Aleutians" ~ "AI",
                          area == "Western Aleutians" ~ "AI",
                          area == "Central Aleutians" ~ "AI"))
ai <- ai %>% 
  group_by(year, area) %>% 
  summarize(biomass = sum(biomass),
            var = sum(var))

# EBS shelf ----

# query <- "select   survey, year, species_code, 
#                    biomass, varbio as var
#           from     afsc.race_biomass_ebsshelf_standard
#           where    species_code in ('30576') and 
#                    stratum in ('999')"
# 
# ebs_shelf <- sqlQuery(channel_akfin, query) %>% 
#   write_csv(paste0(raw_path, "/ebs_shelf_biomass_raw_", YEAR, ".csv"))
# 
# ebs_shelf <- ebs_shelf %>% 
#   rename_all(tolower) %>% 
#   arrange(species_code, year) %>% 
#   mutate(area = "EBS Shelf")

# EBS slope ----

query <- "select   year, species_code, 
                   stratum_biomass as biomass, bio_var as var
          from     afsc.race_biomass_ebsslope
          where    species_code in ('30576') and 
                   stratum in ('999999')"

ebs_slope <- sqlQuery(channel_akfin, query) %>% 
  write_csv(paste0(raw_path, "/ebs_slope_biomass_raw_", YEAR, ".csv"))


ebs_slope <- ebs_slope %>% 
  rename_all(tolower) %>% 
  arrange(species_code, year) %>% 
  mutate(survey = "EBS_SLOPE",
         area = "EBS Slope")

# Write biomass data ----

# Bind rows
biomass_dat <- ai %>% 
  bind_rows(ebs_slope) %>% 
  mutate(cv = sqrt(var) / biomass) %>% 
  select(strata = area, year, biomass, cv) %>% 
  write_csv(paste0(dat_path, "/bsai_shortraker_biomass_", YEAR, ".csv")) 

# LLS Relative Pop Wts ----

query <- "select    *
         from      afsc.lls_area_rpn_all_strata
         where     species_code = '30576' and fmp_management_area = 'BSAI'
         order by  year asc"

lls <- sqlQuery(channel_akfin, query) %>% 
  rename_all(tolower) %>% 
  write_csv(paste0(raw_path, "/bsai_lls_raw_", YEAR, ".csv"))

# write cpue dat ----

cpue_dat <- lls %>% 
  filter(country == 'United States' & council_management_area == 'Bering Sea') %>% 
  mutate(strata = ifelse(grepl('Bering', geographic_area_name), 'EBS Slope', 'Define me!')) %>% 
  group_by(strata, year) %>% 
  dplyr::summarise(cpue = sum(rpw, na.rm = TRUE),
                   cv = sqrt(sum(rpw_var, na.rm = TRUE)) / cpue) %>% 
  write_csv(paste0(dat_path, "/bsai_shortraker_rpw_", YEAR, ".csv")) 

ggplot() +
  geom_line(data = biomass_dat, aes(x = year, y = biomass)) + 
  geom_point(data = biomass_dat, aes(x = year, y = biomass)) + 
  geom_line(data = cpue_dat, aes(x = year, y = cpue), col = 'green') + 
  geom_point(data = cpue_dat, aes(x = year, y = cpue), col = 'green') + 
  facet_wrap(~strata) 

# Fishery lengths ----

# Look up table for NMFS areas in the BSAI FMP

query <- "select  distinct  fmp_area as fmp, fmp_subarea, 
                            reporting_area_code as nmfs_area
          from              council.comprehensive_blend_ca
          where             fmp_area = 'BSAI'"

bsai <- sqlQuery(channel_akfin, query) %>% rename_all(tolower)
bsai <- bsai %>% filter(!is.na(nmfs_area)) %>% write_csv("data/bsai_nmfs_area_lookup.csv")
bsai_lkup <- toString(sprintf("'%s'", pull(bsai, nmfs_area)))

# Fishery lengths ----

query <- paste0("select   year, nmfs_area, gear, species as species_code, 
                          length, sex, frequency, sample_system
                 from     norpac.debriefed_length
                 where    species in ('326') and
                          nmfs_area in (%s) and
                          year between 2002 and ", YEAR-1) 

fsh_len <- sqlQuery(channel_akfin, sprintf(query, bsai_lkup)) %>% 
  write_csv(paste0(raw_path, "/bsai_fishery_lengths_shortraker_raw_", YEAR-1, ".csv"))

fsh_len <- fsh_len %>% 
  rename_all(tolower) %>% 
  mutate(species_code = 30576) %>% 
  left_join(bsai) %>% 
  mutate(fmp_subarea = ifelse(fmp_subarea == "BS", "EBS", fmp_subarea)) %>% 
  arrange(species_code, year, length) 

# Survey lengths ----

# AI
query <- "select   survey, year, stratum, length / 10 as length, species_code, 
                   total as frequency
          from     afsc.race_sizestratumaigoa
          where    species_code in ('30576') and
                   survey = 'AI'"

ai_len <- sqlQuery(channel_akfin, query) %>% 
  write_csv(paste0(raw_path, "/ai_survey_lengths_shortraker_raw_", YEAR, ".csv"))

ai_strata <- sqlQuery(channel_akfin,
                      "select distinct   survey, regulatory_area_name, inpfc_area as area,
                            stratum, min_depth, max_depth, area
          from              afsc.race_goastrataaigoa
          where             survey in ('AI')
          order by          regulatory_area_name asc, min_depth asc") %>% 
  rename_all(tolower) %>% 
  distinct(area, stratum, min_depth, max_depth, fmp = regulatory_area_name) %>% 
  mutate(description = paste0(area, ' (', min_depth, '-', max_depth, ' m)')) %>% 
  mutate(description = paste0(min_depth, '-', max_depth, ' m')) %>% 
  distinct(area, stratum, description)

ai_strata

ai_len <- ai_len %>% 
  rename_all(tolower) %>% 
  left_join(ai_strata) %>% 
  filter(area != 'Chirikof') %>% 
  mutate(area = ifelse(grepl('Aleutians', area), 'AI', 'SBS')) %>% 
  arrange(species_code, area, year, length) 

# EBS slope
query <- "select   survey, year, stratum, length / 10 as length, species_code, 
                   total as frequency
          from     afsc.race_sizecomp_ebsslope
          where    species_code in ('30576') and length < 999"

ebs_slope_len <- sqlQuery(channel_akfin, query) %>% 
  write_csv(paste0(raw_path, "/ebs_slope_survey_lengths_shortraker_raw_", YEAR, ".csv"))

ebs_slope_len <- ebs_slope_len %>% 
  rename_all(tolower) %>% 
  arrange(species_code, year, length)

# LLS lengths ----

lls_len <- sqlQuery(channel_akfin, query = ("
                select    *
                from      afsc.lls_length_rpn_by_area_all_strata
                where     species_code = '30576' and length < 999 
                order by  year asc
                ")) %>% 
  rename_all(tolower) 

lls_len_sum <- lls_len %>% 
  filter(grepl(c('Bering|Aleutians'), geographic_area_name)) %>% 
  group_by(year, council_sablefish_management_area, length) %>% 
  dplyr::summarise(rpw = sum(rpw, na.rm = TRUE)) %>% 
  write_csv(paste0(dat_path, '/lls_length_shortraker.csv'))

# Write lengths ----

comps <- fsh_len %>% 
  select(species_code, year, length, frequency, fmp_subarea) %>% 
  mutate(source = paste0(fmp_subarea, " fishery")) %>% 
  bind_rows(ai_len %>% 
              mutate(fmp_subarea = area,
                     source = "AI BTS") %>% 
              select(species_code, year, fmp_subarea, length, frequency, source)) %>% 
  bind_rows(ebs_slope_len %>% 
              select(species_code, year, length, frequency) %>% 
              mutate(fmp_subarea = "EBS",
                     source = "EBS slope BTS")) %>% 
  bind_rows(lls_len_sum %>% 
              ungroup() %>% 
              mutate(species_code = 30576,
                     fmp_subarea = ifelse(council_sablefish_management_area == 'Aleutians', 'AI', 'EBS'),
                     source = paste0(fmp_subarea, ' LLS')) %>% 
              select(species_code, year, fmp_subarea, length, frequency = rpw, source)) %>% 
  write_csv(paste0(dat_path, "/lengths_shortraker_", YEAR, ".csv"))
