# BSAI shortraker queries
# Sebastes borealis
# RACE species code 30576
# NORPAC code 326

# set up ----

# assessment year
YEAR <- 2024

libs <- c('readr', 'dplyr', 'tidyr', 'RODBC', 'ggplot2', 'keyring','akfingapdata')
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

# folder set up
dat_path <- paste0("data/", YEAR); dir.create(dat_path)
raw_path <- paste0(dat_path, "/raw"); dir.create(raw_path)
out_path <- paste0("results/", YEAR); dir.create(out_path)

# database connection ----

# to get to GAP PRODUCTS
# first set up secret string with AKFIN to access webservice for GAP PRODUCTS
library(keyring)
library(akfingapdata)
key_list() #tells you how many keys are set up
token <-create_token("akfin_secret")

# to get to Longline Survey, need to be on VPN for this to work
# enter your username and password for the AKFIN database. Note that these
# credentials are different than what you may use to access AKFIN Answer.
# Contact AKFIN for more information.
username_akfin = 'kshotwell'
password_akfin = 'rapt4mol'
channel_akfin <- odbcConnect("akfin", uid = username_akfin, pwd = password_akfin, believeNRows=FALSE)

# to find survey and area IDs, download survey, area, and stratum group tables
# NOTE can skip this step if all Definitions below
survey<-get_gap_survey_design()
area<-get_gap_area()
stratum<-get_gap_stratum_groups()

# combine spatial lookup tables 
stratum<-stratum %>%
  left_join(survey %>%
              # remove year specific information from survey, we only need the region codes
              group_by(survey_definition_id) %>%
              summarize(survey_definition_id=max(survey_definition_id)),
            by="survey_definition_id") %>%
  left_join(area,  by=c("area_id"="area_id", "survey_definition_id"="survey_definition_id", "design_year"="design_year"))

# look for a specific area by filtering by survey and search for word such as "western", or just look through stratum table 
region<-stratum %>%
  filter(survey_definition_id==52,
         grepl("western", tolower(area_name))
  )

# Definitions: 
# stock = shortraker rockfish = 30576
# Aleutian Islands = survey definition = 52, area_id = 99904
# Western Aleutian Islands = survey definition = 52, area_id = 299
# Central Aleutian Islands = survey definition = 52, area_id = 3499
# Eastern Aleutian Islands = survey definition = 52, area_id = 5699
# Southern Bering Sea = survey definition = 52, area_id = 799
# Eastern Bering Sea = survey definition = 98, area_id = 99900
# Eastern Bering Sea Slope = survey definition = 78, area_id = 99905

# globals for database queries
species_code=30576
start_year=1990
end_year=3000

# AI Survey biomass ----

wai <- get_gap_biomass(survey_definition_id = 52,
                               area_id = 299,
                               species_code = species_code,
                               start_year = start_year,
                               end_year = end_year) %>% 
  write_csv(paste0(raw_path, "/wai_biomass_raw_", YEAR, ".csv"))

cai <- get_gap_biomass(survey_definition_id = 52,
                       area_id = 3499,
                       species_code = species_code,
                       start_year = start_year,
                       end_year = end_year) %>% 
  write_csv(paste0(raw_path, "/cai_biomass_raw_", YEAR, ".csv"))

eai <- get_gap_biomass(survey_definition_id = 52,
                       area_id = 5699,
                       species_code = species_code,
                       start_year = start_year,
                       end_year = end_year) %>% 
  write_csv(paste0(raw_path, "/eai_biomass_raw_", YEAR, ".csv"))

sbs <- get_gap_biomass(survey_definition_id = 52,
                       area_id = 799,
                       species_code = species_code,
                       start_year = start_year,
                       end_year = end_year) %>% 
  write_csv(paste0(raw_path, "/sbs_biomass_raw_", YEAR, ".csv"))

ai <- rbind(wai,cai,eai,sbs) 
ai <- ai %>% 
  #left_join(areas) %>% 
  arrange(species_code, year) %>% 
  mutate(area = case_when(area_id == 799 ~ "SBS",
                          area_id == 5699 ~ "AI",
                          area_id == 3499 ~ "AI",
                          area_id == 299 ~ "AI"))

ai <- ai %>% 
  group_by(year, area) %>% 
  summarize(biomass = sum(biomass_mt),
            var = sum(biomass_var)) 

# EBS slope biomass----

ebs_slope <- get_gap_biomass(survey_definition_id = 78,
                       area_id = 99905,
                       species_code = species_code,
                       start_year = start_year,
                       end_year = end_year) %>% 
  write_csv(paste0(raw_path, "/ebs_slope_biomass_raw_", YEAR, ".csv"))

ebs_slope <- ebs_slope %>% 
  arrange(species_code, year) %>% 
  mutate(area = "EBS Slope") 

ebs_slope <- ebs_slope %>% 
  group_by(year, area) %>% 
  summarize(biomass = sum(biomass_mt),
            var = sum(biomass_var))

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

# check plots ----

ggplot() +
  geom_line(data = biomass_dat, aes(x = year, y = biomass)) + 
  geom_point(data = biomass_dat, aes(x = year, y = biomass)) + 
  geom_line(data = cpue_dat, aes(x = year, y = cpue), col = 'green') + 
  geom_point(data = cpue_dat, aes(x = year, y = cpue), col = 'green') + 
  facet_wrap(~strata) +
theme_bw()
# Fishery lengths lookup ----

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
ai_len <- get_gap_sizecomp(survey_definition_id = 52,
                                 area_id = 99904,
                                 species_code = species_code,
                                 start_year = start_year,
                                 end_year = end_year) %>%
  filter(length_mm>=0)%>%
  mutate(sex=factor(case_match(sex, 1~"male", 2~"female", 3~"unsexed"),
                    levels=c("male", "female", "unsexed")))%>% 
  write_csv(paste0(raw_path, "/ai_survey_lengths_shortraker_raw_", YEAR, ".csv"))

ai_len <- ai_len %>% 
  group_by(length_mm, year) %>% 
  summarise(frequency = sum(population_count, na.rm = TRUE)) %>% 
  mutate(length = length_mm/10, species_code = 30576, fmp_subarea = 'AI', source = 'AI BTS') %>%
  ungroup()

# EBS slope, make sure to convert to cm, ungroup to get rid of other
ebs_slope_len <- get_gap_sizecomp(survey_definition_id = 78,
                           area_id = 99905,
                           species_code = species_code,
                           start_year = start_year,
                           end_year = end_year) %>%
  filter(length_mm>=0)%>%
  mutate(sex=factor(case_match(sex, 1~"male", 2~"female", 3~"unsexed"),
                    levels=c("male", "female", "unsexed")))%>% 
  write_csv(paste0(raw_path, "/ebs_slope_survey_lengths_shortraker_raw_", YEAR, ".csv"))

ebs_slope_len <- ebs_slope_len %>% 
  group_by(length_mm, year) %>% 
  summarise(frequency = sum(population_count, na.rm = TRUE)) %>% 
  mutate(length = length_mm/10, species_code = 30576, fmp_subarea = 'EBS', source = 'EBS slope BTS') %>% 
  ungroup()

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
              select(species_code, year, length, frequency, fmp_subarea, source)) %>% 
  bind_rows(ebs_slope_len %>% 
              select(species_code, year, length, frequency, fmp_subarea, source)) %>% 
  bind_rows(lls_len_sum %>% 
              ungroup() %>% 
              mutate(species_code = 30576,
                     fmp_subarea = ifelse(council_sablefish_management_area == 'Aleutians', 'AI', 'EBS'),
                     source = paste0(fmp_subarea, ' LLS')) %>% 
              select(species_code, year, length, frequency = rpw, fmp_subarea, source)) %>% 
  write_csv(paste0(dat_path, "/lengths_shortraker_", YEAR, ".csv"))
