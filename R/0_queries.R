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
username_akfin = 'my_username'
password_akfin = 'my_password'
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