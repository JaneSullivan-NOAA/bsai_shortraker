# Biological data (length comps) for SST and Duskies
# Contact: jane.sullivan@noaa.gov
# Last updated: Oct 2020

# devtools::session_info()
# version  R version 4.0.2 (2020-06-22)
# os       Windows 10 x64              
# system   x86_64, mingw32             
# ui       RStudio 

# Set up ----

# Assessment year (most recent year with complete data set)
YEAR <- 2022
dat_path <- paste0("data/", YEAR) # directory where source data is contained
out_path <- paste0("results/", YEAR) # directory for results/output
dir.create(out_path)

libs <- c("dplyr", "ggridges", "ggpubr", "cowplot")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

# Data ----

lengths <- read_csv(paste0(dat_path, "/lengths_shortraker_", YEAR, ".csv"))

# Get length comps ----
comps <- lengths %>% 
  group_by( year, length) %>% 
  summarize(n = sum(frequency)) %>% 
  mutate(prop = n / sum(n)) %>% 
  group_by(year) %>% 
  # somewhat arbitrary cutoff for sample sizes
  filter(sum(n) > 100) %>% 
  ungroup()
ggplot(comps, aes(x=length, y = prop)) +
  geom_bar(stat = 'identity')
# very few fish > 100 cm and < 15 cm

comps <- lengths %>% 
  filter(between(length, 15, 100)) %>% 
  group_by(source, fmp_subarea, year, length) %>% 
  summarize(n = sum(frequency)) %>% 
  mutate(prop = n / sum(n)) %>% 
  group_by(source, fmp_subarea, year) %>% 
  # somewhat arbitrary cutoff for sample sizes
  filter(sum(n) > 190) %>% 
  ungroup()

# Comp test: should all be 1!
comps %>% group_by(source, fmp_subarea, year) %>% summarize(tst = sum(prop)) %>% pull(tst) 
unique(comps$source)

# Plot shortraker AI ----

ggplot(data = comps %>% 
         filter(year >= 2002 & between(length, 15, 80)) %>%
         filter(source %in% c("AI BTS", "AI fishery", 'AI LLS')) %>% 
         filter(fmp_subarea == 'AI'),
       aes(x = length, y = factor(year), height = prop, fill = source)) +
  geom_density_ridges(stat = "identity", col = "white", alpha = 0.4, #alpha = 0.8
                      panel_scaling = TRUE, size = 0.5) +
  scale_fill_manual(values = c("grey30", "#00BFC4", "#daa520", "#da2055")) +
  labs(x = "Length (cm)", y = NULL, fill = NULL, title = "AI shortraker") +
  theme_light() +
  # facet_wrap(~ fmp_subarea) +
  theme(legend.position = "top",
        strip.background = element_blank())

ggsave(paste0(out_path, "/lencomps_shortraker_AI_", YEAR, ".png"), 
       dpi=300, height=7, width=5, units="in")


# Plot shortraker EBS ----

ggplot(data = comps %>% 
         filter(year >= 2002) %>% #View() 
         # not many SR in the Southern Bering Sea, leave it out
         filter(source %in% c('EBS fishery', 'EBS LLS', 'EBS slope BTS')) %>% 
         mutate(source = factor(source, levels = c('EBS LLS', 'EBS fishery','EBS slope BTS'))),
       aes(x = length, y = factor(year), height = prop, fill = source)) +
  geom_density_ridges(stat = "identity", col = "white", alpha = 0.4, #alpha = 0.8
                      panel_scaling = TRUE, size = 0.5) +
  scale_fill_manual(values = c("grey30", "#00BFC4", "#daa520", "#da2055")) +
  # geom_hline(yintercept = factor(2002:YEAR), col = "lightgrey") +
  labs(x = "Length (cm)", y = NULL, fill = NULL, title = "EBS shortraker") + #) +
  theme_light() +
  # facet_wrap(~ fmp_subarea) +
  theme(legend.position = "top",
        strip.background = element_blank())

ggsave(paste0(out_path, "/lencomps_shortraker_EBS_", YEAR, ".png"), 
       dpi=300, height=7, width=5, units="in")
