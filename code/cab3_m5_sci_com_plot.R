#################################################
# Title: LT Monitoring Sci Com Final Draft Fig
# Purpose: stacked bar fig for m5 at cabr3
# Author: LP
# Created: 3/23/22
# Last edited: 3/23/22
##################################################

# preset folder to save things in
save_folder <- './figs/sci_com_figs_feb22/'
# preset folder to look for data in
data_folder <- './data/MEDN_RI_DB_JAN21/'

##### packages #####
library(readxl)
library(janitor)
library(broom)
library(viridis)
library(lubridate)
library(PNWColors)
library(tidyverse)

##### load data #####

# target data
target <- read_excel(paste0(data_folder, 'Photoplot_summary_by_plot.xlsx'))

# alignment info for target data 
align <- read_csv('data/crosswalk/TGT_all.csv')
# see RI_Species_Alighment_Feb21.R for methods
# summary: took species lists for 1990 and 2000, applied to all unique spp.

##### tidy data #####

# keep target raw, retains plot-level data, and tidy
target <- 
  # add generic codes to this set, along with formatted scientific names
  left_join(target, align, by = 'SpeciesCode') %>%
  # get rid of old scientific name (align has better one)
  select(-c(Scientific_name.x)) %>%
  # rename scientific name column
  rename(Scientific_name = Scientific_name.y) %>%
  # make plot number a numeric column
  mutate(Plot_num = parse_number(substr(Plot_code, 6,6))) %>%
  # only get unique rows (removes duplicates)
  distinct() 

# summarize by 2 methods: all and 1990 (1990-present)

# make list of all possible spp names and codes from target dataset
spplist <- align %>%
  select(SpeciesCode, Scientific_name) %>%
  distinct()

# all categories
target_all <- target %>%
  group_by(SiteCode, SiteName, SurveyYear, Zone, Plot_num, Plot_code, SpeciesCode, Scientific_name) %>%
  summarise(cover = sum(N))


# get total # of pts surveyed per plot in each site/spp/year/season (range = 89-104)
target_summary <- target_all %>%
  group_by(SiteCode, SiteName, SurveyYear, Zone, Plot_num, Plot_code) %>% 
  summarise(points = sum(cover))

# merge w target dataset (to calculate % cover)
target_all <- 
  # align # pts per plot with target 
  left_join(target_all, target_summary) %>%
  # calculate % cover (range of # pts is 89 - 104)
  group_by(SiteCode, SiteName, SurveyYear, Zone, Plot_num, Plot_code, SpeciesCode, Scientific_name) %>%
  mutate(pct_cover = (cover/points)*100) %>%
  # streamline dataset
  select(SiteCode, SiteName, SurveyYear, Zone, Plot_num, Plot_code, SpeciesCode, Scientific_name, pct_cover) 

# get all spp classes how they were in 1990

# all categories
target1990 <- target %>%
  group_by(SiteCode, SiteName, SurveyYear, Zone, Plot_num, Plot_code, Code_1990) %>%
  summarise(cover = sum(N))

target1990 <- left_join(target1990, target_summary) %>%
  group_by(SiteCode, SiteName, SurveyYear, Zone, Plot_num, Plot_code, Code_1990) %>%
  mutate(pct_cover = (cover/points)*100) %>%
  select(SiteCode, SiteName, SurveyYear, Zone, Plot_num, Plot_code, Code_1990, pct_cover) %>%
  rename(SpeciesCode = Code_1990)

# join spp names to 1990 data
target1990 <- left_join(target1990, spplist)

# 2018 data weren't exported b/c they aren't verified, so fill in with mean of 2017 and 2019
seventeen <- ungroup(target1990) %>%
  filter(SiteCode == 'CAB3' & Zone == 'MYT' & Plot_num == 5 & SurveyYear %in% c(2017, 2019)) %>%
  pivot_wider(names_from = 'SurveyYear', values_from = 'pct_cover') %>%
  mutate(pct_cover = (`2017`+`2019`)/2,
         SurveyYear = 2018) %>%
  select(-`2017`, -`2019`)

# add 2017 data to target1990
target_1990 <- full_join(target1990,seventeen)

##### wrangle data for cabr3 myt5 ####

  myt5 <- ungroup(target_1990) %>%
    # filter for mussel plot 5 in zone 3
    filter(SiteCode == 'CAB3' & Zone == 'MYT' & Plot_num == 5) %>%
    # select cols of interest
    select(SurveyYear, pct_cover, Scientific_name) %>%
    # make pretty label columns, nicer spp names
    mutate(Scientific_name = case_when(Scientific_name == 'Mussel' ~ 'Mussels',
                                       Scientific_name =='Bare substrate' ~ 'Rock',
                                       Scientific_name =='Misc animal' ~ 'Invertebrates',
                                       Scientific_name =='Other plants' ~ 'Algae',
                                       Scientific_name =='Tetraclita rubescens' ~ 'Thatched barnacles',
                                       Scientific_name =='Pollicipes polymerus' ~ 'Goose barnacles',
                                       Scientific_name =='Silvetia compressa' ~ 'Rockweed algae',
                                       TRUE ~ 'Unspecified'))

##### stacked bar plot #####

  ggplot(data = myt5,
         mapping = aes(x = SurveyYear, y = pct_cover, 
                       # make target spp last (bottom bar on plots)
                       fill = fct_relevel(Scientific_name, 'Mussels', after = Inf))) +
    # bar stacked by spp group (Scientific_name)
    geom_bar(stat = 'identity', width = 1) + 
    # axis and plot labels
    xlab('Year') + 
    ylab('Percent cover') + 
    scale_x_continuous(expand = c(0,0)) + 
    scale_y_continuous(expand = c(0,0)) + 
    ggtitle('Cabrillo National Monument Plot Composition') +
    # colors
    scale_fill_manual(name = 'Cover types',
                      values = c(pnw_palette('Sailboat', 7, type = 'continuous'), 'gray20')) +
    theme_bw() + 
    theme(panel.grid = element_blank(),
          text = element_text(size = 12),
          axis.text = element_text(size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(paste0(save_folder, 'bar_stacked_cabr3_m5.png'))

