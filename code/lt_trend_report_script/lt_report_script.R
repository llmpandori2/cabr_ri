#################################################
# Title: Report Figures & Stats - 1990-2021 Trend Report
# Purpose: adapted from LT report code + clean up code
# Author: LP
# Created: 4/20/22
# Last edited: 4/28/22
##################################################

# save place
saveplace <- './figs/lt_trend_report_figs/tables/'

##### packages #####
library(readxl)
library(janitor)
library(viridis)
library(tidyverse)

##### load data #####
# set wd to folder with data for year of interest
setwd("D:/LP_Files/RStudio_Files/cabr_ri/data/MEDN_RI_DB_JAN21")

# owl limpet data
lim_density <- read_excel('Limpet_density_by_plot_size.xlsx')
lim_measure <- read_excel('Limpet_measurements.xlsx')

# transect data (doesn't need alignment - spp consistent throughout)
transect <- read_excel('Line_transect_summary.xlsx')

# target data (needs alignment - spp lists differ by year)
target <- read_excel('Photoplot_summary_by_plot.xlsx')

# target scoring type (from MARINe db exports)
target_scoretype <- read_csv("D:/LP_Files/RStudio_Files/cabr_ri/data/MARINe_db_CABR_RockyIntertidal_FA21_csv_20220414/CABR_RockyIntertidal_FA21_csv_20220405/qryEMLPhotoPlots.csv")

# timed search
timed_search <- read_excel("TimedSearch_plot_counts.xlsx")

setwd("D:/LP_Files/RStudio_Files/cabr_ri")

# alignment info for target data 
taxa_crosswalk <- read_excel("data/crosswalk/crosswalk_lt_trend_report_photoplot.xlsx")
# see RI_Species_Alighment_Feb21.R for methods
# summary: took species lists for 1990 and 2000, applied to all unique spp.

##### presets #####
# lists of commonly filtered-for items
cabrsites <- c('CAB1', 'CAB2', 'CAB3') 
zonelist <- c('CHT', unique(target$Zone)[1:4])
zonenames <- c('Balanus/Chthamalus', 'Tetraclita rubescens', 'Mussel', 'Silvetia compressa', 'Pollicipes polymerus')
targetlist <- c('CHTBAL', 'TETRUB', 'MUSSEL', 'SILCOM', 'POLPOL')
endyear <- 2021

##### tidy: transect ######

# tidy 
transect <- transect %>%
  # snake case column names
  clean_names() %>%
  # make "transect" not a column
  rename(transect_number = transect) %>%
  # limit to sites at CABR, 1990-2020, and fall seasons
  filter(site_code %in% cabrsites & survey_year < 2022 & season_name == 'Fall') %>%
  # remove values where transect_number is NA (rows checked - don't contain data)
  drop_na(transect_number) %>%
  # remove duplicated rows (there are none, but this is here for qc)
  distinct() %>%
  mutate(
    # combine PHYOVE (phyllospadix overstory) and PHYUND (understory) to PHYALL
    species_code = if_else(species_code %in% c('PHYOVE', 'PHYUND'), 'PHYALL', species_code),
    # correct scientific names
    scientific_name = if_else(scientific_name == 'Phyllospadix torreyi (understory)',
                              'Phyllospadix spp', scientific_name),
    # make column name for transect target spp
    type = case_when(transect_number < 3 ~ 'Red algae',
                     transect_number > 4 ~ 'Egregia',
                     transect_number %in% c(3,4) ~ 'Phyllospadix')) %>%
  # capitalize first letter of strings
  mutate(scientific_name = str_to_sentence(scientific_name)) %>%
  # calculate new totals/% cover with new category
  group_by(site_code, site_name, type, survey_year, season, season_name, survey_date, transect_number, species_code, scientific_name, total_points) %>%
  summarise(points = sum(n)) %>%
  # calculate new % cover
  mutate(pct_cover = (points/total_points)*100) %>%
  # tidy panels/sites
  mutate(zone_name = paste('(', case_when(site_name == 'Cabrillo I'~ 'A',
                                          site_name == 'Cabrillo II' ~ 'B',
                                          TRUE ~ 'C'), ')', 
                           ' Zone', substr(site_name, 9, 12), sep = '')) %>%
  ungroup()

# table w/ sum of percent cover for each year + transect combo
  # fill in with an empty 2018 row so that year is present
  rbind(transect, mutate(transect[1,], survey_year = 2018, pct_cover = NA)) %>%
  group_by(site_code, survey_year, type, transect_number) %>%
  summarize(cover_sum = sum(pct_cover)) %>%
  distinct() %>%
  pivot_wider(., names_from = survey_year, values_from = cover_sum) %>%
  arrange(site_code, transect_number) %>%
  write_csv(., paste0(saveplace,'transect_coverage_table.csv'))

# table with species presence/absence for each year
  rbind(transect, mutate(transect[1,], survey_year = 2018, pct_cover = NA)) %>%
  group_by(survey_year, species_code, scientific_name) %>%
  summarize(pct_cover_na = if_else(!is.na(pct_cover), 'X', '')) %>%
  distinct() %>%
  pivot_wider(., names_from = survey_year, values_from = pct_cover_na) %>%
  arrange(scientific_name) %>%
  write_csv(., paste0(saveplace, 'transect_spp_table.csv'))

# data notes: 
  # prior to 2003, only target spp counted
  # 2003-2020, spp list relatively consistent - use for community
  # 2018 data missing (not verified?)

##### tidy: photoplot ####
  
# get scoring method from target_scoretype (add to % cover tables later - unfinished)
target_scoretype2 <- target_scoretype %>%
    # snake case column names
    clean_names() %>%
    # collapse scoring types
    mutate(method_code = case_when())
    # select relevant columns
    group_by(site_id, sample_season_code) %>%
    # if multiple methods, separate w/ comma
    summarize(method = paste(unique(method_code), collapse = ','))
  
target <- target %>%
  # snake case column names
  clean_names() %>%
  # limit to sites at CABR, 1990-2020
  filter(site_code %in% cabrsites & survey_year < 2021 
        # limit to sites surveyed in fall
        & season_name == 'Fall' & was_surveyed == 'surveyed'
        # remove 1 errant spp name
        & species_code != 'ALISPP') %>%
  rename(pct_cover = cover_pct, type = zone) %>%
  # make species names in sentence case (1st letter of 1st word capitalized)
  mutate(scientific_name = str_to_sentence(scientific_name))
  
# table w/ sum of percent cover for each year + transect combo
  # fill in with an empty 2018 row so that year is present
  rbind(target, mutate(target[1,], survey_year = 2018, pct_cover = NA)) %>%
    group_by(site_code, plot_code, survey_year) %>%
    summarize(cover_sum = sum(pct_cover)) %>%
    distinct() %>%
    pivot_wider(., names_from = survey_year, values_from = cover_sum) %>%
    write_csv(., paste0(saveplace,'target_coverage_table.csv'))
  
# table with species presence/absence for each year
  rbind(target, mutate(target[1,], survey_year = 2018, pct_cover = NA)) %>%
    group_by(survey_year, species_code, scientific_name) %>%
    summarize(pct_cover_na = if_else(!is.na(pct_cover), 'X', '')) %>%
    distinct() %>%
    pivot_wider(., names_from = survey_year, values_from = pct_cover_na) %>%
    arrange(scientific_name) %>%
    write_csv(., paste0(saveplace, 'target_spp_table.csv'))
  
# step 2 of tidy - after looking at above tables
target <- target %>%
    # limit to mussel plots <= 5 and pollicipes plots <= 6 
    filter(!plot_code %in% c('MYT-06', 'POL-07', 'POL-08')) %>%
    # align with taxa codes for 1990 and 2000s
    left_join(., taxa_crosswalk)
  
  
# notes: 
  # Po plots not established until 1995
  # Po plots > 6 removed, other plots > 5 removed from analyses (added later)
  

  
  
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
  # add zone/site column
  mutate(ZoneName = paste('Zone', substr(SiteName, 9, 12)),
         Panel = if_else(SiteName == 'Cabrillo I', 'A', 
                         if_else(SiteName == 'Cabrillo II', 'B', 'C'))) %>%
  mutate(ZoneName = paste('(', Panel, ') ', ZoneName, sep = '')) %>%
  # only get unique rows (removes duplicates)
  distinct() %>%
  # make names nice
  clean_names()


anti_join(target$species_code, clean_names(align)$species_code)

# get full list of spp, spp list in 1990 and spp list in 2000
fullspp <- target %>%
  select(SpeciesCode)


# reduce species list to 1990 species (only 12 categories measured - so not too taxonomically interesting)

# make list of all possible spp names and codes from target dataset
spplist <- align %>%
  select(SpeciesCode, Scientific_name) %>%
  distinct()

# all categories
target_all <- target %>%
  group_by(SiteCode, SiteName, ZoneName, SurveyYear, Zone, Plot_num, Plot_code, SpeciesCode, Scientific_name) %>%
  summarise(cover = sum(N))


# get total # of pts surveyed per plot in each site/spp/year/season (range = 89-104)
target_summary <- target_all %>%
  group_by(SiteCode, SiteName, ZoneName, SurveyYear, Zone, Plot_num, Plot_code) %>% 
  summarise(points = sum(cover))

# merge w target dataset (to calculate % cover)
target_all <- 
  # align # pts per plot with target 
  left_join(target_all, target_summary) %>%
  # calculate % cover (range of # pts is 89 - 104)
  group_by(SiteCode, SiteName, ZoneName, SurveyYear, Zone, Plot_num, Plot_code, SpeciesCode, Scientific_name) %>%
  mutate(pct_cover = (cover/points)*100) %>%
  # streamline dataset
  select(SiteCode, SiteName, ZoneName, SurveyYear, Zone, Plot_num, Plot_code, SpeciesCode, Scientific_name, pct_cover) 

# repeat process for 1990
target_1990 <- target %>%
  group_by(SiteCode, SiteName, ZoneName, SurveyYear, Zone, Plot_num, Plot_code, Code_1990) %>%
  summarise(cover = sum(N))

target_1990 <- left_join(target_1990, target_summary) %>%
  group_by(SiteCode, ZoneName, SiteName, SurveyYear, Zone, Plot_num, Plot_code, Code_1990) %>%
  mutate(pct_cover = (cover/points)*100) %>%
  select(SiteCode, SiteName, ZoneName, SurveyYear, Zone, Plot_num, Plot_code, Code_1990, pct_cover) %>%
  rename(SpeciesCode = Code_1990)

# join spp names to 1990 data
target_1990 <- left_join(target_1990, spplist)

# remove extras
remove(target_summary, align, spplist, target)

##### tidy: timed search #####
timed_search <- timed_search %>%
  filter(SiteCode %in% cabrsites) %>%
  mutate(ZoneName = paste('Zone', substr(SiteName, 9, 12)),
         Panel = if_else(SiteName == 'Cabrillo I', 'A', 
                         if_else(SiteName == 'Cabrillo II', 'B', 'C'))) %>%
  mutate(ZoneName = paste('(', Panel, ') ', ZoneName, sep = '')) %>%
  group_by(ZoneName, SurveyYear, SpeciesCode, Scientific_name) %>%
  summarise(`Number observed` = sum(N))