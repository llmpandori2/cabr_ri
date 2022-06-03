#################################################
# Title: Report Figures & Stats - 1990-2021 Trend Report
# Purpose: adapted from LT report code + clean up code
# Author: LP
# Created: 4/20/22
# Last edited: 5/2/22
##################################################

# save place
saveplace <- './figs/lt_trend_report_figs/tables/'
tidy_folder <- './data/lt_trend_report_tidy_data/'

##### packages #####
library(readxl)
library(janitor)
library(viridis)
library(tidyverse)

##### load data #####
# set wd to folder with data for year of interest
setwd("D:/LP_Files/RStudio_Files/cabr_ri/data/MEDN_RI_DB_JAN21")

# owl limpet data

  # note - used "count" data because density file doesn't contain plots > 5
lim_density <- read_excel('Limpet_plot_counts.xlsx')
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

# visitation data from shorebird/people counts
people_count <- read_excel("data/Shorebird_People_Data_LS_to_2020.xlsx")

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
  ungroup() %>%
  # widdle down columns
  select(-c(season:survey_date, total_points, points)) %>%
  rename(taxa_code = species_code) %>%
  # make zone column
  mutate(zone = case_when(site_name == 'Cabrillo I' ~ 'Zone I',
                          site_name == 'Cabrillo II' ~ 'Zone II',
                          site_name == 'Cabrillo III' ~ 'Zone III'))

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
  group_by(survey_year, taxa_code, scientific_name) %>%
  summarize(pct_cover_na = if_else(!is.na(pct_cover), 'X', '')) %>%
  distinct() %>%
  pivot_wider(., names_from = survey_year, values_from = pct_cover_na) %>%
  arrange(scientific_name) %>%
  write_csv(., paste0(saveplace, 'transect_spp_table.csv'))

# data notes: 
  # prior to 2003, only target spp counted
  # 2003-2020, spp list relatively consistent - use for community
  # 2018 data missing (not verified?)

# write csv with tidy version of data
  write_csv(select(transect, -site_name, -site_code), paste0(tidy_folder, 'transect.csv'))
  
##### tidy: photoplot ####
  
# get scoring method from target_scoretype (add to % cover tables later - unfinished)
target_scoretype <- target_scoretype %>%
    # snake case column names
    clean_names() %>%
    # collapse scoring types
    mutate(method_code = case_when(method_code ==  'F' ~ 'Field',
                                   method_code %in% c('LBS', 'LBD', 'O') ~ 'Photo',
                                   TRUE ~ '')) %>%
    # select relevant columns
    group_by(site_id, sample_season_code, quad_number, target_species) %>%
    # if multiple methods, no separating comma
    summarize(method = paste(unique(method_code), collapse = '')) %>%
    # make names // to target for joining
    ungroup() %>%
    rename(site_code = site_id, season = sample_season_code) %>%
    mutate(plot_code = paste(
      case_when(target_species == 'chthalamus_balanus' ~ 'CHT',
                target_species == 'mytilus' ~ 'MYT',
                target_species == 'pollicipes' ~ 'POL',
                target_species == 'silvetia' ~ 'SIL'),
      '-0', quad_number, sep = '')) %>%
    select(-target_species, -quad_number)
  
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
  mutate(scientific_name = str_to_sentence(scientific_name)) %>%
  # join with scoring method type
  left_join(., target_scoretype, by = c('site_code', 'season', 'plot_code')) %>%
  mutate(method = if_else(method %in% c('Photo', 'Field'), method, 'Not specified')) %>%
  distinct()

# remove scoring method data (not needed anymore)
remove(target_scoretype)
  
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
  
  # table w info on scoring type
  rbind(target, mutate(target[1,], survey_year = 2018, pct_cover = NA)) %>%
    group_by(site_code, plot_code, survey_year) %>%
    summarize(method = method) %>%
    distinct() %>%
    pivot_wider(., names_from = survey_year, values_from = method) %>%
    write_csv(., paste0(saveplace,'target_method_table.csv'))
  
# step 2 of tidy - after looking at above tables
target <- target %>%
    # limit to mussel plots <= 5 and pollicipes plots <= 6 
    filter(!plot_code %in% c('MYT-06', 'POL-07', 'POL-08')) %>%
    # align with taxa codes for 1990 and 2000s
    left_join(., taxa_crosswalk, by = c('species_code', 'scientific_name'))

# notes: 
  # Po plots not established until 1995
  # Po plots > 6 removed, other plots > 5 removed from analyses (added later)

# export to tidy data folder (commented out b/c not used in LT ms)
# write_csv(target, paste0(tidy_folder, 'target.csv'))

# calculate % cover for 90s data and 00s data
target_90 <- ungroup(target) %>%
  group_by(site_name, survey_year, type, plot_code, code_1990, name_1990) %>%
  summarize(pct_cover = sum(pct_cover)) %>%
  rename(taxa_code = code_1990, scientific_name = name_1990) %>%
  # make zone column
  mutate(zone = case_when(site_name == 'Cabrillo I' ~ 'Zone I',
                          site_name == 'Cabrillo II' ~ 'Zone II',
                          site_name == 'Cabrillo III' ~ 'Zone III')) %>%
  ungroup()

write_csv(select(target_90, -site_name), paste0(tidy_folder, 'target_90.csv'))

target_00 <- ungroup(target) %>%
  group_by(site_name, survey_year, type, plot_code, code_2000, name_2000) %>%
  summarize(pct_cover = sum(pct_cover)) %>%
  rename(taxa_code = code_2000, scientific_name = name_2000)%>%
  # make zone column
  mutate(zone = case_when(site_name == 'Cabrillo I' ~ 'Zone I',
                          site_name == 'Cabrillo II' ~ 'Zone II',
                          site_name == 'Cabrillo III' ~ 'Zone III')) %>%
  ungroup()

write_csv(select(target_00, -site_name), paste0(tidy_folder, 'target_00.csv'))

# remove crosswalk table
remove(taxa_crosswalk)

##### tidy: timed search #####

timed_search <- timed_search %>%
  # snake case col names
  clean_names() %>%
  # get years 1990 - 2021, fall seasons & sites 
  filter(site_code %in% cabrsites & season_name == 'Fall' & survey_year < 2022) %>%
  # select columns of interest
  group_by(site_name, survey_year, species_code, scientific_name) %>%
  # there's one weird duplicate - 2 x timed searches for CAB3 in FA17 - I chose the one entered by BH that didn't include size info
  summarize(n = min(n)) %>%
  # change spp code to taxa code
  rename(taxa_code = species_code) %>%
  # get distinct values
  distinct() %>%
  # make zone column
  mutate(zone = case_when(site_name == 'Cabrillo I' ~ 'Zone I',
                          site_name == 'Cabrillo II' ~ 'Zone II',
                          site_name == 'Cabrillo III' ~ 'Zone III')) %>%
  ungroup()

# make summary table of spp searched for and # found
timed_search %>%
  # add blank 2018 column
  rbind(., mutate(timed_search[1,], survey_year = 2018, n = NA)) %>%
  arrange(survey_year) %>%
  # make matrix w/ years as columns
  pivot_wider(., names_from = survey_year, values_from = n) %>%
  # save as csv
  write_csv(., paste0(saveplace, 'timed_search_table.csv'))

# write csv file for tidy data
write_csv(select(timed_search, -site_name), paste0(tidy_folder, 'timed_search.csv'))

# data notes: 
 # there were 2 numbers for 2017 CAB3 for H. fulgens. I took the lower one, which was from the non-measured abalone data

##### tidy limpet data - density + size #####

# limpet density 
lim_density <- lim_density %>%
  # snake case col names
  clean_names() %>%
  # get years 1990 - 2021 @ CABR sites. Kept both seasons
  filter(site_code %in% cabrsites & survey_year < 2022) %>%
  # remove duplicate columns (none - code is placeholder)
  distinct() %>%
  # one weird duplicated data point (2021 CABR2 Plot 5)
  group_by(site_name, survey_year, season_name, season, plot_no) %>%
  summarize(n = min(n),
            density = n/pi) %>%
  ungroup()

# make table of available data
lim_density %>%
  select(site_name, plot_no, season, density) %>%
  mutate(density = round(density, digits = 2)) %>%
  # summarize across years 
  pivot_wider(., names_from = c(site_name, plot_no), values_from = density) %>%
  write_csv(., paste0(saveplace, 'lim_density_table.csv'))

# data notes: 
  # plots above #5 added ~ 2002, not included
  # CABR2 L4 & L5 not included; L4 moved over time, L5 data ends ~ 2002
  # "size" data used to calculate density b/c "density" file doesn't include plots above #5

lim_density <- lim_density %>%
  filter(plot_no < 6) %>%
  filter(!(plot_no < 3 & site_name == 'Cabrillo II')) %>%
  # make zone column
  mutate(zone = case_when(site_name == 'Cabrillo I' ~ 'Zone I',
                          site_name == 'Cabrillo II' ~ 'Zone II',
                          site_name == 'Cabrillo III' ~ 'Zone III')) %>%
  ungroup()

# write csv file with resulting data
write_csv(select(lim_density, -site_name), paste0(tidy_folder, 'lim_density.csv'))

# limpet size (same plot #'s used as lim_density)
lim_size <- lim_measure %>%
  # snake case col names
  clean_names() %>%
  # get years 1990 - 2021 @ CABR sites. Kept both seasons
  filter(site_code %in% cabrsites & survey_year < 2022) %>%
  # remove duplicate columns (none - code is placeholder)
  distinct() %>%
  # select columns of interest
  select(site_name, survey_year, season_name, season, plot_no, size_class, n) %>%
  filter(plot_no < 6) %>%
  filter(!(plot_no == 5 & site_name == 'Cabrillo II'))%>%
  # make zone column
  mutate(zone = case_when(site_name == 'Cabrillo I' ~ 'Zone I',
                          site_name == 'Cabrillo II' ~ 'Zone II',
                          site_name == 'Cabrillo III' ~ 'Zone III')) %>%
  ungroup()

# write csv file with resulting data
write_csv(select(lim_size, -site_name), paste0(tidy_folder, 'lim_size.csv'))

##### tidy - people count data #####

people_count <- people_count %>%
  # snake case column names, remove duplicates (there were none)
  clean_names() %>%
  distinct() %>%
  # only people data
  filter(data_type == 'People',
         !is.na(data_count)) %>%
  # remove missing data (1 instance where count is NA for z3 in december 1996)
  # survey year column
  mutate(survey_year = lubridate::year(survey_date)) %>%
  # calculate mean # of visitors per zone and # of surveys per year
  group_by(survey_year, zone_class) %>%
  summarize(mean_count = mean(data_count),
            n_surveys = length(unique(survey_date))) %>%
  mutate(zone = paste('Zone', zone_class)) %>%
  select(-zone_class) %>%
  ungroup()

write_csv(people_count, paste0(tidy_folder, 'people_count.csv'))

# that's a wrap - now use tidy_folder data for analyses :)