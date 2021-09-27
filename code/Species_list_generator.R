#################################################
# Title: Species list
# Purpose: Generate a species list from LT monitoring and biodiversity survey protocols
# Author: LP
# Created: 9/27/21
# Last edited: 9/27/21
##################################################

##### packages #####
library(readxl)
library(janitor)
library(broom)
library(viridis)
library(tidyverse)

##### load and tidy data #####
# load spp list from MARINe
spp_codes <- read_excel("data/biodiversity/CABR_Biodiversity_20210316.xlsx", sheet = 2) %>%
  select(marine_species_code, marine_species_name)

# Goal - get spp lists for each survey type (biodiversity vs long term monitoring)
# function to get species codes and scientific names for each year
lt_year_fn <- function(dataset) {
  
  dataset2 <- dataset %>%
    select(SpeciesCode, Scientific_name, SurveyYear) %>%
    distinct()
  
  return(dataset2)
}

# apply across LT datasets
lt_data <- rbind(
  # limpet
  lt_year_fn(read_excel("data/MEDN_RI_DB_SP21/Limpet_density_total_20210413.xlsx")), 
  # photoplot
  lt_year_fn(read_excel("data/MEDN_RI_DB_SP21/Photoplot_summary_by_plot_20210414.xlsx")), 
  # transect 
  lt_year_fn(read_excel("data/MEDN_RI_DB_SP21/Line_transect_summary_20210413.xlsx")),
  # timed search
  lt_year_fn(read_excel("data/MEDN_RI_DB_SP21/TimedSearch_plot_counts_20210413.xlsx"))) %>%
  # add ID column
  mutate(data_type = 'longterm')

# function to create // set from biodiversity

bd_year_fn <- function(dataset) {
  # one dataset with spp code (star), turn into species_lump
  if(any(colnames(dataset) == 'species_code')) {
    # change to marine_species_name from spp_codes 
    dataset <- dataset %>%
      rename('marine_species_code' = 'species_code') %>%
      left_join(., spp_codes, by = 'marine_species_code') %>%
      rename('species_lump' = 'marine_species_name',
             'year' = 'marine_common_year',
             'total_count' = 'total')}
  
  # change all to abundance cols to "abundance" 
  if(any(colnames(dataset) == 'number_of_hits')) {
    dataset <- rename(dataset, 'total_count' = 'number_of_hits')}
  
  dataset2 <- dataset %>%
    # get values where abundance is non-zero
    filter(total_count > 0) %>%
    select(species_lump, year) %>%
    distinct()
  
  # rename to match
  return(dataset2)
}

bd_data <- rbind(
  # sea star data
  bd_year_fn(read_excel("data/biodiversity/CABR_Biodiversity_20210316.xlsx", sheet = 4)),
  # mobile data 
  bd_year_fn(read_excel("data/biodiversity/CABR_Biodiversity_20210316.xlsx", sheet = 6)),
  # point contact survey
  bd_year_fn(read_excel("data/biodiversity/CABR_Biodiversity_20210316.xlsx", sheet = 5))) %>%
  # add ID column
  mutate(data_type = 'biodiversity')

### make // and join biodiversity and long-term monitoring spp lists

spp_list <- rbind(lt_data %>%
                    rename('scientific_name' = 'Scientific_name', 'year' = 'SurveyYear') %>%
                    select(-c(SpeciesCode)),
                  bd_data %>% rename('scientific_name' = 'species_lump')) %>%
  # limit to years when all surveys conducted
            filter(year %in% c(2002, 2004, 2012, 2017, 2020))
  


# remove component parts
remove(lt_data, bd_data, spp_codes)

### look for inconsistencies in naming and fix in excel file
spp_list <- left_join(spp_list, read_excel("data/crosswalk/Species_List_Conversions_Sept21.xlsx"), by = 'scientific_name')

# remove na values (they are non-biotic things like zspar, rock, sand, etc.)
spp_list <- spp_list %>%
  filter(!is.na(scientific_name2))

# make list of spp with years as columns, fill in where present with survey method (b for biodiversity, l for long-term)

spp_matrix <- spp_list %>%
  mutate(present = 'X') %>%
  # limit to relevant columns
  select(scientific_name2, present, year) %>%
  distinct() %>%
  pivot_wider(names_from = year, values_from = present) %>%
  rename(`Scientific name` = 'scientific_name2')

# save
write_csv(x = spp_matrix,
          file = 'data/species_list_cabr_21.csv')



  
