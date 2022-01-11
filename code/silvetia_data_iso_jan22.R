#################################################
# Title: Silvetia Data for SW
# Purpose: isolate Silvetia cover data in Silvetia plots for SW
# Author: LP
# Created: 1/11/22
# Last edited: 1/11/22
##################################################

##### packages #####
library(readxl)
library(janitor)
library(tidyverse)

##### load data #####
setwd("D:/LP_Files/RStudio_Files/cabr_ri/data/MEDN_RI_DB_JAN21")
target <- read_excel('Photoplot_summary_by_plot.xlsx')

##### tidy #####
target_sw <- target %>%
  filter(SiteCode %in% c('CAB1', 'CAB2', 'CAB3', 'NAVYN', 'NAVYS') & 
         SurveyYear %in% c(2019, 2020) & 
         Zone == 'SIL' & 
         SpeciesCode == 'SILCOM' &
         SeasonName == 'Fall')

##### write csv output #####
write_csv(target_sw, '../silcom_sw_jan22.csv')
