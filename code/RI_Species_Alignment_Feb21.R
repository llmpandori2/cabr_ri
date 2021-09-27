#################################################
# Title: Species List Alignment (TGT and PHY)
# Purpose: aligning species lists which change over time
# Author: LP
# Last edited: 2/23/2021
##################################################

##### packages #####

library(tidyverse)
library(janitor)

##### load data #####

# target (photoplot)
tgt <- read_csv("TGT_Spp_Codes_1990_2021.csv")
# phyllospadix (transect)
phy <- read_csv("PHY_Spp_Codes_1990_2021.csv")

##### how do spp lists differ over time? #####

# look at yrs each spp considered

  # tgt
  tgt_time <- tgt %>%
    group_by(Spp_Code, Spp_Name) %>%
    summarize(
      first_year = min(Sampling_Year),
      last_year = max(Sampling_Year))

  # substantial additions in 1996 and 2000  - align all data to 1990 and 2000

  # phy 
  phy_time <- phy %>%
    group_by(Spp_Code, Spp_Name) %>%
    summarize(
      first_year = min(Sampling_Year),
      last_year = max(Sampling_Year))
  
  # phy hasn't changed over time - no changes needed to list   
  
##### align target spp to 1990 and 2000 lists #####
# done in Excel by hand, stored in TGT_all.csv
  # Code_1990 is the code if it was categorized in 1990
  # Code_2000 is code if it was categorized in 2000
  # if no match for a category, code is 'NOMATCH' in Code_1990 and/or Code_2000

# read spreadsheet
align <- read_csv('TGT_all.csv')


  
  
    