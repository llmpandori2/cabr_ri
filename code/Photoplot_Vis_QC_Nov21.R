#################################################
# Title: Photoplot Visual QA/QC
# Purpose: visual qa/qc for photo plot MARINe data
# Author: LP
# Last edited: 11/15/2021
##################################################

##### packages #####
library(readxl)
library(ggdark)
library(Hmisc)
library(broom)
library(viridis)
library(tidyverse)
library(janitor)

##### load data #####

# photo plot data
photo <- read_excel(pattern = '../data/Photoplot_summary_by_plot')

# alignment info for target data 
align <- read_csv('Raw Data/TGT_all.csv')



##### presets #####

# facilitate looking at all years, and last 5 years from system date
allyrs <- c(1990:lubridate::year(Sys.Date()))
fiveyrs <- c((lubridate::year(Sys.Date())-4):lubridate::year(Sys.Date()))



