#################################################
# Title: MS Code - Visitation and Long Term Monitoring
# Purpose: analyses and figures for manuscript
# Author: LS & LP
# Created: 3/28/22
# Last edited: 3/28/22
##################################################

# all brainstorming for this MS in cabr_intertidal_community_ms repo

##### presets #####



##### load packages #####

library(readxl)     # load excel file
#library(grDevices)
#library(patchwork) # arrange plots
library(janitor)    # clean up messy datasets + names
library(ggdark)     # dark theme plots for presentations
#library(Hmisc)      
library(broom)      # glance command for linregs   
library(viridis)    # color palette
library(lubridate)  # deal with dates and times
library(vegan)      # multivariate toolbox
library(gt)         # generate tables w/ gg framework
#library(ggpubr)
#library(rstatix)
#library(labdsv) #hellinger transformation
library(tidyverse)  # all things tidy