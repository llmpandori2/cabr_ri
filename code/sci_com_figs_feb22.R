#################################################
# Title: Sci Com Figure Exploration
# Purpose: make 2 figures for potential sci com with RLC
# Author: LP
# Created: 2/10/22
# Last edited: 2/10/22
##################################################

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

# target data (needs alignment - spp lists differ by year)
target <- read_excel('Photoplot_summary_by_plot.xlsx')

setwd("D:/LP_Files/RStudio_Files/cabr_ri")

# alignment info for target data 
align <- read_csv('./data/crosswalk/TGT_all.csv')
# see RI_Species_Alighment_Feb21.R for methods
# summary: took species lists for 1990 and 2000, applied to all unique spp.

##### presets #####
# lists of commonly filtered-for items
cabrsites <- c('CAB1', 'CAB2', 'CAB3') 
zonelist <- c('CHT', unique(target$Zone)[1:4])
zonenames <- c('Balanus/Chthamalus', 'Tetraclita rubescens', 'Mussel', 'Silvetia compressa', 'Pollicipes polymerus')
targetlist <- c('CHTBAL', 'TETRUB', 'MUSSEL', 'SILCOM', 'POLPOL')
currentyear <- lubridate::year(Sys.Date())

# theme arguments
lltheme_heatmap <- theme_classic() + theme(text = element_text(size = 12),
                                      # add more space between panels
                                      panel.spacing = unit(1, 'lines'),
                                      # no background to wrap panels
                                      strip.background = element_blank(),
                                      strip.text = element_text(size = 12, hjust = 0),
                                      # panel labels outside x axis labels
                                      strip.placement = 'outside',
                                      # adjust x axis labels
                                      axis.text.y = element_text(size = 11, color = 'black'),
                                      axis.text.x = element_text(size = 11, color = 'black'),
                                      panel.grid.major = element_blank(),
                                      panel.grid.minor = element_blank(),
                                      legend.position = 'bottom')

lltheme <- theme_bw() + theme(text = element_text(size = 12),
                              # add more space between panels
                              panel.spacing = unit(1, 'lines'),
                              # no background to wrap panels
                              strip.background = element_blank(),
                              strip.text = element_text(size = 12, hjust = 0),
                              # panel labels outside x axis labels
                              strip.placement = 'outside',
                              # adjust x axis labels
                              axis.text.y = element_text(size = 11),
                              axis.text.x = element_text(size = 11, angle = 45, hjust = 1))

##### tidy: photoplot #####
# adapted from B. Hong's 2019-2021 Annual Report Code

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
  distinct() 

# summarize by 2 methods: all and 1990 (1990-present)

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

##### vis: lottia heatmap z3 all yrs #####

heatmap_annual <- lim_measure %>%
  # only CABR sites and in 10 yr study period (2010-2019)
  filter(SiteCode == 'CAB3' & 
           Size_class >= 15) %>%
  # select necessary columns 
  select(SurveyYear, Season, PlotNo, Size_class, N) %>%
  # make Size_classes into lowest 5, then make character
  mutate(Size_bin = 5*floor(Size_class/5),
         Season = parse_factor(Season)) %>%
  # group by site, survey year & plot, get total counts in each class
  group_by(SurveyYear, Size_bin) %>%
  summarise(`Number of limpets` = sum(N)/length(unique(PlotNo))) %>%
  # make size bin column categorical with bin descriptor
  mutate(Size_bin = parse_character(paste(Size_bin, '-', (Size_bin + 5))))

ggplot(data = heatmap_annual,
       mapping = aes(x = SurveyYear, y = Size_bin, fill = `Number of limpets`)) + 
  # give tiles black outline
  geom_tile(color = 'black') + 
  xlab('Year') +
  ylab('Limpet size (mm)') +
  scale_x_continuous(breaks = seq(min(heatmap_annual$SurveyYear),max(heatmap_annual$SurveyYear), 5)) + 
  # reverse viridis (colorblind friendly palette) colors
  scale_fill_viridis(begin = 1, end = 0) +
  coord_equal(xlim = c(1990,2021))+
  lltheme_heatmap

ggsave('./figs/sci_com_figs_feb22/lottia_heatmap_5yr.png',
       width = 6, height = 4)

##### mussel plot cover shifts #####

# use target 1990 - has fewer categories, preserves all temporal data
target_90_home <- target_1990 %>% 
  filter(SiteCode == 'CAB3') %>%
  # filter to only tgt spp in their own plots
  filter(Zone == 'MYT') %>%
  # get mean + SE cover for each yr
  group_by(ZoneName, SurveyYear, Zone, SpeciesCode, Scientific_name) %>%
  summarise(Cover_mean = mean(pct_cover),
            Cover_SE = sd(pct_cover)/sqrt(length(pct_cover)))

# get grand means and join w/ plot data
target_grandmean <- target_90_home %>%
  group_by(ZoneName, SpeciesCode) %>%
  summarise(grand_mean = mean(Cover_mean))

target_90_home <- left_join(target_90_home, target_grandmean, 
                            by = c('ZoneName', 'SpeciesCode')) %>%
  mutate(Scientific_name = if_else(Scientific_name == 'Mussel', 'Mytilus californianus', Scientific_name))

# panel plot for annual report
ggplot(data = target_90_home,
       mapping = aes(x = SurveyYear, y = Cover_mean)) + 
  geom_hline(aes(yintercept = grand_mean), linetype = 'dashed', color = 'black') + 
  geom_linerange(mapping = aes(x = SurveyYear, 
                               ymin = (Cover_mean - Cover_SE),
                               ymax = (Cover_mean + Cover_SE)),
                 color = 'gray70', size = 1) +
  geom_line(aes(color = ZoneName), size = 1) + 
  geom_point(aes(color = ZoneName), size = 2) + 
  scale_color_viridis(discrete = TRUE, begin = 0, end = 0.7, name = 'Site') +
  xlab('Year') + 
  ylab('Percent cover') +
  facet_grid(Scientific_name ~ ZoneName, scales = 'free_y') + 
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020))  +
  lltheme +
  theme(
    legend.position = 'none',
    panel.grid.minor = element_blank(),
    strip.text.x = element_text(size = 12),
    strip.text.y = element_text(size = 10))

ggsave('./figs/annual_report_figs_2021/target_allyrs.png', height = 9 )
