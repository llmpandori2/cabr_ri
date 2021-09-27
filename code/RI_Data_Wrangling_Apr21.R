#################################################
# Title: Cleaning Script for RI Data - Updated for 2020-2021
# Purpose: clean up data for consistency in reporting
# Author: LP
# Created: 4/13/21
# Last edited: 4/13/21
##################################################
##### packages #####
library(readxl)
library(tidyverse)
library(janitor)
library(wesanderson)
library(PNWColors)
library(ggdark)
library(Hmisc)
library(calecopal)
library(viridis)
library(broom)

##### settings #####
# current year 
report_year <- 2021

# years/seasons considered for 10 yr report
ten_year <- c((report_year - 10):(report_year - 2))

# 5 yr period for annual report
five_year <-c((report_year - 5):(report_year - 1))

##### load data #####

# Notes on converting from Pre-2020 Data (Raw Data/MEDN_RI_DB_pre2020) to current data (Raw Data/MEDN_RI_DB_SP21):
  # LOTTIA
    # qsummarizer_LIM_count is now Limpet_plot_counts
    # qsummarizer_LIM_density_bysize is now Limpet_density_by_plot_size
    # qsummarizer_LIM_measure is now Limpet_measurements
  # TRANSECT
    # qsummarizer_PHY_bytransect is now Line_transect_summary
    # qsummarizer_PHY_totalavg is now Line_transect_average
  # TARGET/PHOTOPLOT
    # qsummarizer_TGT is now Photoplot_summary
    # qsummarizer_SppN_rawdata is now Photoplot_summary_by_plot

# owl limpet data (doesn't need alignment - spp consistent throughout)
lim_density <- read_excel('Raw Data/MEDN_RI_DB_SP21/Limpet_density_by_plot_size_20210413.xlsx')
lim_measure <- read_excel('Raw Data/MEDN_RI_DB_SP21/Limpet_measurements_20210414.xlsx')

# transect data (doesn't need alignment - spp consistent throughout)
transect <- read_excel('Raw Data/MEDN_RI_DB_SP21/Line_transect_summary_20210413.xlsx')

# target data (needs alignment - spp lists differ by year)
target <- read_excel('Raw Data/MEDN_RI_DB_SP21/Photoplot_summary_by_plot_20210414.xlsx')

# alignment info for target data 
align <- read_csv('Raw Data/TGT_all.csv')
# see RI_Species_Alighment_Feb21.R for methods
# summary: took species lists for 1990 and 2000, applied to all unique spp.

##### tidy: limpet #####
# no tidy needed, keep 2 separate datasets for 2 vis types

##### tidy: transect ######
# adapted from B. Hong's 2019-2021 Annual Report Code

# tidy 
transect <- transect %>%
  # remove duplicated rows (not present as of SP20 data)
  distinct() %>%
  # collapse PHYOVE (phyllospadix overstory) and PHYUND (understory) to PHYALL
  mutate(
    SpeciesCode = if_else(SpeciesCode %in% c('PHYOVE', 'PHYUND'), 'PHYALL', SpeciesCode),
    Scientific_name = if_else(Scientific_name %in% c('Phyllospadix spp', 'Phyllospadix torreyi (understory)'), 'Phyllospadix spp.', Scientific_name)) %>%
  # capitalize first letter of strings
  mutate(Scientific_name = str_to_sentence(Scientific_name)) %>%
  # calculate new totals/% cover with new category
  group_by(SiteCode, SiteName, SurveyYear, Season, SeasonName, SurveyDate, Transect, SpeciesCode, Scientific_name, TotalPoints) %>%
  summarise(points = sum(N)) %>%
  # calculate new % cover
  mutate(pct_cover = (points/TotalPoints)*100)

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
  # only get unique rows (removes duplicates)
  distinct() 

# summarize by 2 methods: 1990 (1990-present), and 2000 (2000-present)

# make list of all possible spp names and codes from target dataset
spplist <- align %>%
  select(SpeciesCode, Scientific_name) %>%
  distinct()

# all categories
target2020 <- target %>%
  group_by(SiteCode, SiteName, SurveyYear, Zone, Plot_num, Plot_code, SpeciesCode, Scientific_name) %>%
  summarise(cover = sum(N))


# get total # of pts surveyed per plot in each site/spp/year/season (range = 89-104)
target_summary <- target2020 %>%
  group_by(SiteCode, SiteName, SurveyYear, Zone, Plot_num, Plot_code) %>% 
  summarise(points = sum(cover))

# merge w target dataset (to calculate % cover)
target2020 <- 
  # align # pts per plot with target 
  left_join(target2020, target_summary) %>%
  # calculate % cover (range of # pts is 89 - 104)
  group_by(SiteCode, SiteName, SurveyYear, Zone, Plot_num, Plot_code, SpeciesCode, Scientific_name) %>%
  mutate(pct_cover = (cover/points)*100) %>%
  # streamline dataset
  select(SiteCode, SiteName, SurveyYear, Zone, Plot_num, Plot_code, SpeciesCode, Scientific_name, pct_cover) 

# repeat process for 1990
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

# repeat process for 2000
target2000 <- target %>%
  group_by(SiteCode, SiteName, SurveyYear, Zone, Plot_num, Plot_code, Code_2000) %>%
  summarise(cover = sum(N))

target2000 <- left_join(target2000, target_summary) %>%
  group_by(SiteCode, SiteName, SurveyYear, Zone, Plot_num, Plot_code, Code_2000) %>%
  mutate(pct_cover = (cover/points)*100) %>%
  select(SiteCode, SiteName, SurveyYear, Zone, Plot_num, Plot_code, Code_2000, pct_cover) %>%
  rename(SpeciesCode = Code_2000) %>%
  # limit to applicable years (2000 onward)
  filter(SurveyYear >= 2000) 

target2000 <- left_join(target2000, spplist)

# remove extras
remove(target_summary, target_raw, align, spplist)

##### qc vis set-up #####

# lists of commonly filtered-for items
cabrsites <- c('CAB1', 'CAB2', 'CAB3') 
zonelist <- c('CHT', unique(target$Zone)[1:4])
zonenames <- c('Balanus/Chthamalus', 'Tetraclita rubescens', 'Mussel', 'Silvetia compressa', 'Pollicipes polymerus')
targetlist <- c('CHTBAL', 'TETRUB', 'MUSSEL', 'SILCOM', 'POLPOL')
# theme arguments
lltheme <- theme(text = element_text(size = 12),
                 # add more space between panels
                 panel.spacing = unit(1, 'lines'),
                 # no background to wrap panels
                 strip.background = element_blank(),
                 strip.text = element_text(size = 12),
                 # panel labels outside x axis labels
                 strip.placement = 'outside',
                 # adjust x axis labels
                 axis.text.y = element_text(size = 12),
                 axis.text.x = element_text(size = 12, angle = 45, hjust = 1))

bigtexttheme <- theme(text = element_text(size = 14),
                      # add more space between panels
                      panel.spacing = unit(1, 'lines'),
                      # no background to wrap panels
                      strip.background = element_blank(),
                      strip.text = element_text(size = 14),
                      # panel labels outside x axis labels
                      strip.placement = 'outside',
                      # adjust x axis labels
                      axis.text.y = element_text(size = 14),
                      axis.text.x = element_text(size = 14, angle = 45, hjust = 1))

##### qc vis: photoplot #####

# area plots for cabr target spp in 1990 per plot (lots of figs ahead - for qc purpose)

# step 1 - wrangle
cabr1990 <- target1990 %>%
  filter(SiteCode %in% cabrsites) %>%
  # make pretty label columns
  mutate(Nice_num = paste('Plot ', Plot_num))

# step 2 - plot w/ lots of facets (one per target spp)
# make loop eventually...start w single taxa focus
# emphasize target taxon/a, de-emphasize others

for(i in c(1,3:length(zonelist))){
  ggplot(data = filter(cabr1990, Zone == zonelist[i]),
         mapping = aes(x = SurveyYear, y = pct_cover, 
                       # make target spp last (bottom bar on plots)
                       fill = fct_relevel(Scientific_name, zonenames[i], after = Inf))) +
    # bar stacked by spp group (Scientific_name)
    geom_bar(stat = 'identity', width = 1) + 
    # plots in grids by site and plot #
    facet_grid(SiteName ~ Nice_num) + 
    # axis and plot labels
    xlab('Year') + 
    ylab('Percent Cover') + 
    ggtitle(paste('Target Taxa:', zonenames[i])) +
    # colors
    scale_fill_manual(name = 'Taxon',
                      values = c(pnw_palette('Sailboat', 8, type = 'continuous'), 'gray20')) +
    theme_bw() + 
    lltheme
  
  ggsave(paste('RI_Plots_Apr21/TARGET_Series_by_Plot_',targetlist[i], '.png', sep = ''),
         width = 10)
}

##### qc vis: transect #####

# wrangle data
transect <- transect %>%
  # remove NAs 
  filter(!is.na(Transect)) %>%
  # add transect type column
  mutate(Zone = if_else(Transect %in% 1:2, 'Red algal turf', 
                        if_else(Transect %in% 3:4, 'Seagrass', 'Boa kelp'))) %>%
  mutate(Nice_zone = paste(Zone, Transect))

# get only cabr data
cabrtransect <- filter(transect, SiteCode %in% cabrsites)

# make a list of transect types over which to loop
transectnames <- unique(transect$Zone)

for(i in 1:length(transectnames)) {
  
  ggplot(data = filter(cabrtransect, Zone == transectnames[1]), 
         mapping = aes(x = SurveyYear, y = pct_cover, 
                       fill = Scientific_name)) + 
    geom_bar(stat = 'identity', width = 1) +
    facet_grid(SiteName ~ Nice_zone) +
    # axis and plot labels
    xlab('Year') + 
    ylab('Percent Cover') + 
    ggtitle(paste('Target Taxa:', transectnames[i])) +
    # colors
    scale_fill_manual(name = 'Taxon',
                      values = c(pnw_palette('Sailboat', 
                                             length(unique(cabrtransect$Scientific_name)), 
                                             type = 'continuous'), 'gray20')) +
    theme_bw() + 
    lltheme
  ggsave(paste('RI_Plots_Apr21/TRANSECT_Series_by_Plot_',zonelist[i], '.png', sep = ''),
         width = 10)
}

##### summary vis: photoplot #####

# no summarization
for(i in 1:length(targetlist)) {
  ggplot(data = filter(cabr1990, Zone == zonelist[i] & SpeciesCode %in% targetlist[i]),
         mapping = aes(x = SurveyYear, y = pct_cover)) + 
    geom_point(mapping = aes(color = Nice_num), size = 2) + 
    scale_color_manual(name = 'Plot Number',
                       values = c(cal_palette('kelp1', 7, type = 'continuous'))) +
    geom_smooth(color = 'black', method = 'lm', formula = y ~ x) +
    facet_wrap(~SiteName) + 
    # axis and plot labels
    xlab('Year') + 
    ylab('Percent Cover') + 
    ggtitle(paste('Target Taxa:', zonenames[i])) +
    # colors
    theme_bw() + 
    bigtexttheme
  
  # save resulting plot in plots mar21 folder
  ggsave(paste('RI_Plots_Apr21/TARGET_TIMESERIES1_',targetlist[i], '.png', sep = ''),
         width = 10)  
}

# get real creative - heatmap time series for all (slope = color for future idea?)

# limit dataset to only target spp in target plot (ex - mussels in mussel plots)
tgt_in_summary <- cabr1990 %>% 
  # make zone list where spp codes == zone names
  mutate(Zone2 = if_else(Zone == 'CHT', 'CHTBAL',
                         if_else(Zone == 'MYT', 'MUSSEL',
                                 if_else(Zone == 'SIL', 'SILCOM', 'POLPOL')))) %>%
  # get only where zone == target spp, and also tetrub in chtbal plots 
  filter(Zone2 == SpeciesCode | Zone2 == 'CHTBAL' & SpeciesCode == 'TETRUB') %>%
  mutate(Scientific_name2 = if_else(Scientific_name == 'Mussel', 'Mytilus/Septifer',
                                    Scientific_name)) %>%
  # get mean % cover for each type across years
  group_by(SiteName, SurveyYear, Zone, Scientific_name2) %>%
  summarise(
    cover_mean = mean(pct_cover),
    cover_sd = sd(pct_cover))

tgt_in <-   tgt_in_summary <- cabr1990 %>% 
  # make zone list where spp codes == zone names
  mutate(Zone2 = if_else(Zone == 'CHT', 'CHTBAL',
                         if_else(Zone == 'MYT', 'MUSSEL',
                                 if_else(Zone == 'SIL', 'SILCOM', 'POLPOL')))) %>%
  # get only where zone == target spp, and also tetrub in chtbal plots 
  filter(Zone2 == SpeciesCode | Zone2 == 'CHTBAL' & SpeciesCode == 'TETRUB') %>%
  mutate(Scientific_name2 = if_else(Scientific_name == 'Mussel', 'Mytilus/Septifer',
                                    Scientific_name))



# try boxplots w/ star at recent year
ggplot(data = tgt_in_summary,
       mapping = aes(x = cover_mean, y = Scientific_name2)) + 
  geom_boxplot(mapping = aes(fill = Scientific_name2)) + 
  geom_point(data = filter(tgt_in_summary, SurveyYear == 2017),
             mapping = aes(size = 2), pch = 8) +
  facet_wrap(~SiteName) + 
  xlab('Percent cover') + 
  ylab('Target taxa') + 
  ggtitle('Target Taxa Cover Summary across Sites') + 
  scale_fill_manual(values = cal_palette('kelp1', 6, 'discrete')) + 
  dark_theme_bw() + 
  lltheme + 
  theme(legend.position = 'none',
        axis.text.y = element_text(face = 'italic'))

ggsave('RI_Plots_Apr21/TARGET_BOXPLOTS.png', width = 10)

# linreg to accompnay boxplots
# make big panel fig w/ same things
ggplot(data = tgt_in,
       mapping = aes(x = SurveyYear, y = pct_cover)) + 
  geom_point(mapping = aes(color = Nice_num), size = 2) + 
  scale_color_manual(name = 'Plot Number',
                     values = c(cal_palette('kelp1', 7, type = 'continuous'))) +
  geom_smooth(color = 'black', method = 'lm', formula = y ~ x) +
  facet_grid(Zone2~SiteName, scales = 'free') + 
  # axis and plot labels
  xlab('Year') + 
  ylab('Percent Cover') + 
  ggtitle('Target Taxa Regressions') +
  # colors
  theme_bw() + 
  lltheme

ggsave('RI_Plots_Apr21/TARGET_Mega_Linreg.png', width = 10, height = 10)

