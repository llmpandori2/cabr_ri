#################################################
# Title: Report Figures & Stats - 2021 Annual Report
# Purpose: adapted from clean-up code, figures for annual report & 10 yr
# Author: LP
# Created: 4/14/21
# Last edited: 1/4/22
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

# transect data (doesn't need alignment - spp consistent throughout)
transect <- read_excel('Line_transect_summary.xlsx')

# target data (needs alignment - spp lists differ by year)
target <- read_excel('Photoplot_summary_by_plot.xlsx')

# timed search
timed_search <- read_excel("TimedSearch_plot_counts.xlsx")

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
lltheme_heatmap <- theme_bw() + theme(text = element_text(size = 12),
                                      # add more space between panels
                                      panel.spacing = unit(1, 'lines'),
                                      # no background to wrap panels
                                      strip.background = element_blank(),
                                      strip.text = element_text(size = 12, hjust = 0),
                                      # panel labels outside x axis labels
                                      strip.placement = 'outside',
                                      # adjust x axis labels
                                      axis.text.y = element_text(size = 11, color = 'black'),
                                      axis.text.x = element_text(size = 11, angle = 45, hjust = 1, color = 'black'),
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

##### tidy: transect ######

# tidy 
transect <- transect %>%
  # remove duplicated rows (not present as of SP20 data)
  distinct() %>%
  # combine PHYOVE (overstory) and PHYUND (understory) to PHYALL
  mutate(
    SpeciesCode = if_else(SpeciesCode %in% c('PHYOVE', 'PHYUND'), 'PHYALL', SpeciesCode),
    Scientific_name = if_else(Scientific_name %in% c('Phyllospadix spp', 'Phyllospadix torreyi (understory)'), 'Seagrass', 
                              if_else(Scientific_name == 'Egregia menziesii', 'Boa kelp', Scientific_name))) %>%
  # capitalize first letter of strings
  mutate(Scientific_name = str_to_sentence(Scientific_name)) %>%
  # calculate new totals/% cover with new category
  group_by(SiteCode, SiteName, SurveyYear, Season, SeasonName, SurveyDate, Transect, SpeciesCode, Scientific_name, TotalPoints) %>%
  summarise(points = sum(N)) %>%
  # calculate new % cover
  mutate(pct_cover = (points/TotalPoints)*100)

# further tidy spp names/category specification, etc.
red_turf_spp <- c('ARTCOR', 'OTHRED')

transect <-   transect %>%
  filter(SiteCode %in% cabrsites) %>% 
  filter(!is.na(Transect)) %>%
  filter((Transect %in% c(1:2) & SpeciesCode %in% red_turf_spp) |
           (Transect %in% c(3:4) & SpeciesCode == 'PHYALL') |
           (Transect %in% c(5:6) & SpeciesCode == 'EGRMEN')) %>%
  # add transect type column
  mutate(Zone = if_else(Transect %in% 1:2, 'Red algal turf', 
                        if_else(Transect %in% 3:4, 'Phyllospadix', 'Egregia'))) %>%
  # add zone/site column
  mutate(ZoneName = paste('Zone', substr(SiteName, 9, 12)),
         Panel = if_else(SiteName == 'Cabrillo I', 'A', 
                         if_else(SiteName == 'Cabrillo II', 'B', 'C'))) %>%
  mutate(ZoneName = paste('(', Panel, ') ', ZoneName, sep = '')) %>%
  select(-c(Panel))


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

##### tidy: timed search #####
timed_search <- timed_search %>%
  filter(SiteCode %in% cabrsites) %>%
  mutate(ZoneName = paste('Zone', substr(SiteName, 9, 12)),
         Panel = if_else(SiteName == 'Cabrillo I', 'A', 
                         if_else(SiteName == 'Cabrillo II', 'B', 'C'))) %>%
  mutate(ZoneName = paste('(', Panel, ') ', ZoneName, sep = '')) %>%
  group_by(ZoneName, SurveyYear, SpeciesCode, Scientific_name) %>%
  summarise(`Number observed` = sum(N))

##### vis: lottia heatmap #####

# size class heatmap for annual report
heatmap_annual <- lim_measure %>%
  # only CABR sites and in 10 yr study period (2010-2019)
  filter(SiteCode %in% cabrsites & 
           SurveyYear %in% c((currentyear - 6):(currentyear - 1)) & 
           Size_class >= 15) %>%
  # make site names align w/ report (zones not sites)
  mutate(ZoneName = paste('Zone', substr(SiteName, 9, 12)),
         Panel = if_else(SiteName == 'Cabrillo I', 'A', 
                         if_else(SiteName == 'Cabrillo II', 'B', 'C'))) %>%
  mutate(ZoneName = paste('(', Panel, ') ', ZoneName, sep = '')) %>%
  # select necessary columns 
  select(ZoneName, SurveyYear, Season, PlotNo, Size_class, N) %>%
  # make Size_classes into lowest 5, then make character
  mutate(Size_bin = 5*floor(Size_class/5),
         Season = parse_factor(Season)) %>%
  # group by site, survey year & plot, get total counts in each class
  group_by(ZoneName, SurveyYear, Size_bin) %>%
  summarise(`Mean count` = sum(N)/length(unique(PlotNo))) %>%
  # make size bin column categorical with bin descriptor
  mutate(Size_bin = parse_character(paste(Size_bin, '-', (Size_bin + 5))))

ggplot(data = heatmap_annual,
       mapping = aes(x = SurveyYear, y = Size_bin, fill = `Mean count`)) + 
  # give tiles black outline
  geom_tile(color = 'black') + 
  facet_wrap(~ZoneName) + 
  xlab('Sampling year') +
  ylab(expression(italic('Lottia') ~ 'size (mm)')) +
  scale_x_continuous(breaks = c(min(heatmap_annual$SurveyYear):max(heatmap_annual$SurveyYear))) + 
  # reverse viridis (colorblind friendly palette) colors
  scale_fill_viridis(begin = 1, end = 0) +
  coord_equal() +
  lltheme_heatmap

ggsave('./figs/annual_report_figs_2021/lottia_heatmap_5yr.png',
       width = 7, height = 5)

# make heatmap for all years
# size class heatmap for annual report
heatmap_all <- lim_measure %>%
  # only CABR sites and in 10 yr study period (2010-2019)
  filter(SiteCode %in% cabrsites & 
           Size_class >= 15) %>%
  # make site names align w/ report (zones not sites)
  mutate(ZoneName = paste('Zone', substr(SiteName, 9, 12)),
         Panel = if_else(SiteName == 'Cabrillo I', 'A', 
                         if_else(SiteName == 'Cabrillo II', 'B', 'C'))) %>%
  mutate(ZoneName = paste('(', Panel, ') ', ZoneName, sep = '')) %>%
  # select necessary columns 
  select(ZoneName, SurveyYear, Season, PlotNo, Size_class, N) %>%
  # make Size_classes into lowest 5, then make character
  mutate(Size_bin = 5*floor(Size_class/5),
         Season = parse_factor(Season)) %>%
  # group by site, survey year & plot, get total counts in each class
  group_by(ZoneName, SurveyYear, Size_bin) %>%
  summarise(`Mean count` = sum(N)/length(unique(PlotNo))) %>%
  # make size bin column categorical with bin descriptor
  mutate(Size_bin = parse_character(paste(Size_bin, '-', (Size_bin + 5))))

ggplot(data = heatmap_all,
       mapping = aes(x = SurveyYear, y = Size_bin, fill = `Mean count`)) + 
  # give tiles black outline
  geom_tile(color = 'black') + 
  facet_wrap(~ZoneName, ncol = 1) + 
  xlab('Sampling year') +
  ylab(expression(italic('Lottia') ~ 'size (mm)')) +
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  # reverse viridis (colorblind friendly palette) colors
  scale_fill_viridis(begin = 1, end = 0) +
  coord_equal() +
  lltheme_heatmap

ggsave('./figs/annual_report_figs_2021/lottia_heatmap_all.png', height = 10)

##### vis: lottia density time series ##### 
# overall density summary
density_summary <- lim_density %>%
  filter(SiteCode %in% cabrsites) %>%
  mutate(ZoneName = paste('Zone', substr(SiteName, 9, 12)),
         Panel = if_else(SiteName == 'Cabrillo I', 'A', 
                         if_else(SiteName == 'Cabrillo II', 'B', 'C'))) %>%
  mutate(ZoneName = paste('(', Panel, ') ', ZoneName, sep = '')) %>%
  group_by(ZoneName) %>%
  summarise(mean = mean(Density, na.rm = TRUE))

# plot all years
density <- lim_density %>% 
  filter(SiteCode %in% cabrsites) %>%
  mutate(ZoneName = paste('Zone', substr(SiteName, 9, 12)),
         Panel = if_else(SiteName == 'Cabrillo I', 'A', 
                         if_else(SiteName == 'Cabrillo II', 'B', 'C'))) %>%
  mutate(ZoneName = paste('(', Panel, ') ', ZoneName, sep = '')) %>%
  # select necessary columns 
  group_by(ZoneName, SurveyYear) %>%
  summarise(`Mean Density` = mean(Density),
            SE = sd(Density)/sqrt(length(unique(PlotNo)))) 

ggplot(data = density, 
       mapping = aes(x = SurveyYear, y = `Mean Density`)) + 
  geom_linerange(mapping = aes(x = SurveyYear, 
                               ymin = (`Mean Density` - SE),
                               ymax = (`Mean Density` + SE), color = ZoneName),
                 color = 'gray70', size = 1) +
  geom_point(aes(color = ZoneName), size = 2) + 
  geom_line(aes(color = ZoneName), size = 1) + 
  scale_color_viridis(discrete = TRUE, begin = 0, end = 0.7) +
  geom_hline(data = density_summary, aes(yintercept = mean), linetype = 'dashed', color = 'black') + 
  xlab('Year') + 
  ylab(expression(italic('Lottia') ~ 'density (count/m²)')) +
  facet_wrap(~ZoneName) + 
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) + 
  scale_y_continuous(breaks = c(5, 10, 15, 20, 25)) +
  lltheme +
  theme(aspect.ratio = 1,
        legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave('./figs/annual_report_figs_2021/lottia_density_allyrs.png',
       width = 7)

remove(density_summary)

##### vis: lottia avg size time series #####
size_summary <- lim_measure %>%
  filter(SiteCode %in% cabrsites) %>%
  mutate(ZoneName = paste('Zone', substr(SiteName, 9, 12)),
         Panel = if_else(SiteName == 'Cabrillo I', 'A', 
                         if_else(SiteName == 'Cabrillo II', 'B', 'C'))) %>%
  mutate(ZoneName = paste('(', Panel, ') ', ZoneName, sep = '')) %>%
  group_by(ZoneName) %>%
  summarise(mean = sum(Size_class*N)/sum(N))

# plot of size over time for all zones - annual report
size_all <- lim_measure %>%
  filter(SiteCode %in% cabrsites) %>%
  mutate(ZoneName = paste('Zone', substr(SiteName, 9, 12)),
         Panel = if_else(SiteName == 'Cabrillo I', 'A', 
                         if_else(SiteName == 'Cabrillo II', 'B', 'C'))) %>%
  mutate(ZoneName = paste('(', Panel, ') ', ZoneName, sep = '')) %>%
  group_by(SurveyYear, ZoneName) %>%
  summarise(mean = sum(Size_class*N)/sum(N),
            SE = sd(Size_class*N)/sqrt(sum(N)))

# plot
# plot time series
ggplot(data = size_all, 
       mapping = aes(x = SurveyYear, y = mean)) + 
  geom_linerange(mapping = aes(x = SurveyYear, 
                               ymin = (mean - SE),
                               ymax = (mean + SE)),
                 color = 'gray70', size = 1) +
  geom_point(aes(color = ZoneName), size = 2) + 
  geom_line(aes(color = ZoneName), size = 1) + 
  geom_hline(data = size_summary, aes(yintercept = mean), linetype = 'dashed', color = 'black') +
  scale_color_viridis(discrete = TRUE, begin = 0, end = 0.7, name = 'Site') +
  xlab('Year') + 
  ylab(expression(italic('Lottia') ~ 'size (mm)')) +
  facet_wrap(~ZoneName) + 
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) + 
  scale_y_continuous() +
  lltheme +
  theme(aspect.ratio = 1,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none')

ggsave('./figs/annual_report_figs_2021/lottia_size_allyrs.png',
       width = 7)

##### vis: transect time series #####
# get grand means and plot as facet grid for annual report

# get grand means (1990 - 2020)
transect_grandmean <- transect %>%
  group_by(ZoneName, SpeciesCode) %>%
  summarise(grand_mean = mean(pct_cover))

# facet plot for all years
transectall <- transect %>%
  mutate(Scientific_name = if_else(Scientific_name == 'Boa kelp', 'Egregia menziesii',
                                   if_else(Scientific_name == 'Seagrass', 'Phyllospadix spp.',
                                           Scientific_name))) %>%
  group_by(ZoneName, SurveyYear, Zone, SpeciesCode, Scientific_name) %>%
  summarise(mean_cover = mean(pct_cover))

transectall <- left_join(transectall, transect_grandmean)

ggplot(data = transectall,
       mapping = aes(x = SurveyYear, y = mean_cover)) + 
  geom_hline(aes(yintercept = grand_mean), linetype = 'dashed', color = 'black') + 
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
        strip.text.y = element_text(size = 12))

ggsave('./figs/annual_report_figs_2021/transect_allyrs.png', height = 8.25)

##### vis: photo plot time series #####

# use target 1990 - has fewer categories, preserves all temporal data
target_90_home <- target_1990 %>% 
  filter(SiteCode %in% cabrsites) %>%
  # filter to only tgt spp in their own plots
  filter((Zone == 'CHT' & SpeciesCode %in% c('TETRUB', 'CHTBAL')) |
           (Zone == 'MYT' & SpeciesCode == 'MUSSEL') | 
           (Zone == 'SIL' & SpeciesCode == 'SILCOM') | 
           (Zone == 'POL' & SpeciesCode == 'POLPOL')) %>%
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

##### table: timed search #####

timed_search_tbl <- ungroup(timed_search) %>%
  filter(SurveyYear == (currentyear-1)) %>%
  mutate(Zone = substr(ZoneName, 10, 13),
         Species = Scientific_name) %>%
  select(Zone, Species, `Number observed`)