#################################################
# Title: SAMO Fund VIP Seminar Presentation Figures
# Purpose: adapted from clean-up code
# Author: LP
# Created: 4/27/21
# Last edited: 4/27/21
##################################################
##### packages #####
library(readxl)
library(tidyverse)
library(janitor)
library(ggdark)
library(Hmisc)
library(broom)
library(viridis)
library(gghighlight)

##### load data #####

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

# timed search
timed_search <- read_excel("Raw Data/MEDN_RI_DB_SP21/TimedSearch_plot_counts_20210413.xlsx")

# abiotic data
# sea surface temp from SIO Pier
sst <- read_csv("Raw Data/SIO_TEMP.csv", 
                skip = 26)

# tide predictions from tbone tides
tide <- read_csv("Raw Data/tbone_tides_hourly_1990_2026.csv", 
                 col_types = cols(datetime = col_datetime(format = "%m/%d/%Y %H:%M")))

# southern oscillation index from NOAA NCEI
soi <- read_excel("Raw Data/SOI_Data_NOAA_NCEI_1990_2020.xlsx")

##### presets #####
# lists of commonly filtered-for items
cabrsites <- c('CAB1', 'CAB2', 'CAB3') 
zonelist <- c('CHT', unique(target$Zone)[1:4])
zonenames <- c('Balanus/Chthamalus', 'Tetraclita rubescens', 'Mussel', 'Silvetia compressa', 'Pollicipes polymerus')
targetlist <- c('CHTBAL', 'TETRUB', 'MUSSEL', 'SILCOM', 'POLPOL')
currentyear <- lubridate::year(Sys.Date())

# facilitate looping over year groups for reporting
tenyrs <- c((currentyear - 11):(currentyear - 2))
fiveyrs <- c((currentyear - 6):(currentyear - 1))
allyrs <- c(1990:(currentyear - 1))

# theme arguments
lltheme <- dark_theme_bw() + theme(text = element_text(size = 14),
                              # add more space between panels
                              panel.spacing = unit(1, 'lines'),
                              # no background to wrap panels
                              strip.background = element_blank(),
                              strip.text = element_text(size = 14, hjust = 0),
                              # panel labels outside x axis labels
                              strip.placement = 'outside',
                              # adjust x axis labels
                              axis.text.y = element_text(size = 14),
                              axis.text.x = element_text(size = 14, angle = 45, hjust = 1))

##### tidy: lottia #####

##### tidy: transect ######
# adapted from B. Hong's 2019-2021 Annual Report Code

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

##### vis: lottia density time series ##### 
# overall density summary
density_summary <- lim_density %>%
  filter(SiteCode %in% cabrsites) %>%
  mutate(ZoneName = paste('Zone', substr(SiteName, 9, 12)),
         Panel = if_else(SiteName == 'Cabrillo I', 'A', 
                         if_else(SiteName == 'Cabrillo II', 'B', 'C'))) %>%
  mutate(ZoneName = paste('(', Panel, ') ', ZoneName, sep = '')) %>%
  summarise(mean = mean(Density))

meandensity <- mean(filter(lim_density, SiteCode %in% cabrsites)$Density)

# Annual Report: All Years
density <- lim_density %>% 
  filter(SiteCode %in% cabrsites) %>%
  mutate(ZoneName = paste('Zone', substr(SiteName, 9, 12)),
         Panel = if_else(SiteName == 'Cabrillo I', 'A', 
                         if_else(SiteName == 'Cabrillo II', 'B', 'C'))) %>%
  mutate(ZoneName = paste('(', Panel, ') ', ZoneName, sep = '')) %>%
  # select necessary columns 
  group_by(SurveyYear) %>%
  summarise(`Mean Density` = mean(Density),
            SE = sd(Density)/sqrt(length(unique(PlotNo)))) %>%
  mutate(ZoneName = 'all')

ggplot(data = density, 
       mapping = aes(x = SurveyYear, y = `Mean Density`)) + 
  geom_hline(data = density_summary, aes(yintercept = mean), linetype = 'dashed', color = 'white') + 
  geom_linerange(mapping = aes(x = SurveyYear, 
                               ymin = (`Mean Density` - SE),
                               ymax = (`Mean Density` + SE)),
                 color = 'gray20', size = 1) +
  geom_point(aes(color = ZoneName), size = 3) + 
  geom_line(aes(color = ZoneName), size = 1.5) + 
  scale_color_viridis(discrete = TRUE, begin = 0.5, end = 1) +
  xlab('Year') + 
  ylab(expression(italic('Lottia') ~ 'density (count/m²)')) +
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) + 
  scale_y_continuous(breaks = c(5, 10, 15, 20, 25)) +
  lltheme +
  theme(
        legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

ggsave('SAMO_Fund_Talk_Figures_Apr21/Lottia_Density_TimeSeries.png', dpi = 300)

remove(density_summary)

##### vis: lottia avg size time series #####
size_summary <- lim_measure %>%
  filter(SiteCode %in% cabrsites) %>%
  mutate(ZoneName = paste('Zone', substr(SiteName, 9, 12)),
         Panel = if_else(SiteName == 'Cabrillo I', 'A', 
                         if_else(SiteName == 'Cabrillo II', 'B', 'C'))) %>%
  mutate(ZoneName = paste('(', Panel, ') ', ZoneName, sep = '')) %>%
  summarise(mean = sum(Size_class*N)/sum(N))

# Annual report (all yrs)
size_all <- lim_measure %>%
  filter(SiteCode %in% cabrsites) %>%
  mutate(ZoneName = paste('Zone', substr(SiteName, 9, 12)),
         Panel = if_else(SiteName == 'Cabrillo I', 'A', 
                         if_else(SiteName == 'Cabrillo II', 'B', 'C'))) %>%
  mutate(ZoneName = paste('(', Panel, ') ', ZoneName, sep = '')) %>%
  group_by(SurveyYear) %>%
  summarise(mean = sum(Size_class*N)/sum(N),
            SE = sd(Size_class*N)/sqrt(sum(N))) %>%
  mutate(ZoneName = 'all')

# plot
# plot time series
ggplot(data = size_all, 
       mapping = aes(x = SurveyYear, y = mean)) + 
  geom_hline(data = size_summary, aes(yintercept = mean), linetype = 'dashed', color = 'white') +
  geom_linerange(mapping = aes(x = SurveyYear, 
                               ymin = (mean - SE),
                               ymax = (mean + SE)),
                 color = 'gray20', size = 1) +
  geom_point(aes(color = ZoneName), size = 3) + 
  geom_line(aes(color = ZoneName), size = 1.5) + 
  scale_color_viridis(discrete = TRUE, begin = 0.5, end = 1, name = 'Site') +
  xlab('Year') + 
  ylab(expression(italic('Lottia') ~ 'size (mm)')) +
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) + 
  scale_y_continuous() +
  lltheme +
  theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none')

ggsave('SAMO_Fund_Talk_Figures_Apr21/Lottia_Size_TimeSeries_allyrs.png', dpi = 300)


##### vis: transect time series #####

# get grand means (1990 - 2020)
transect_grandmean <- transect %>%
  group_by(SpeciesCode) %>%
  summarise(grand_mean = mean(pct_cover))

# tidy
transectall <- transect %>%
  group_by(SurveyYear, Zone, SpeciesCode, Scientific_name) %>%
  summarise(mean_cover = mean(pct_cover))

transectall <- left_join(transectall, transect_grandmean)

ggplot(data = transectall,
       mapping = aes(x = SurveyYear, y = mean_cover)) + 
  geom_hline(aes(yintercept = grand_mean), linetype = 'dashed', color = 'white') + 
  geom_line(aes(color = Scientific_name), size = 1) + 
  geom_point(aes(color = Scientific_name), size = 2) + 
  scale_color_viridis(discrete = TRUE, begin = 0.5, end = 1) +
  xlab('Year') + 
  ylab('Percent cover') +
  facet_wrap(~Scientific_name, scales = 'free_y') + 
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020))  +
  lltheme +
  theme(
        legend.position = 'none',
        panel.grid.minor = element_blank())

ggsave('SAMO_Fund_Talk_Figures_Apr21/Transect_TimeSeries.png', dpi = 300)

##### vis: photo plot time series #####

# use target 1990 - has fewer categories, preserves all temporal data
target_90_home <- target_1990 %>% 
  filter(SiteCode %in% cabrsites) %>%
  # filter to only tgt spp in their own plots
  filter((Zone == 'CHT' & SpeciesCode == 'CHTBAL') |
           (Zone == 'MYT' & SpeciesCode == 'MUSSEL') | 
           (Zone == 'SIL' & SpeciesCode == 'SILCOM') | 
           (Zone == 'POL' & SpeciesCode == 'POLPOL')) %>%
  # get mean + SE cover for each yr
  group_by(SurveyYear, Zone, SpeciesCode, Scientific_name) %>%
  summarise(Cover_mean = mean(pct_cover),
            Cover_SE = sd(pct_cover)/sqrt(length(pct_cover)))

# get grand means and join w/ plot data
target_grandmean <- target_90_home %>%
  group_by(SpeciesCode) %>%
  summarise(grand_mean = mean(Cover_mean))

target_90_home2 <- left_join(target_90_home, target_grandmean, 
                            by = c('SpeciesCode')) %>%
  mutate(Scientific_name = ifelse(Scientific_name == "Balanus/Chthamalus", 'Acorn barnacle', ifelse(Scientific_name == 'Silvetia compressa', 'Rockweed', ifelse(Scientific_name == 'Pollicipes polymerus', 'Goose barnacle', Scientific_name))))

# panel plot for annual report
ggplot(data = target_90_home2,
       mapping = aes(x = SurveyYear, y = Cover_mean)) + 
  geom_hline(aes(yintercept = grand_mean), linetype = 'dashed', color = 'white') + 
  geom_linerange(mapping = aes(x = SurveyYear, 
                               ymin = (Cover_mean - Cover_SE),
                               ymax = (Cover_mean + Cover_SE)),
                 color = 'gray70', size = 1) +
  geom_line(aes(color = Scientific_name), size = 1) + 
  geom_point(aes(color = Scientific_name), size = 2) + 
  scale_color_viridis(discrete = TRUE, begin = 0.5, end = 1, name = 'Site') +
  xlab('Year') + 
  ylab('Percent cover') +
  facet_wrap(~Scientific_name, scales = 'free_y') + 
  coord_cartesian(xlim = c(1990, 2020)) + 
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020))  +
  lltheme +
  theme(
        legend.position = 'none',
        panel.grid.minor = element_blank())

ggsave('SAMO_Fund_Talk_Figures_Apr21/Photoplot_TimeSeries.png', dpi = 300)

##### table: timed search #####

timed_search_tbl <- ungroup(timed_search) %>%
  filter(SurveyYear == (currentyear-1)) %>%
  mutate(Zone = substr(ZoneName, 10, 13),
         Species = Scientific_name) %>%
  select(Zone, Species, `Number observed`)

##### Number of taxa over years #####

n_spp <- ungroup(target_all) %>%
  select(SurveyYear, Scientific_name) %>%
  distinct() %>%
  group_by(SurveyYear) %>%
  summarise(n = length(Scientific_name))

  ggplot(data = n_spp, 
         mapping = aes(x = SurveyYear, y = n, fill = SurveyYear)) +
  geom_col() + 
  scale_fill_viridis() + 
  xlab('Year') + 
  ylab('Number of taxa/Categories') + 
  lltheme + 
  theme(legend.position = 'none') 
  
ggsave('number_of_taxa_over_time.png')
