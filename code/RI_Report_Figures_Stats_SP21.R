#################################################
# Title: Report Figures & Stats - 2020-2021
# Purpose: adapted from clean-up code, figures for annual report & 10 yr
# Author: LP
# Created: 4/14/21
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

##### tidy: mean annual SST #####

sst <- sst %>%
  mutate(year = YEAR) %>%
  # remove extra columns
  select(-c(TIME_PST, TIME_FLAG, X10:X14, YEAR)) %>%
  # get recent data, no NA's (drops 2% of data)
  filter(year >= 1990 & SURF_TEMP_C != 'NaN' & BOT_TEMP_C != 'NaN') %>%
  # get annual mean
  group_by(year) %>%
  summarise(surface_mean = mean(SURF_TEMP_C),
            surface_SD = sd(SURF_TEMP_C)/sqrt(length(SURF_TEMP_C)))

##### tidy: annual hrs emersed #####
tide <- tide %>% 
  # to calculate # annual hrs emersed, water = 0, air = 1
  mutate(em_0 = if_else(tidelvl >= 0, 0, 1),
         em_05 = if_else(tidelvl >= 0.5, 0, 1),
         em_1 = if_else(tidelvl >= 1, 0,1),
         year = lubridate::year(datetime)) %>%
  group_by(year) %>%
  summarise(`0` = ((sum(em_0)/length(em_0))*100),
            `0.5` = ((sum(em_05)/length(em_05))*100),
            `1` = ((sum(em_1)/length(em_1))*100)) %>%
  filter(year %in% allyrs) %>%
  pivot_longer(!year, names_to = 'tide_ht', values_to = 'pct_time_emersed')

##### tidy: southern oscillation index (SOI) #####
soi <- soi %>%
  # parse month and date
  mutate(date = lubridate::as_date(paste(year, mont, '1', sep = '-')),
         numeric_date = year + ((mont*8.33)/100)) %>%
  select(year, date, numeric_date, soi)

##### tidy: timed search #####
timed_search <- timed_search %>%
  filter(SiteCode %in% cabrsites) %>%
  mutate(ZoneName = paste('Zone', substr(SiteName, 9, 12)),
         Panel = if_else(SiteName == 'Cabrillo I', 'A', 
                         if_else(SiteName == 'Cabrillo II', 'B', 'C'))) %>%
  mutate(ZoneName = paste('(', Panel, ') ', ZoneName, sep = '')) %>%
  group_by(ZoneName, SurveyYear, SpeciesCode, Scientific_name) %>%
  summarise(`Number observed` = sum(N))

##### vis: SST #####

# 10 yr report
ggplot(data = filter(sst, year %in% tenyrs), 
       mapping = aes(x = year, y = surface_mean)) + 
  geom_linerange(mapping = aes(x = year, 
                               ymin = (surface_mean - surface_SD),
                               ymax = (surface_mean + surface_SD)),
                 color = 'gray70', size = 1) +
  geom_point(size = 2, color = '#440154FF') + 
  geom_line(size = 1, color = '#440154FF') + 
  # add 30 yr (1990 - 2020) mean line
  geom_hline(mapping = aes(yintercept = mean(sst$surface_mean)),
             linetype = 'dashed', color = 'black') +
  scale_color_viridis(discrete = TRUE, begin = 0, end = 0.7) +
  xlab('Year') + 
  ylab('Sea surface temperature (°C)') +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018)) + 
  lltheme +
  theme(aspect.ratio = 1,
        legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave('RI_Plots_10yr_2021/SST_TimeSeries_10yr.png')

# Annual report (all yrs)
ggplot(data = filter(sst, year %in% allyrs), 
       mapping = aes(x = year, y = surface_mean)) + 
  geom_linerange(mapping = aes(x = year, 
                               ymin = (surface_mean - surface_SD),
                               ymax = (surface_mean + surface_SD)),
                 color = 'gray70', size = 1) +
  geom_point(size = 2, color = '#440154FF') + 
  geom_line(size = 1, color = '#440154FF') + 
  scale_color_viridis(discrete = TRUE, begin = 0, end = 0.7) +
  xlab('Year') + 
  ylab('Sea surface temperature (°C)') +
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) + 
  geom_hline(mapping = aes(yintercept = mean(surface_mean)),
             linetype = 'dashed', color = 'black') +
  lltheme +
  theme(legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave('RI_Plots_Annual_2021/SST_TimeSeries_allyrs.png')

##### vis: SOI #####

# 10 yr report
ggplot(data = filter(soi, year %in% tenyrs),
       mapping = aes(x = numeric_date, y = soi)) + 
  # labels for el nino/la nina
   annotate('text', label = 'El Niño', x = 2010.25, y = -2.75, size = 5, color = 'black') +
   annotate('text', label = 'La Niña', x = 2010.25, y = 2.75, size = 5, color = 'black') +
  # add in data points connected by line
  geom_point(color = 'black', size = 2) + 
  geom_line(size = 1, color = 'black') + 
  xlab('Year') + 
  ylab('Southern Oscillation Index') +
  coord_cartesian(xlim = c(2010, 2020), ylim = c(-3, 3)) + 
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018, 2020)) + 
  # dashed horizontal line at y = 0
  geom_hline(aes(yintercept = 0), linetype = 'dashed', color = 'black') + 
  lltheme +
  theme(legend.position = 'none')


ggsave('RI_Plots_10yr_2021/SOI_TimeSeries_10yr_Horizontal.png')

# Annual report (all yr series)
ggplot(data = filter(soi, year >= 1990),
       mapping = aes(x = numeric_date, y = soi)) + 
  # labels for el nino/la nina
  annotate('text', label = 'El Niño', x = 1990.25, y = -2.75, size = 5, color = 'black') +
  annotate('text', label = 'La Niña', x = 1990.25, y = 2.75, size = 5, color = 'black') +
  # add in data points connected by line
  geom_point(color = 'black', size = 2) + 
  geom_line(size = 1, color = 'black') + 
  xlab('Year') + 
  ylab('Southern Oscillation Index') +
  coord_cartesian(xlim = c(1990,2020), ylim = c(-3, 3)) + 
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) + 
  # dashed horizontal line at y = 0
  geom_hline(aes(yintercept = 0), linetype = 'dashed', color = 'black') + 
  lltheme +
  theme(legend.position = 'none')

ggsave('RI_Plots_Annual_2021/SOI_TimeSeries_allyrs.png')

##### vis: hrs emersed #####

# 10 yr 
ggplot(data = filter(tide, year %in% tenyrs & tideht == '1'),
       mapping = aes(x = year, y = hrs, group = tideht)) +
    geom_line()
  


##### vis: lottia heatmap #####

# size class heatmap for 10 yr report
  # data
heatmap10 <- lim_measure %>%
  # only CABR sites and in 10 yr study period (2010-2019)
  filter(SiteCode %in% cabrsites & 
         SurveyYear %in% c((currentyear - 11):(currentyear - 2)) & 
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

  # plot
  # note to self: second discrete x axis not supported for ggplot2 (check github issues in future to see if solution)
ggplot(data = heatmap10,
       mapping = aes(x = SurveyYear, y = Size_bin, fill = `Mean count`)) + 
  # give tiles black outline
  geom_tile(color = 'black') + 
  facet_wrap(~ZoneName) + 
  scale_x_continuous(breaks = c(min(heatmap10$SurveyYear):max(heatmap10$SurveyYear))) + 
  xlab('Year') +
  ylab(expression(italic('Lottia') ~ 'size (mm)')) +
  # reverse viridis (colorblind friendly palette) colors
  scale_fill_viridis(begin = 1, end = 0) +
  lltheme_heatmap 


ggsave('RI_Plots_10yr_2021/Lottia_Heatmap_10yr.png')

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

ggsave('RI_Plots_Annual_2021/Lottia_Heatmap_5yr.png')

##### vis: lottia density time series ##### 
# overall density summary
density_summary <- lim_density %>%
  filter(SiteCode %in% cabrsites) %>%
  mutate(ZoneName = paste('Zone', substr(SiteName, 9, 12)),
         Panel = if_else(SiteName == 'Cabrillo I', 'A', 
                         if_else(SiteName == 'Cabrillo II', 'B', 'C'))) %>%
  mutate(ZoneName = paste('(', Panel, ') ', ZoneName, sep = '')) %>%
  group_by(ZoneName) %>%
  summarise(mean = mean(Density))


# 10 yr report
  # data
density10 <- lim_density %>% 
  # only CABR sites and in 10 yr study period (2010-2019)
  filter(SiteCode %in% cabrsites & SurveyYear %in% c((currentyear - 11):(currentyear - 2))) %>%
  mutate(ZoneName = paste('Zone', substr(SiteName, 9, 12)),
         Panel = if_else(SiteName == 'Cabrillo I', 'A', 
                         if_else(SiteName == 'Cabrillo II', 'B', 'C'))) %>%
  mutate(ZoneName = paste('(', Panel, ') ', ZoneName, sep = '')) %>%
  # select necessary columns 
  group_by(ZoneName, SurveyYear) %>%
  summarise(`Mean Density` = mean(Density),
             SE = sd(Density)/sqrt(length(unique(PlotNo)))) 

meandensity <- mean(filter(lim_density, SiteCode %in% cabrsites)$Density)

  # plot time series
ggplot(data = density10, 
       mapping = aes(x = SurveyYear, y = `Mean Density`)) + 
  geom_linerange(mapping = aes(x = SurveyYear, 
                              ymin = (`Mean Density` - SE),
                              ymax = (`Mean Density` + SE)),
                 color = 'gray70', size = 1) +
  geom_point(aes(color = ZoneName), size = 2) + 
  geom_line(aes(color = ZoneName), size = 1) + 
  geom_hline(data = density_summary, aes(yintercept = mean), linetype = 'dashed', color = 'black') +
  scale_color_viridis(discrete = TRUE, begin = 0, end = 0.7, name = 'Site') +
  xlab('Year') + 
  ylab(expression(italic('Lottia') ~ 'density (count/m²)')) +
  facet_wrap(~ZoneName) + 
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018)) + 
  scale_y_continuous(breaks = c(5, 10, 15, 20, 25)) +
  lltheme +
  theme(aspect.ratio = 1,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none')

ggsave('RI_Plots_10yr_2021/Lottia_TimeSeries_10yr.png')

# Annual Report: All Years
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
  
ggsave('RI_Plots_Annual_2021/Lottia_TimeSeries_allyrs.png')

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

# data for 10 yr report
size_10 <- lim_measure %>%
  filter(SiteCode %in% cabrsites & 
         SurveyYear %in% tenyrs) %>%
  mutate(ZoneName = paste('Zone', substr(SiteName, 9, 12)),
         Panel = if_else(SiteName == 'Cabrillo I', 'A', 
                         if_else(SiteName == 'Cabrillo II', 'B', 'C'))) %>%
  mutate(ZoneName = paste('(', Panel, ') ', ZoneName, sep = '')) %>%
  group_by(SurveyYear, ZoneName) %>%
  summarise(mean = sum(Size_class*N)/sum(N),
            SE = sd(Size_class*N)/sqrt(sum(N)))

# plot
# plot time series
ggplot(data = size_10, 
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
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018)) + 
  scale_y_continuous(breaks = c(5, 10, 15, 20, 25)) +
  lltheme +
  theme(aspect.ratio = 1,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none')

ggsave('RI_Plots_10yr_2021/Lottia_Size_TimeSeries_10yr.png')

# Annual report (all yrs)
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
  
ggsave('RI_Plots_Annual_2021/Lottia_Size_TimeSeries_allyrs.png')

##### vis/da: linreg - 10 yr lottia density vs SST #####

# align data
sst$SurveyYear <- sst$year

lottia_linreg_data <- left_join(lim_density, sst, by = 'SurveyYear') %>% 
  # only CABR sites and in 10 yr study period (2010-2019)
  filter(SiteCode %in% cabrsites & SurveyYear %in% c((currentyear - 11):(currentyear - 2))) %>%
  # select necessary columns 
  group_by(SiteName, SurveyYear, surface_mean) %>%
  summarise(`Mean Density` = mean(Density))

# models for each site
lottia_lm1 <- lm(`Mean Density` ~ surface_mean, data = filter(lottia_linreg_data, SiteName == 'Cabrillo I'))

lottia_lm2 <- lm(`Mean Density` ~ surface_mean, data = filter(lottia_linreg_data, SiteName == 'Cabrillo II'))

lottia_lm3 <- lm(`Mean Density` ~ surface_mean, data = filter(lottia_linreg_data, SiteName == 'Cabrillo III'))

# incorporate results into dataset
lottia_linreg_data <- lottia_linreg_data %>%
mutate(rsquared = if_else(SiteName == 'Cabrillo I', summary(lottia_lm1)$r.squared,
                  if_else(SiteName == 'Cabrillo II', summary(lottia_lm2)$r.squared,
                            summary(lottia_lm3)$r.squared)),
         pvalue = if_else(SiteName == 'Cabrillo I', summary(lottia_lm1)$coefficients[2,4],
                  if_else(SiteName == 'Cabrillo II', summary(lottia_lm2)$coefficients[2,4],
                                  summary(lottia_lm3)$coefficients[2,4] )))

# plot results
ggplot(data = lottia_linreg_data, mapping = aes(x = surface_mean, y = `Mean Density`, color = SiteName)) + 
  geom_point() + 
  geom_smooth(method = 'lm', se = FALSE) + 
  geom_text(mapping = aes(x = 19, y = 20, label = paste('p =',round(pvalue, digits = 3))), color = 'black') +   
  geom_text(mapping = aes(x = 19, y = 21, label = paste('R² =', round(rsquared, digits = 2))), color = 'black') +
  facet_wrap(~SiteName) + 
  scale_color_viridis(discrete = TRUE, begin = 0, end = 0.7) +
  xlab('Mean annual SST (°C)') + 
  ylab(expression(italic('Lottia') ~ 'density (count/m²)')) + 
  lltheme + 
  theme(legend.position = 'none')
  
ggsave('RI_Plots_10yr_2021/Lottia_SST_Linreg_10yr.png')

remove(lottia_lm1, lottia_lm2, lottia_lm3)
##### vis: transect time series #####

transect10 <- transect %>% 
  # only CABR sites and in 10 yr study period (2010-2019)
  filter(SurveyYear %in% c((currentyear - 11):(currentyear - 2))) %>%
  # select necessary columns 
  group_by(ZoneName, SurveyYear, Zone, SpeciesCode, Scientific_name) %>%
  summarise(mean_cover = mean(pct_cover),
            SE = sd(pct_cover)/sqrt(length(unique(Transect))))

# 10 yr report time series looped across study spp

  # list of spp to loop across
  transect_spp <- c('PHYALL', 'ARTCOR', 'OTHRED', 'EGRMEN')
  
  for(i in 1:length(transect_spp)){
    
    # get grand mean cover (1990-current year, all zones)
    trans_grand_mean <- mean(filter(transect, SpeciesCode == transect_spp[i])$pct_cover)
    speciesname <- filter(transect, SpeciesCode == transect_spp[i])$Scientific_name[1]
    zone <- filter(transect, SpeciesCode == transect_spp[i])$Zone[1]
    
    # plot time series for 10 yr report for each zone
    ggplot(data = filter(transect10, SpeciesCode == transect_spp[i]),
           mapping = aes(x = SurveyYear, y = mean_cover)) + 
      geom_hline(aes(yintercept = trans_grand_mean), linetype = 'dashed', color = 'black') +
      geom_linerange(mapping = aes(x = SurveyYear, 
                                   ymin = (mean_cover - SE),
                                   ymax = (mean_cover + SE)), color = 'gray70', size = 1) + 
      geom_point(aes(color = ZoneName), size = 2) + 
      geom_line(aes(color = ZoneName), size = 1) +
      scale_color_viridis(discrete = TRUE, begin = 0, end = 0.7, name = 'Site') +
      xlab('Year') + 
      ylab('Percent cover') +
      ggtitle(paste(speciesname, 'in', zone, 'transect')) + 
      facet_wrap( ~ ZoneName) + 
      scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018))  +
      lltheme +
      theme(aspect.ratio = 1,
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = 'none')
    
    ggsave(paste('RI_Plots_10yr_2021/Transect_', transect_spp[i], '_10yr.png'))
      
  }

# 10 yr report time series as facet grid
  
  # get grand means (1990 - 2020)
  transect_grandmean <- transect %>%
    group_by(ZoneName, SpeciesCode) %>%
    summarise(grand_mean = mean(pct_cover))
  
transect10 <- left_join(transect10, transect_grandmean)

ggplot(data = transect10,
       mapping = aes(x = SurveyYear, y = mean_cover)) + 
  geom_hline(aes(yintercept = grand_mean), linetype = 'dashed', color = 'black') + 
  geom_linerange(aes(x = SurveyYear, ymin = (mean_cover - SE), ymax = (mean_cover + SE)),
                 color = 'gray70', size = 1) + 
  geom_line(aes(color = ZoneName), size = 1) + 
  geom_point(aes(color = ZoneName), size = 2) + 
  scale_color_viridis(discrete = TRUE, begin = 0, end = 0.7, name = 'Site') +
  xlab('Year') + 
  ylab('Percent cover') +
  facet_grid(Scientific_name ~ ZoneName, scales = 'free_y') + 
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018))  +
  lltheme +
  theme(aspect.ratio = 1,
        legend.position = 'none')

ggsave('RI_Plots_10yr_2021/Transect_FacetGrid_10yr.png', width = 10, height = 10)
  

# repeat facet plot for all years

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
  theme(aspect.ratio = 1,
        legend.position = 'none',
        panel.grid.minor = element_blank())

ggsave('RI_Plots_Annual_2021/Transect_FacetGrid_allyrs.png', width = 10, height = 10, dpi = 300)

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
  theme(aspect.ratio = 1,
        legend.position = 'none',
        panel.grid.minor = element_blank())

ggsave('RI_Plots_Annual_2021/Photoplot_FacetGrid_allyrs.png', width = 10, height = 10, dpi = 300)




  
##### table: timed search #####

timed_search_tbl <- ungroup(timed_search) %>%
  filter(SurveyYear == (currentyear-1)) %>%
  mutate(Zone = substr(ZoneName, 10, 13),
        Species = Scientific_name) %>%
  select(Zone, Species, `Number observed`)

