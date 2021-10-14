#################################################
# Title: MARINe Meeting Figures - all sites
# Purpose: adapted from RI_Report_Figures_Stats_SP21, figures for annual report & 10 yr
# Author: LP
# Created: 10/12/21
# Last edited: 10/12/21
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

# owl limpet data
lim_density <- read_excel('data/MEDN_RI_DB_SP21/Limpet_density_by_plot_size_20210413.xlsx')
lim_measure <- read_excel('data/MEDN_RI_DB_SP21/Limpet_measurements_20210414.xlsx')

# transect data
transect <- read_excel('data/MEDN_RI_DB_SP21/Line_transect_summary_20210413.xlsx')

# target data
target <- read_excel('data/MEDN_RI_DB_SP21/Photoplot_summary_by_plot_20210414.xlsx')

# timed search
timed_search <- read_excel('data/MEDN_RI_DB_SP21/TimedSearch_plot_counts_20210413.xlsx')

# alignment info for target data 
align <- read_csv('data/crosswalk/TGT_all.csv')
# see RI_Species_Alighment_Feb21.R for methods
# summary: took species lists for 1990 and 2000, applied to all unique spp.

# sea surface temp from SIO Pier
sst <- read_csv('data/abiotic/SIO_TEMP.csv', 
                skip = 26)

# southern oscillation index from NOAA NCEI
soi <- read_excel('data/abiotic/SOI_Data_NOAA_NCEI_1990_2020.xlsx')

##### presets #####
# lists of commonly filtered-for items
zonelist <- c('CHT', unique(target$Zone)[1:4])
zonenames <- c('Balanus/Chthamalus', 'Tetraclita rubescens', 'Mussel', 'Silvetia compressa', 'Pollicipes polymerus')
targetlist <- c('CHTBAL', 'TETRUB', 'MUSSEL', 'SILCOM', 'POLPOL')
klsites <- c('CARDIFF', 'SCRIPPS', 'SWAM')
lpsites <- c('CAB1', 'CAB2', 'CAB3', 'NAVYN', 'NAVYS')

# theme arguments
lltheme <- dark_theme_bw() + theme(text = element_text(size = 12),
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

lltheme_smalltext <- dark_theme_bw() + theme(text = element_text(size = 8),
                                   # add more space between panels
                                   panel.spacing = unit(1, 'lines'),
                                   # no background to wrap panels
                                   strip.background = element_blank(),
                                   strip.text = element_text(size = 8, hjust = 0),
                                   # panel labels outside x axis labels
                                   strip.placement = 'outside',
                                   # adjust x axis labels
                                   axis.text.y = element_text(size = 7),
                                   axis.text.x = element_text(size = 7, angle = 45, hjust = 1))
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
  filter(!is.na(Transect)) %>%
  filter((Transect %in% c(1:2) & SpeciesCode %in% red_turf_spp) |
           (Transect %in% c(3:4) & SpeciesCode == 'PHYALL') |
           (Transect %in% c(5:6) & SpeciesCode == 'EGRMEN')) %>%
  # add transect type column
  mutate(Zone = if_else(Transect %in% 1:2, 'Red algal turf', 
                        if_else(Transect %in% 3:4, 'Phyllospadix', 'Egregia')))


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
  distinct() %>%
  # get sites without lani and puma
  filter(SiteCode != 'LANI' & SiteCode != 'PUMA')

# summarize by 2 methods: all and 1990 (1990-present)

# make list of all possible spp names and codes from target dataset
spplist <- align %>%
  select(SpeciesCode, Scientific_name) %>%
  distinct()

# all categories
target_all <- target %>%
  group_by(SiteCode, SiteName, SurveyYear, Zone, Plot_num, Plot_code, SpeciesCode, Scientific_name) %>%
  summarise(cover = sum(N))


# get total # of pts surveyed per plot in each site/spp/year/season (range = 89-104)
target_summary <- target_all %>%
  group_by(SiteCode, SiteName, SurveyYear, Zone, Plot_num, Plot_code) %>% 
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

##### tidy: mean annual SST #####

sst <- sst %>%
  mutate(year = YEAR) %>%
  # remove extra columns
  select(-c(TIME_PST, TIME_FLAG, ...10:...14, YEAR)) %>%
  # get recent data, no NA's (drops 2% of data)
  filter(year >= 1990 & SURF_TEMP_C != 'NaN' & BOT_TEMP_C != 'NaN') %>%
  # get annual mean
  group_by(year) %>%
  summarise(surface_mean = mean(SURF_TEMP_C),
            surface_SD = sd(SURF_TEMP_C)/sqrt(length(SURF_TEMP_C)))

##### tidy: southern oscillation index (SOI) #####
soi <- soi %>%
  # parse month and date
  mutate(date = lubridate::as_date(paste(year, mont, '1', sep = '-')),
         numeric_date = year + ((mont*8.33)/100)) %>%
  select(year, date, numeric_date, soi)

##### tidy: timed search #####
timed_search <- timed_search %>%
  group_by(SiteName, SurveyYear, SpeciesCode, Scientific_name) %>%
  summarise(`Number observed` = sum(N))

##### vis: SST #####

ggplot(data = filter(sst, year > 1989), 
       mapping = aes(x = year, y = surface_mean, color = surface_mean)) + 
  geom_line(size = 1, color = 'gray48') + 
  geom_linerange(mapping = aes(x = year, 
                               ymin = (surface_mean - surface_SD),
                               ymax = (surface_mean + surface_SD)),
                 size = 1) +
  geom_point(size = 2) + 
  scale_color_viridis(discrete = FALSE) +
  xlab('Year') + 
  ylab('Sea surface temperature (°C)') +
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) + 
  lltheme +
  theme(legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave('figs/MARINe_Meeting_Oct21/SST_TimeSeries.png')

##### vis: SOI #####

ggplot(data = filter(soi, year >= 1990),
       mapping = aes(x = numeric_date, y = soi, color = soi)) + 
  # labels for el nino/la nina
  annotate('text', label = 'El Niño', x = 1990.7, y = -2.75, size = 5, color = 'white') +
  annotate('text', label = 'La Niña', x = 1990.7, y = 2.75, size = 5, color = 'white') +
  geom_hline(aes(yintercept = 0), linetype = 'dashed', color = 'white') + 
  # add in data points connected by line
  geom_line(size = 1) + 
  geom_point(size = 2) + 
  scale_colour_gradient2() + 
  xlab('Year') + 
  ylab('Southern Oscillation Index') +
  coord_cartesian(xlim = c(1990,2020), ylim = c(-3, 3)) + 
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) + 
  lltheme +
  theme(legend.position = 'none')

ggsave('figs/MARINe_Meeting_Oct21/SOI_TimeSeries.png')


##### vis: lottia density time series ##### 
# overall density summary
density_summary <- lim_density %>%
  group_by(SiteCode, SiteName) %>%
  summarise(mean = mean(Density))


# Annual Report: All Years
density <- lim_density %>% 
  filter(SiteCode %in% klsites | SiteCode %in% lpsites) %>%
  group_by(SiteName, SiteCode, SurveyYear) %>%
  summarise(`Mean Density` = mean(Density),
            SE = sd(Density)/sqrt(length(unique(PlotNo)))) 

lim_density_fn <- function(sitelist, savename) {
  
ggplot(data = filter(density, SiteCode %in% c(sitelist)), 
       mapping = aes(x = SurveyYear, y = `Mean Density`)) +
  geom_hline(data = filter(density_summary, SiteCode %in% sitelist)
               , aes(yintercept = mean), linetype = 'dashed', color = 'white') +
  geom_linerange(mapping = aes(x = SurveyYear, 
                               ymin = (`Mean Density` - SE),
                               ymax = (`Mean Density` + SE), color = SiteName),
                 color = 'gray70') +
  geom_line(aes(color = SiteName)) + 
  geom_point(aes(color = SiteName)) + 
  scale_color_viridis(discrete = TRUE, begin = 0.3, end = 1) +
  xlab('Year') + 
  ylab(expression(italic('Lottia') ~ 'density (count/m²)')) +
  facet_wrap(~SiteName) + 
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) + 
  scale_y_continuous(breaks = c(5, 10, 15, 20, 25)) +
  lltheme +
  theme(aspect.ratio = 1,
        legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave(paste('figs/MARINe_Meeting_Oct21/Lottia_Density_', savename, '.png', sep = ''))
}

lim_density_fn(klsites, 'klsites')
lim_density_fn(lpsites, 'lpsites')

remove(density_summary, density, lim_density_fn)

##### vis: lottia avg size time series #####
# summary data for grand mean lines
size_summary <- lim_measure %>%
  group_by(SiteCode, SiteName) %>%
  summarise(mean = sum(Size_class*N)/sum(N))

# summary data for points by site + year
size_all <- lim_measure %>%
  filter(SiteCode %in% klsites | SiteCode %in% lpsites) %>%
  group_by(SurveyYear, SiteCode, SiteName) %>%
  summarise(mean = sum(Size_class*N)/sum(N),
            SE = sd(Size_class*N)/sqrt(sum(N)))

lim_size_fn <- function(sitelist, savename) {
  
# plot
ggplot(data = filter(size_all, SiteCode %in% c(sitelist)), 
       mapping = aes(x = SurveyYear, y = mean)) + 
  geom_hline(data = filter(size_summary, SiteCode %in% c(sitelist)),
             aes(yintercept = mean), linetype = 'dashed', color = 'white') +
  geom_linerange(mapping = aes(x = SurveyYear, 
                               ymin = (mean - SE),
                               ymax = (mean + SE)),
                 color = 'gray70') +
  geom_line(aes(color = SiteName)) + 
  geom_point(aes(color = SiteName)) + 
  scale_color_viridis(discrete = TRUE, begin = 0.3, end = 1, name = 'Site') +
  xlab('Year') + 
  ylab(expression(italic('Lottia') ~ 'size (mm)')) +
  facet_wrap(~SiteName) + 
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) + 
  scale_y_continuous() +
  lltheme +
  theme(aspect.ratio = 1,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none')

ggsave(paste('figs/MARINe_Meeting_Oct21/Lottia_Size_', savename, '.png', sep = ''))
}

lim_size_fn(klsites, 'klsites')
lim_size_fn(lpsites, 'lpsites')

remove(lim_size_fn, size_summary, size_all)

##### vis: transect time series #####

# get grand means
transect_grandmean <- transect %>%
  mutate(Scientific_name = if_else(Scientific_name == 'Boa kelp', 'Egregia',
                           if_else(Scientific_name == 'Seagrass', 'Phyllospadix',
                           if_else(Scientific_name == 'Articulated corallines', 'Art. coralline',
                           if_else(Scientific_name == 'Other red algae', 'Other reds',
                                                           Scientific_name))))) %>%
  group_by(SiteCode, SiteName, Scientific_name) %>%
  summarise(grand_mean = mean(pct_cover))

# summary data for points by site, spp + year
trans_all <- transect %>%
  filter(SiteCode %in% klsites | SiteCode %in% lpsites) %>%
  mutate(Scientific_name = if_else(Scientific_name == 'Boa kelp', 'Egregia',
                           if_else(Scientific_name == 'Seagrass', 'Phyllospadix',
                           if_else(Scientific_name == 'Articulated corallines', 'Art. coralline',
                           if_else(Scientific_name == 'Other red algae', 'Other reds',
                                           Scientific_name))))) %>%
  group_by(SurveyYear, SiteCode, SiteName, Scientific_name) %>%
  summarise(mean = mean(pct_cover),
            SE = sd(pct_cover)/sqrt(length(pct_cover)))

# plot function
trans_fn <- function(sitelist, savename) {
  
  # plot
  ggplot(data = filter(trans_all, SiteCode %in% c(sitelist)), 
         mapping = aes(x = SurveyYear, y = mean)) + 
    geom_hline(data = filter(transect_grandmean, SiteCode %in% c(sitelist)),
               aes(yintercept = grand_mean), linetype = 'dashed', color = 'white') +
    geom_linerange(mapping = aes(x = SurveyYear, 
                                 ymin = (mean - SE),
                                 ymax = (mean + SE)),
                   color = 'gray70') +
    geom_line(aes(color = SiteName)) + 
    geom_point(aes(color = SiteName)) + 
    scale_color_viridis(discrete = TRUE, begin = 0.3, end = 1, name = 'Site') +
    xlab('Year') + 
    ylab('Percent cover') +
    facet_grid(Scientific_name ~ SiteName) + 
    scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020))  +
    lltheme_smalltext +
    theme(
          legend.position = 'none',
          panel.grid.minor = element_blank())
  
  ggsave(paste('figs/MARINe_Meeting_Oct21/Transect_', savename, '.png', sep = ''))
}

trans_fn(klsites, 'klsites')
trans_fn(lpsites, 'lpsites')

remove(trans_fn, transect_grandmean, trans_all)

##### vis: photo plot time series #####

target_home <- target_all %>% 
  mutate(SpeciesCode = if_else(SpeciesCode == 'MYTCAL', 'MUSSEL', SpeciesCode)) %>%
  # filter to only tgt spp in their own plots
  filter((Zone == 'CHT' & SpeciesCode %in% c('TETRUB', 'CHTBAL')) |
           (Zone == 'MYT' & SpeciesCode == 'MUSSEL') | 
           (Zone == 'SIL' & SpeciesCode == 'SILCOM') | 
           (Zone == 'POL' & SpeciesCode == 'POLPOL')) %>%
  # get mean + SE cover for each yr
  group_by(SiteCode, SiteName, SurveyYear, Zone, SpeciesCode, Scientific_name) %>%
  summarise(Cover_mean = mean(cover),
            Cover_SE = sd(cover)/sqrt(length(cover)))

# get grand means and join w/ plot data
target_grandmean <- target_home %>%
  group_by(SiteCode, SiteName, SpeciesCode) %>%
  summarise(grand_mean = mean(Cover_mean))

target_home <- left_join(target_home, target_grandmean,
                         by = c("SiteCode", "SiteName", "SpeciesCode")) %>%
  mutate(Scientific_name = word(Scientific_name, 1),
         Scientific_name = if_else(Scientific_name == 'Mussel', 'Mytilus',
                           if_else(Scientific_name == 'Balanus/Chthamalus', 'Cht/Bal',
                                   Scientific_name)))

# panel plot function
photoplot_fn <- function(sitelist, savename){

ggplot(data = filter(target_home, SiteCode %in% c(sitelist)),
       mapping = aes(x = SurveyYear, y = Cover_mean)) + 
  geom_hline(aes(yintercept = grand_mean), linetype = 'dashed', color = 'white') + 
  geom_linerange(mapping = aes(x = SurveyYear, 
                               ymin = (Cover_mean - Cover_SE),
                               ymax = (Cover_mean + Cover_SE)),
                 color = 'gray70') +
  geom_line(aes(color = SiteName)) + 
  geom_point(aes(color = SiteName)) + 
  scale_color_viridis(discrete = TRUE, begin = 0.3, end = 1, name = 'Site') +
  xlab('Year') + 
  ylab('Percent cover') +
  facet_grid(Scientific_name ~ SiteName) + 
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020))  +
  lltheme+
  theme(
        legend.position = 'none',
        panel.grid.minor = element_blank())

  ggsave(paste('figs/MARINe_Meeting_Oct21/Photoplot_', savename, '.png', sep = ''))
}

photoplot_fn(klsites, 'klsites')
photoplot_fn(lpsites, 'lpsites')

remove(photoplot_fn, target_grandmean, target_home)
