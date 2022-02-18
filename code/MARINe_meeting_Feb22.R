#################################################
# Title: MARINe Meeting Figures - all sites
# Purpose: adapted from RI_Report_Figures_Stats_SP21, figures for annual report & 10 yr
# Author: LP
# Created: 10/12/21
# Last edited: 2/16/22
##################################################

# preset folder to save things in
save_folder <- './figs/MARINe_Meeting_Feb22/'
# preset folder to look for data in
data_folder <- './data/MEDN_RI_DB_FA21/'

# site groups
klsites <- c('CARDIFF', 'SCRIPPS', 'SWAM')
lpsites <- c('CAB1', 'CAB2', 'CAB3', 'NAVYN', 'NAVYS')
bajasites <- c('LANI', 'PUMA')

##### packages #####
library(readxl)
library(tidyverse)
library(janitor)
library(ggdark)
library(Hmisc)
library(broom)
library(viridis)
library(lubridate)
library(PNWColors)

##### load data #####

# owl limpet data
lim_density <- read_excel(paste0(data_folder, 'Limpet_density_by_plot_size.xlsx'))
lim_measure <- read_excel(paste0(data_folder, 'Limpet_measurements.xlsx'))

# transect data
transect <- read_excel(paste0(data_folder, 'Line_transect_summary.xlsx'))

# target data
target <- read_excel(paste0(data_folder, 'Photoplot_summary_by_plot.xlsx'))

# timed search
timed_search <- read_excel(paste0(data_folder, 'TimedSearch_plot_counts.xlsx'))

# alignment info for target data 
align <- read_csv('data/crosswalk/TGT_all.csv')
# see RI_Species_Alighment_Feb21.R for methods
# summary: took species lists for 1990 and 2000, applied to all unique spp.

##### presets #####

# lists of commonly filtered-for items
zonelist <- c('CHT', unique(target$Zone)[1:4])
zonenames <- c('Balanus/Chthamalus', 'Tetraclita rubescens', 'Mussel', 'Silvetia compressa', 'Pollicipes polymerus')
targetlist <- c('CHTBAL', 'TETRUB', 'MUSSEL', 'SILCOM', 'POLPOL')


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

lltheme_heatmap <- dark_theme_bw() + theme(text = element_text(size = 12),
                                      # add more space between panels
                                      panel.spacing = unit(1, 'lines'),
                                      # no background to wrap panels
                                      strip.background = element_blank(),
                                      strip.text = element_text(size = 12, hjust = 0),
                                      # panel labels outside x axis labels
                                      strip.placement = 'outside',
                                      # adjust x axis labels
                                      axis.text.y = element_text(size = 11, color = 'white'),
                                      axis.text.x = element_text(size = 11, angle = 45, hjust = 1,
                                                                 color = 'white'),
                                      panel.grid.major = element_blank(),
                                      panel.grid.minor = element_blank(),
                                      legend.position = 'bottom')

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
  distinct() 

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

##### tidy: timed search #####
timed_search <- timed_search %>%
  group_by(SiteName, SurveyYear, SpeciesCode, Scientific_name) %>%
  summarise(`Number observed` = sum(N))

##### vis: lottia density time series ##### 
# overall density summary
density_summary <- lim_density %>%
  group_by(SiteCode, SiteName) %>%
  summarise(mean = mean(Density))


# Annual Report: All Years

lim_density_fn <- function(sitelist, savename, ...) {
  
density <- lim_density %>% 
    filter(SiteCode %in% sitelist) %>%
    group_by(SiteName, SiteCode, SurveyYear) %>%
    summarise(`Mean Density` = mean(Density),
              SE = sd(Density)/sqrt(length(unique(PlotNo)))) 
  
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
  scale_y_continuous(breaks = c(5, 10, 15, 20, 25)) +
  lltheme +
  theme(aspect.ratio = 1,
        legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave(paste(save_folder, 'Lottia_Density_', savename, '.png', sep = ''))
}

lim_density_fn(klsites, 'klsites')
lim_density_fn(lpsites, 'lpsites')
lim_density_fn(bajasites, 'bajasites')

remove(density_summary, lim_density_fn)

##### vis: lottia avg size time series #####
# summary data for grand mean lines
size_summary <- lim_measure %>%
  group_by(SiteCode, SiteName) %>%
  summarise(mean = sum(Size_class*N)/sum(N))

lim_size_fn <- function(sitelist, savename) {
  
# summary data for points by site + year
size_all <- lim_measure %>%
  filter(SiteCode %in% sitelist) %>%
  group_by(SurveyYear, SiteCode, SiteName) %>%
  summarise(mean = sum(Size_class*N)/sum(N),
              SE = sd(Size_class*N)/sqrt(sum(N)))
  
# plot
ggplot(data = filter(size_all, SiteCode %in% c(sitelist)), 
       mapping = aes(x = round(SurveyYear, digits = 0), y = mean)) + 
  geom_hline(data = filter(size_summary, SiteCode %in% c(sitelist)),
             aes(yintercept = mean), linetype = 'dashed', color = 'white') +
  geom_linerange(mapping = aes(x = round(SurveyYear, digits = 0), 
                               ymin = (mean - SE),
                               ymax = (mean + SE)),
                 color = 'gray70') +
  geom_line(aes(color = SiteName)) + 
  geom_point(aes(color = SiteName)) + 
  scale_color_viridis(discrete = TRUE, begin = 0.3, end = 1, name = 'Site') +
  xlab('Year') + 
  ylab(expression(italic('Lottia') ~ 'size (mm)')) +
  facet_wrap(~SiteName) + 
  scale_y_continuous() +
  lltheme +
  theme(aspect.ratio = 1,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none')

ggsave(paste(save_folder, 'Lottia_Size_', savename, '.png', sep = ''))
}

lim_size_fn(klsites, 'klsites')
lim_size_fn(lpsites, 'lpsites')
lim_size_fn(bajasites, 'bajasites')

remove(lim_size_fn, size_summary)

##### vis: limpet heatmap #####

# function to make heatmap 
heatmap_fn <- function(sitelist, savename, ...) {

# filter and prep data
heatmap_all <- lim_measure %>%
  # only CABR sites and in 10 yr study period (2010-2019)
  filter(SiteCode %in% sitelist &
           Size_class >= 15) %>%
  select(SiteName, SurveyYear, Season, PlotNo, Size_class, N) %>%
  # make Size_classes into lowest 5, then make character
  mutate(Size_bin = 5*floor(Size_class/5),
         Season = parse_factor(Season)) %>%
  # group by site, survey year & plot, get total counts in each class
  group_by(SiteName, SurveyYear, Size_bin) %>%
  summarise(`Mean count` = sum(N)/length(unique(PlotNo))) %>%
  # make size bin column categorical with bin descriptor
  mutate(Size_bin = parse_character(paste(Size_bin, '-', (Size_bin + 5))))

# make heatmap plot for all years
ggplot(data = heatmap_all,
       mapping = aes(x = round(SurveyYear, digits = 0), y = Size_bin, fill = `Mean count`)) + 
  # give tiles black outline
  geom_tile(color = 'black') + 
  facet_wrap(~SiteName) + 
  xlab('Sampling year') +
  ylab(expression(italic('Lottia') ~ 'size (mm)')) +
  # reverse viridis (colorblind friendly palette) colors
  scale_fill_viridis(begin = 1, end = 0) +
  coord_equal() + 
  lltheme_heatmap

# save plot
ggsave(paste(save_folder, 'Lottia_Heatmap_', savename, '.png', sep = ''), ...)
}

# run fn for klsites, lpsites & just z3cabr
heatmap_fn(klsites, 'klsites', width = 8)
heatmap_fn(lpsites, 'lpsites', width = 13)
heatmap_fn('CAB3', 'cab3')
heatmap_fn(bajasites, 'bajasites')

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

# plot function
trans_fn <- function(sitelist, savename) {
  
  # summary data
  trans_all <- transect %>%
    filter(SiteCode %in% sitelist) %>%
    mutate(Scientific_name = if_else(Scientific_name == 'Boa kelp', 'Egregia',
                                     if_else(Scientific_name == 'Seagrass', 'Phyllospadix',
                                             if_else(Scientific_name == 'Articulated corallines', 'Art. coralline',
                                                     if_else(Scientific_name == 'Other red algae', 'Other reds',
                                                             Scientific_name))))) %>%
    group_by(SurveyYear, SiteCode, SiteName, Scientific_name) %>%
    summarise(mean = mean(pct_cover),
              SE = sd(pct_cover)/sqrt(length(pct_cover)))
  
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
  
  ggsave(paste(save_folder, 'Transect_', savename, '.png', sep = ''))
}

trans_fn(klsites, 'klsites')
trans_fn(lpsites, 'lpsites')

remove(trans_fn, transect_grandmean)

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
photoplot_fn <- function(sitelist, savename, ...){

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
  scale_x_continuous(...)  +
  lltheme+
  theme(
        legend.position = 'none',
        panel.grid.minor = element_blank())

  ggsave(paste(save_folder, 'Photoplot_', savename, '.png', sep = ''))
}

photoplot_fn(klsites, 'klsites', breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020))
photoplot_fn(lpsites, 'lpsites',breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020))
photoplot_fn(bajasites, 'bajasites', breaks = c(2018, 2019))

remove(photoplot_fn, target_grandmean, target_home)

##### stacked bar plots for CAB mussel plots #####

# get all spp classes how they were in 1990

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

# area plot function for CABR plots

area_fn <- function(zone_name, site_name, spp_code, plot, sort_spp){
  
  # wrangle data
  dataset <- target1990 %>%
    filter(SiteCode %in% lpsites) %>%
    # make pretty label columns, nicer spp names
    mutate(Nice_num = paste('Plot ', Plot_num),
           Scientific_name = case_when(Scientific_name == 'Mussel' ~ 'Mytilus',
                                       Scientific_name =='Bare substrate' ~ 'Rock',
                                       Scientific_name =='Misc animal' ~ 'Invertebrates',
                                       Scientific_name =='Other plants' ~ 'Algae',
                                       Scientific_name =='Tetraclita rubescens' ~ 'Tetraclita',
                                       Scientific_name =='Pollicipes polymerus' ~ 'Pollicipes',
                                       Scientific_name =='Silvetia compressa' ~ 'Silvetia',
                                       TRUE ~ 'Unspecified'))
  
  # step 2 - plotting function, with specified plot number and type
  ggplot(data = filter(dataset, Zone == zone_name & SiteCode == site_name & Plot_num == plot),
         mapping = aes(x = SurveyYear, y = pct_cover, 
                       # make target spp last (bottom bar on plots)
                       fill = fct_relevel(Scientific_name, sort_spp, after = Inf))) +
    # bar stacked by spp group (Scientific_name)
    geom_bar(stat = 'identity', width = 1) + 
    # axis and plot labels
    xlab('Year') + 
    ylab('Percent Cover') + 
    ggtitle(paste(site_name, sort_spp, plot)) +
    # colors
    scale_fill_manual(name = 'Taxon',
                      values = c(pnw_palette('Sailboat', 7, type = 'continuous'), 'gray20')) +
    theme_bw()
  
# save plot
  
  ggsave(paste(save_folder, 'Stackedbar_', site_name, sort_spp, plot, '.png', sep = ''))
}


# make for M5 in Z3
area_fn ('MYT', 'CAB3', 'Mytilus', 5, 'Mytilus')

# make generalized fn for all plots of a type in #'s 1-5

area_fn2 <- function(zone_name, spp_code){
  
  # wrangle data
  dataset <- target1990 %>%
    filter(SiteCode %in% lpsites & Plot_num < 6) %>%
    # make pretty label columns, nicer spp names
    mutate(Nice_num = paste('Plot ', Plot_num),
           Scientific_name = case_when(Scientific_name == 'Mussel' ~ 'Mytilus',
                                       Scientific_name =='Bare substrate' ~ 'Rock',
                                       Scientific_name =='Misc animal' ~ 'Invertebrates',
                                       Scientific_name =='Other plants' ~ 'Algae',
                                       Scientific_name =='Tetraclita rubescens' ~ 'Tetraclita',
                                       Scientific_name =='Pollicipes polymerus' ~ 'Pollicipes',
                                       Scientific_name =='Silvetia compressa' ~ 'Silvetia',
                                       TRUE ~ 'Unspecified'))
  
  # step 2 - plotting function, with specified plot number and type
  ggplot(data = filter(dataset, Zone == zone_name),
         mapping = aes(x = SurveyYear, y = (pct_cover/5), 
                       # make target spp last (bottom bar on plots)
                       fill = fct_relevel(Scientific_name, spp_code, after = Inf))) +
    # bar stacked by spp group (Scientific_name)
    geom_bar(stat = 'identity', width = 1) + 
    # facet by site
    facet_wrap(~SiteCode) + 
    # axis and plot labels
    xlab('Year') + 
    ylab('Percent Cover') +
    # colors
    scale_fill_manual(name = 'Taxon',
                      values = c(pnw_palette('Sailboat', 7, type = 'continuous'), 'gray30')) +
    lltheme
  
  # save plot
  
  ggsave(paste(save_folder, 'Stackedbar_', spp_code, '.png', sep = ''), width = 8)
}


area_fn2('MYT', 'Mytilus')
