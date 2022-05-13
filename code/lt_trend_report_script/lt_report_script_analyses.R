#################################################
# Title: Report Figures & Stats - 1990-2021 Trend Report
# Purpose: adapted from LT report code + clean up code
# Author: LP
# Created: 5/2/22
# Last edited: 5/2/22
##################################################

##### places & themes #####
# 3 color palette
ecopal_3 <- c(cal_palette('chaparral3')[4], cal_palette('bigsur')[5], 
  cal_palette('bigsur')[4])


ecopal_4 <- c(cal_palette('chaparral3')[4], cal_palette('bigsur')[5], 
              cal_palette('bigsur')[4], cal_palette('chaparral3')[1])

# save place
saveplace <- './figs/lt_trend_report_figs/'

# theme
base_theme <- theme(text = element_text(size = 12),
                    # add more space between panels
                    panel.spacing = unit(1, 'lines'),
                    # no background to wrap panels
                    strip.background = element_blank(),
                    strip.text = element_text(size = 12, 
                                              hjust = 0),
                    # panel labels outside x axis labels
                    strip.placement = 'outside',
                    panel.grid = element_blank(),
                    # adjust x axis labels
                    axis.text.y = element_text(size = 12),
                    axis.text.x = element_text(size = 12, 
                                               angle = 45, 
                                               hjust = 1))

light_theme <- theme_bw() + base_theme
dark_theme <- dark_theme_bw() + base_theme

remove(base_theme)

##### packages #####
library(ggdark)     # dark field versions of ggplot2 themes
library(janitor)    # snake case col names and remove duplicates
library(broom)      # glance fn for linregs
library(calecopal)  # color palettes
library(ggrepel)    # flying labels 
library(tidyverse)  # tidyverse packages

##### load data #####

# get list of tidy data files from wrangling output folder
files <- list.files ('D:/LP_Files/RStudio_Files/cabr_ri/data/lt_trend_report_tidy_data/', full.names = TRUE) 

# loop to read csv files from folder and assign them to file name without path and '.csv'
for(i in 1:length(files)) {
assign(gsub('.csv', '', gsub('D:/LP_Files/RStudio_Files/cabr_ri/data/lt_trend_report_tidy_data/', '', files[i])), read_csv(files[i]))
}

# remove files list from environment
remove(files)

##### visitor count (people) data #####

# step 1 - time series/linreg  across zones (has # visitors increased over time?)

# get regression coefficients + bind to people_count data 
people_count <- ungroup(people_count) %>%
  group_by(zone) %>%
  # make 3 sub-groups of data for each zone
  nest() %>%
  # fit linreg to each sub-group + get results summary
  mutate(fit = map(data, ~lm(.$mean_count ~ .$survey_year)),
         summary = map(fit, glance)) %>%
  # un-nest data w/ linreg summary
  unnest(c(data, summary)) %>%
  clean_names() %>%
  select(-c(adj_r_squared, sigma, statistic, df:nobs, fit))

# save stats table
write_csv(select(people_count, zone, r_squared, p_value) %>% distinct(), 
          paste0(saveplace, 'stats_tables/people_linreg.csv'))

# make paneled figure to present results
visit_plot <- ggplot(data = people_count, 
                     mapping = aes(x = survey_year, y = mean_count, color = zone)) + 
  # points + lines for all panels (people ~ year)
  geom_smooth(method = 'lm') + 
  geom_point() + 
  xlab('Year') + 
  ylab('Visitors per survey') + 
  scale_color_manual(values = ecopal_3) + 
  facet_wrap(~zone)

# save light theme version
ggsave(filename = paste0(saveplace, 'visit_linreg_light.png'),
       plot = visit_plot + light_theme + theme(legend.position = 'none'),
       dpi = 300, width = 7, height = 5)

# save dark theme version
ggsave(filename = paste0(saveplace, 'visit_linreg_dark.png'),
       plot = visit_plot + dark_theme + theme(legend.position = 'none'),
       dpi = 300, width = 7, height = 5)

remove(visit_plot)

# step 2 - pie chart of magnitude of visitation across zones

# get % of all visitors observed in each zone
donut_data <- people_count %>%
  group_by(zone) %>%
  summarize(n = sum(mean_count)) %>%
  ungroup() %>%
  # get % of visitors
  mutate(pct = round((n/sum(n)*100), digits = 0),
         # calculate label position
         lab_place = cumsum(pct) - 0.5*pct)

# make donut chart
donut_plot <- ggplot(data = donut_data,
       mapping = aes(x = '', y = pct, fill = zone)) +
  geom_bar(stat = 'identity') + 
  geom_text(mapping = aes(y = pct, label = paste0(zone, ' - ', pct, '%')), 
            nudge_x = 0.9, 
            nudge_y = -1, 
            vjust = 2.7, 
            hjust = 0.4,
            size = 3, color = 'black') + 
  scale_fill_manual(values = ecopal_3) + 
  coord_polar(theta = 'y') + 
  theme_void() + 
  theme(legend.position = 'none')

ggsave(filename = paste0(saveplace, 'visit_pie.png'),
       plot = donut_plot, dpi = 300,
       width = 3, height = 3)

remove(donut_plot, donut_data)

##### linear trends - photoplot & transect #####

# make plot w/ linreg function

linreg_plot_fn <- function(dataset, savename, ...){

# attach linreg results to data  
linreg_data <- dataset %>%
  group_by(zone, taxa_code, scientific_name) %>%
  # make  sub-groups of data for each zone and target spp (scientific_name)
  nest() %>%
  # fit linreg to each sub-group + get results summary
  mutate(fit = map(data, ~lm(.$pct_cover ~ .$survey_year)),
         summary = map(fit, glance)) %>%
  # un-nest data w/ linreg summary
  unnest(c(data, summary)) %>%
  clean_names() %>%
  select(-c(adj_r_squared, sigma, statistic, df:nobs, fit))

# make paneled figure to present results
linreg_plot <- ggplot(data = linreg_data, 
                      mapping = aes(x = survey_year, y = pct_cover, 
                                    color = zone)) + 
  # points + lines for all panels (people ~ year)
  geom_point() + 
  geom_smooth(data = filter(linreg_data, p_value < 0.05),
              mapping = aes(x = survey_year, y = pct_cover,
                            color = zone), method = 'lm') + 
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100)) + 
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  xlab('Year') + 
  ylab('Abundance (% cover)') + 

  scale_color_manual(values = ecopal_3) + 
  facet_grid(scientific_name ~ zone) +
  coord_cartesian(ylim = c(0,115))

# save light theme version

ggsave(plot = linreg_plot + 
              # p-values + r2 values in all panels
              geom_text(data = filter(linreg_data, p_value < 0.05),
                   mapping = aes(x = 1990, y = 110), 
                   label = paste('p = ', 
                                 scales::pvalue(filter(linreg_data, p_value < 0.05)$p_value,
                                                accuracy = 0.01), 
                                 ', R2 = ', 
                                 round(filter(linreg_data, p_value < 0.05)$r_squared, 
                                       digits = 2),
                                 sep = ''), color = 'black', size = 2.5, hjust = 0) + 
              light_theme + 
              theme(legend.position = 'none', 
                    strip.text.y = element_text(face = 'italic')),
       filename = paste0(saveplace, savename , '_linreg_light.png'), 
       width = 7, height = 7)

# save dark theme version
ggsave(plot = linreg_plot + 
  # p-values + r2 values in all panels
  geom_text(data = filter(linreg_data, p_value < 0.05),
            mapping = aes(x = 1990, y = 110), 
            label = paste('p = ', 
                          scales::pvalue(filter(linreg_data, p_value < 0.05)$p_value,
                                         accuracy = 0.01), 
                          ', R? = ', 
                          round(filter(linreg_data, p_value < 0.05)$r_squared, 
                                digits = 2),
                          sep = ''), color = 'white', size = 2.5, hjust = 0) + 
  dark_theme + 
  theme(legend.position = 'none', strip.text.y = element_text(face = 'italic')),
filename = paste0(saveplace, savename , '_linreg_dark.png'), 
                  width = 7, height = 7)

}

# target species in home plots
target_home <- target_90 %>%
  filter(type == substr(taxa_code, 1,3)) %>%
  # shorten taxa names
  mutate(scientific_name = if_else(scientific_name == 'Balanus/Chthamalus', 'Cht/Bal',
                                   word(scientific_name)))

# run for photoplot data
linreg_plot_fn(target_home, 'target')

# tidy transect data 
transect_home <- transect %>%
  # filter for egregia and phyllospadix (1st 3 of taxa code match first 3 of  transect type)
  filter(tolower(substr(type, 1, 3)) == tolower(substr(taxa_code, 1, 3)) |
        # filter for ARTCOR and OTHRED in red algae plots
         (type == 'Red algae' & taxa_code %in% c('ARTCOR', 'OTHRED'))) %>%
  #shorten taxa names
  mutate(scientific_name = 
         if_else(scientific_name %in% c('Articulated corallines', 'Other red algae'),
                 case_when(scientific_name == 'Articulated corallines' ~ 'Corallina*',
                           scientific_name == 'Other red algae' ~ 'Other red*'),
                 word(scientific_name))) %>%
  # arrange so that artcor and othred are adjacent
  mutate(scientific_name = factor(scientific_name, levels = c('Other red*', 'Corallina*', 'Egregia', 'Phyllospadix')))

#run function for transect data
linreg_plot_fn(transect_home, 'transect', group = type)

# remove excess datasets 
remove(target_home, transect_home)

##### limpet size & density linreg #####





