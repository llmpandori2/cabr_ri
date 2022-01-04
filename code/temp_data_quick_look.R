#################################################
# title: quick look at temp loggers
# purpose: 
# author: LP
# created: 12/16/21
# last edited: 12/16/21
##################################################
##### packages #####

library(janitor)    # clean up data
library(ggdark)     # dark themes
library(viridis)
library(tidyverse)  # all things tidy

dark_theme <- dark_theme_bw() + theme(text = element_text(size = 12),
                                      # add more space between panels
                                      panel.spacing = unit(1, 'lines'),
                                      # no background to wrap panels
                                      strip.background = element_blank(),
                                      strip.text = element_text(size = 12, 
                                                                hjust = 0),
                                      # panel labels outside x axis labels
                                      strip.placement = 'outside',
                                      # adjust x axis labels
                                      axis.text.y = element_text(size = 12),
                                      axis.text.x = element_text(size = 12, 
                                                                 angle = 45, 
                                                                 hjust = 1))

#### load and tidy data #####
# prelim tidy done by hand for ease of use
# figure out map later :(

temp_data <- list.files(path = './data/intertidal_temp_loggers/',
                        pattern = '*.csv') %>%
  map(~ read_csv(file.path('./data/intertidal_temp_loggers/', .))) %>%
  reduce(rbind)

#### visualize ####

ggplot(data = temp_data,
      mapping = aes(x = datetime, y = temp, group = site, color = site)) + 
  geom_point() + 
  xlab('Year-month') + 
  ylab('Intertidal temp. (°C)') +
  ggtitle('TidbiT Preliminary Data') + 
  scale_color_viridis() + 
  dark_theme
  

# save plot (theme in save so it's easier to save light version if desired)
ggsave(filename = './figures/intertidal_temp.png',
       width = 7, height = 5)