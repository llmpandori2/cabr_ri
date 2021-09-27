#################################################
# Title: cleaning script - MEDN RI Data
# Purpose: clean up data for figs in presentation for MEDN meeting
# Author: LP
# Last edited: 1/20/2021
##################################################

##### load packages and data #####
library(readxl)
library(tidyverse)
library(inauguration)
library(broom)

###### photoplot setup #####
trans <- read_excel('Raw Data/qsummarizer_PHY_bytransect.xlsx')

  # limit to CABR, assign target to #s
  trans <- trans %>%
    # just CABR sites
    filter(SiteCode %in% c('CAB1', 'CAB2', 'CAB3')) %>%
    # get sites that were surveyed 
    filter(Was_surveyed == 'surveyed') %>%
    # make % cover actually % (multiply by 100)
    mutate(Pct_cover = CoverPct*100) %>%
    # designate target spp for each transect using 6-letter code
    mutate(Target = if_else(Transect %in% 1:2, 'Red Algal Turf',
                    if_else(Transect %in% 3:4, 'Phyllospadix',
                    if_else(Transect %in% 5:6, 'Egregia', 'NA'))))
  
##### timeline and variance plot #####
  # limit to target spp for timeline plot
  trans_target <- trans %>%
    filter(
  (Target == 'Red Algal Turf' & SpeciesCode %in% c('OTHRED', 'ARTCOR', 'CRUCOR')) |
      (Target == 'Phyllospadix' & SpeciesCode %in% c('PHYOVE', 'PHYUND')) |
      (Target == 'Egregia' & SpeciesCode == 'EGRMEN')) %>%
  group_by(SiteCode, SiteName, SurveyYear, Target) %>%
    summarize(avg_cover = mean(Pct_cover),
              sd_cover = sd(Pct_cover)) %>%
    rename(`Site Name` = SiteName)
  
  # make timeline plot for one spp (Phyllospadix)
  ggplot(data = filter(trans_target, Target == 'Phyllospadix'), 
         mapping = aes(x = SurveyYear, y = avg_cover, 
                       group = `Site Name`, color = `Site Name`)) +
    geom_line(size = 1.5) + 
    # colors
    scale_color_manual(values = inauguration('inauguration_2021',3)) +
    # axis labels
    xlab('Survey Year') +
    ylab('Percent Cover') + 
    ggtitle('Phyllospadix time series') +
    # limits
    # theme arguments 
    theme_bw() +
    theme(text = element_text(size = 13, color = 'black'),
          axis.text.y = element_text(size = 13, color = 'black'),
          axis.text.x = element_text(size = 13, color = 'black'))
  
# compound plot with variance for 3 sites across targets
  ggplot(data = filter(trans_target, Target == 'Phyllospadix'),
         mapping = aes(x = `Site Name`, y = avg_cover)) +
    geom_boxplot(aes(fill = `Site Name`)) +
    geom_point(data = filter(trans_target, Target == 'Phyllospadix' & SurveyYear %in% c(2019)), 
               mapping = aes(x = `Site Name`, y = avg_cover),
               size = 3, shape = 8) +
    scale_fill_manual(values = inauguration('inauguration_2021', 3)) +
    # axes
    ylab('Percent Cover') + 
    xlab('Site') + 
    ggtitle('Phyllospadix') +
    # theme arguments 
    theme_bw() +
    theme(text = element_text(size = 12),
          # no background to wrap panels
          strip.background = element_blank(),
          strip.text = element_text(size = 13, color = 'black', face = 'italic'),
          # panel labels outside x axis labels
          strip.placement = 'outside',
          # tilt and adjust position of x axis labels
          axis.text.y = element_text(size = 11, color = 'black'),
          axis.text.x = element_text(size = 11, color = 'black'),
          # no legends
          legend.position = 'none',
          # make caption serif text to match journal style
          plot.caption = element_text(family = 'serif', 
                                      size = 12, color = 'black', hjust = 0))

##### phyllospadix PCA #####
# choose phyllospadix plot for pca (how do we vis changes in so many spp over time along a transect?)
  
  # there are 18 items in this dataset
  phyl <- trans %>% 
          filter(Target == 'Phyllospadix') %>%
          # select columns of interest
          select(SiteName, SurveyYear, Transect, SpeciesCode, Pct_cover) %>%
          # make into wide format
            # year and site as rows
            # speciescodes as columns
            # if seasonal data, take avg 
          pivot_wider(names_from = SpeciesCode, values_from = Pct_cover,
                      values_fn = mean, values_fill = 0)

  # PCA (tutorial: https://clauswilke.com/blog/2020/09/07/pca-tidyverse-style/)
  
    # fit pca
    pca_fit <- phyl %>%
        select(EGRMEN:TAR) %>%
        prcomp(scale = TRUE)
  
    # combine pc coordinates w original data
    pca_fit %>% augment(phyl) %>%
      ggplot(aes(.fittedPC1, .fittedPC2, color = SiteName)) + 
      geom_point(size = 1.5) 
    
    # get rotation matrix
    pca_fit %>% tidy(matrix= 'rotation')
    
    arrow_style <- arrow(
      angle = 20, ends = "first", type = "closed", length = grid::unit(8, "pt")
    )
  
    pca_fit %>%
      tidy(matrix = "rotation") %>%
      pivot_wider(names_from = "PC", names_prefix = "PC", values_from = "value") %>%
      ggplot(aes(PC1, PC2)) +
      geom_segment(xend = 0, yend = 0, arrow = arrow_style) +
      geom_text(aes(label = column),hjust = 1, nudge_x = -0.02) +
      geom_point(data = pca_fit %>% augment(phyl), 
                 mapping = aes(.fittedPC1, .fittedPC2, color = SiteName), size = 3) +
      scale_color_manual(values = inauguration('inauguration_2021', 3)) +
    # axes
    ylab('PC2 (9%)') + 
      xlab('PC1 (22%)') + 
      ggtitle('Phyllospadix transect communities') +
      # theme arguments 
      theme_bw() +
      theme(text = element_text(size = 12),
            # no background to wrap panels
            strip.background = element_blank(),
            strip.text = element_text(size = 13, color = 'black', face = 'italic'),
            # panel labels outside x axis labels
            strip.placement = 'outside',
            # tilt and adjust position of x axis labels
            axis.text.y = element_text(size = 11, color = 'black'),
            axis.text.x = element_text(size = 11, color = 'black'),
            # make caption serif text to match journal style
            plot.caption = element_text(family = 'serif', 
                                        size = 12, color = 'black', hjust = 0))
    
    
    # get eigenvalues
    pca_fit %>% tidy(matrix = 'eigenvalues')
      

###### read photoplot data #####
photo <- read_excel('Raw Data/qsummarizer_TGT_SppN_rawdata.xlsx')

  # limit to relevant data (species codes that match spp captured in target plot)
  photo_target <- photo %>%
    # just CABR site
    filter(SiteCode %in% c('CAB1', 'CAB2', 'CAB3')) %>%
    # make // spp code and zone
    mutate(Zone2 = if_else(Zone == 'CHT', 'CHTBAL', 
                   if_else(Zone == 'MYT', 'MYTCAL',
                   if_else(Zone == 'SIL', 'SILCOM',
                   if_else(Zone == 'POL', 'POLPOL', 'NA'))))) %>%
    # filter for only matches
    filter(Zone2 == SpeciesCode) %>%
    # remove extra columns
    select(-c(Zone2)) %>%
    # summarize by spp and year
    group_by(SiteCode, SiteName, SurveyYear, Zone, SpeciesCode, Scientific_name) %>%
    summarize(avg_cover = mean(N),
              sd_cover = sd(N)) %>%
    rename(Target = Zone) %>%
    rename(`Zone` = SiteName) %>%
    # put in common names
    mutate(Name = if_else(Target == 'CHT', 'Acorn Barnacle',
                  if_else(Target == 'MYT', 'California Mussel',
                  if_else(Target == 'SIL', 'Golden Rockweed',
                  if_else(Target == 'POL', 'Goose Barnacle', 'NA')))))
  
  # Old plot target spp example - acorn barnacle 
  
  # framework
  ggplot(data = filter(photo_target, Target %in% c('POL', 'SIL')), 
        mapping = aes(x = SurveyYear, y = avg_cover, 
                   group = Zone, color = Zone)) +
        geom_line(size = 1.5) + 
        facet_wrap(~ Name) + 
        # colors
        scale_color_manual(values = inauguration('inauguration_2021',3)) +
        # axis labels
        xlab('Survey Year') +
        ylab('Percent Cover') +
        # limits
        coord_cartesian(ylim = c(0, 100)) +
        # theme arguments 
        theme_bw() +
        theme(text = element_text(size = 12),
          # no background to wrap panels
          strip.background = element_blank(),
          strip.text = element_text(size = 13, color = 'black', face = 'italic'),
          # panel labels outside x axis labels
          strip.placement = 'outside',
          # tilt and adjust position of x axis labels
          axis.text.y = element_text(size = 11, color = 'black'),
          axis.text.x = element_text(size = 11, color = 'black'),
          # no legends
          legend.position = 'none',
          # make caption serif text to match journal style
          plot.caption = element_text(family = 'serif', 
                                      size = 12, color = 'black', hjust = 0))
  
  # make new plot w/ 2 highlights
  ggplot(data = filter(photo_target, Target %in% c('POL', 'SIL')),
         mapping = aes(y = Zone, x = avg_cover)) +
           geom_boxplot(aes(fill = Zone)) +
           geom_point(data =
          filter(photo_target, Target %in% c('POL', 'SIL') & SurveyYear == 2017), 
                      mapping = aes(y = Zone, x = avg_cover),
                                    size = 2.5, shape = 8) +
           facet_wrap(~Name) + 
           scale_fill_manual(values = inauguration('inauguration_2021', 3)) +
      # axes
      xlab('Percent Cover') + 
      # theme arguments 
      theme_bw() +
      theme(text = element_text(size = 12),
          # no background to wrap panels
          strip.background = element_blank(),
          strip.text = element_text(size = 13, color = 'black', face = 'italic'),
          # panel labels outside x axis labels
          strip.placement = 'outside',
          # tilt and adjust position of x axis labels
          axis.text.y = element_text(size = 11, color = 'black'),
          axis.text.x = element_text(size = 11, color = 'black'),
          # no legends
          legend.position = 'none',
          # make caption serif text to match journal style
          plot.caption = element_text(family = 'serif', 
                                      size = 12, color = 'black', hjust = 0))

##### silvetia plot PCA #####
  # quick limiting - only silvetia data
  
  sil <- photo %>%
    # just CABR site
    filter(SiteCode %in% c('CAB1', 'CAB2', 'CAB3')) %>%
    # only silvetia plots
    filter(Zone == 'SIL') %>%
    # select columns of interest
    select(SiteName, SurveyYear, Plot_num, SpeciesCode, N) %>%
    rename(Pct_cover = N) %>%
    pivot_wider(names_from = SpeciesCode, values_from = Pct_cover, 
                 values_fn = mean, values_fill = 0) %>%
    # remove columns with all 0's
    select(where(~ any(. != 0)))

    
  
  # PCA (tutorial: https://clauswilke.com/blog/2020/09/07/pca-tidyverse-style/)
  
  # fit pca
  pca_fit <- sil %>%
    select(BARESUB:OSMSPP) %>%
    prcomp(scale = TRUE)
  
  # combine pc coordinates w original data
  pca_fit %>% augment(sil) %>%
    ggplot(aes(.fittedPC1, .fittedPC2, color = SiteName)) + 
    geom_point(size = 1.5) 
  
  # get rotation matrix
  pca_fit %>% tidy(matrix= 'rotation')
  
  arrow_style <- arrow(
    angle = 20, ends = "first", type = "closed", length = grid::unit(8, "pt")
  )
  
  pca_fit %>%
    tidy(matrix = "rotation") %>%
    pivot_wider(names_from = "PC", names_prefix = "PC", values_from = "value") %>%
    ggplot(aes(PC1, PC2)) +
    geom_segment(xend = 0, yend = 0, arrow = arrow_style) +
    geom_text(aes(label = column),hjust = 1, nudge_x = -0.02) +
    geom_point(data = pca_fit %>% augment(sil), 
               mapping = aes(.fittedPC1, .fittedPC2, color = SiteName), size = 3) +
    scale_color_manual(values = inauguration('inauguration_2021', 3)) +
    # axes
    ylab('PC2 (9%)') + 
    xlab('PC1 (22%)') + 
    ggtitle('Phyllospadix transect communities') +
    # theme arguments 
    theme_bw() +
    theme(text = element_text(size = 12),
          # no background to wrap panels
          strip.background = element_blank(),
          strip.text = element_text(size = 13, color = 'black', face = 'italic'),
          # panel labels outside x axis labels
          strip.placement = 'outside',
          # tilt and adjust position of x axis labels
          axis.text.y = element_text(size = 11, color = 'black'),
          axis.text.x = element_text(size = 11, color = 'black'),
          # make caption serif text to match journal style
          plot.caption = element_text(family = 'serif', 
                                      size = 12, color = 'black', hjust = 0))
  
  
  # get eigenvalues
  pca_fit %>% tidy(matrix = 'eigenvalues')
  
  # read lottia data
lottia <- read_excel('Raw Data/qsummarizer_LIM_measure.xlsx')




