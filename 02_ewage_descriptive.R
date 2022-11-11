#-----------------------------------------------------------------------------#
# Pay it Forward (Digitally)
# Descriptive analysis
#-----------------------------------------------------------------------------#

library(magrittr)
library(ggplot2)

source('R/analyze_ewage.R')


# Global distribution -----------------------------------------------------

country <- readRDS('data/findex_country.rds')

median(country$epay, na.rm = TRUE)

# Figure 1: 
plot_global_distribution(df = country)
# Manual save (1000 X 400)


# Relationship of interest ------------------------------------------------

# Figure 2
plot_ewage_dmp(df = country)

ggsave(filename = 'Fig2.png', 
       path = 'output/', device = "png",
       width = 6, height = 4)


# Sample characteristics --------------------------------------------------

micro <- readRDS('data/ewage_microdata.rds')

# Number of countries
length(unique(micro$economy)) # 123

# Number of observations
nrow(micro) # 127,854

# Table 1
sample_characteristics <- summarize_sample(df = micro)

write.csv(sample_characteristics, 'output/sample_characteristics.csv',
          row.names = FALSE)

# Table 2: Number of economies by region
micro %>% 
  dplyr::group_by(region) %>% 
  dplyr::summarize(countries = length(unique(economy)))
