#-----------------------------------------------------------------------------#
# Pay it forward (digitally)
# Prepare data for analysis
#-----------------------------------------------------------------------------#

library(magrittr)

source('R/prep_ewage_data.R')


# Findex Microdata --------------------------------------------------------

micro <- load_micro_data() %>% 
  prep_microdata()

saveRDS(micro, "data/ewage_microdata.rds")


# Findex Country Data -----------------------------------------------------

findex_country <- prep_country_data()

saveRDS(findex_country, "data/findex_country.rds")
