#-----------------------------------------------------------------------------#
# Pay it Forward (Digitally)
# Modeling analysis
#-----------------------------------------------------------------------------#

library(magrittr)
library(ggplot2)

source('R/analyze_ewage.R')

micro <- readRDS('data/ewage_microdata.rds')


# Global ------------------------------------------------------------------

# Model
ewage_model <- model_ewage(df = micro)

# Plot (Figure 3)
plot_ewage_results(model = ewage_model)

# Save
ggsave(filename = 'global_results.png', 
       path = 'output/', device = "png",
       width = 10, height = 6)


# Regional ----------------------------------------------------------------

# Model
regional_model <- model_by_region(df = micro)

# Plot (Figure 4)
plot_ewage_results(regional_model, by_region = TRUE)

# Save
ggsave(filename = 'regional_results.png', 
       path = 'output/', device = "png",
       width = 10, height = 7)

