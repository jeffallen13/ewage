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
ggsave(filename = 'Fig3.png', 
       path = 'output/', device = "png",
       width = 10, height = 6)

# Regional mfx in global model (Figure 4)
plot_regional_mfx(model = ewage_model)

# Save
ggsave(filename = 'Fig4.png', 
       path = 'output/', device = "png",
       width = 9.5, height = 5)


# Regional ----------------------------------------------------------------

# Model
regional_model <- model_by_region(df = micro)

# Plot (Figure 5)
plot_ewage_results(regional_model, by_region = TRUE)

# Save
ggsave(filename = 'Fig5.png', 
       path = 'output/', device = "png",
       width = 10, height = 7)


# Summary Tables ----------------------------------------------------------

# Uptake 
generate_uptake_table(global_model = ewage_model,
                      regional_model = regional_model)

# Usage 
generate_usage_table(global_model = ewage_model,
                     regional_model = regional_model)
