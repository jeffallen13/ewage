#-----------------------------------------------------------------------------#
# Pay it Forward (Digitally)
# Robustness checks
#-----------------------------------------------------------------------------#

library(magrittr)

source('R/analyze_ewage.R')

micro <- readRDS('data/ewage_microdata.rds')


# Alternative DVs ---------------------------------------------------------

# Used a debit card to make a merchant payment 

alt1 <- check_robustness(df = micro, var = "debit_dv")

alt1$usage_robust

alt1$usage_margins_summary

# Used a mobile phone to make a merchant payment 

alt2 <- check_robustness(df = micro, var = "mobile_dv")

alt2$usage_robust

alt2$usage_margins_summary


# EPA Controls ------------------------------------------------------------

# POS terminals 

epa1 <- check_robustness(df = micro, var = "pos_100K")

epa1$usage_robust

epa1$usage_margins_summary

# Noncash transactions

epa2 <- check_robustness(df = micro, var = "epay_trans_1K")

epa2$usage_robust

epa2$usage_margins_summary

