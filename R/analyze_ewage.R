#-----------------------------------------------------------------------------#
# Pay it forward (digitally)
# Analytical functions
#-----------------------------------------------------------------------------#

#-----------------------------------------------------------------------------#
# Modeling
#-----------------------------------------------------------------------------#


# Model ewage -------------------------------------------------------------

model_ewage <- function(df, region=""){
  
  df <- na.omit(df)
  
  if(region != ""){
    if(!(region %in% unique(as.character(df$region)))){
      stop("Must select a valid region")
    }
    df <- df[df$region == region,]
  }
  
  suppressWarnings(
    uptake <- glm(instrument ~
                  # Demographics
                  age_g + educ + sex + 
                  # Economics
                  inlf + inc_q + 
                  # Connectivity
                  internetaccess + mobileowner + 
                  # Economic concern
                  worried_age + worried_medical + worried_bills, 
                family = binomial(link = "probit"),
                weights = df$wgt, data = df)
    )
  
  df$IMR <- sampleSelection::invMillsRatio(uptake)$IMR1

  suppressWarnings(
    usage <- glm(epay ~
                   # Treatment
                   e_wage +
                   # Demographics
                   age_g + educ + sex + 
                   # Economics
                   inlf + inc_q + 
                   # Connectivity
                   internetaccess + mobileowner + 
                   # IMR
                   IMR, 
                 family = binomial(link = "probit"),
                 weights = df[df$instrument == 1,]$wgt, 
                 data = df[df$instrument == 1,])
  )  
  
  usage_vcov <- sandwich::vcovCL(usage, cluster = ~economycode)
  
  usage_cse <- sqrt(diag(usage_vcov))
  
  usage_robust <- lmtest::coeftest(usage, vcov = usage_vcov)
  
  usage_margins <- margins::margins(usage, vcov = usage_vcov)
  
  usage_margins_summary <- summary(usage_margins)
  
  list(uptake = uptake,
       usage = usage, 
       usage_vcov = usage_vcov,
       usage_cse = usage_cse,
       usage_robust = usage_robust,
       usage_margins = usage_margins,
       usage_margins_summary = usage_margins_summary,
       region = region)
  
}


# Model by Region ---------------------------------------------------------

model_by_region <- function(df){
  
  regions <- sort(unique(df$region))
  
  reg_list <- vector(mode = "list", length = length(regions))
  
  for(i in seq_along(regions)){
    reg_list[[i]] <- model_ewage(df = df, region = regions[i])
  }
  
  names(reg_list) <- regions
  
  regional_margins <- data.frame()
  
  for(i in seq_along(regions)){
    margins <- reg_list[[regions[i]]]$usage_margins_summary
    margins$region <- regions[i]
    regional_margins <- rbind(regional_margins, margins)
  }
  
  reg_list$regional_usage_margins <- regional_margins
  
  return(reg_list)
  
}


# Plot E-wage Results ------------------------------------------------------

plot_ewage_results <- function(model,
                               by_region = FALSE,
                               region_limits = c(-0.3, 0.3),
                               region_text_size = 7){
  if(by_region){
    # TODO: catch when using region without regions
    margins <- model$regional_usage_margins
    margins_temp <- margins %>% 
      dplyr::select(factor, region, AME, lower, upper) %>% 
      # Leaving out mobile; throws off scale
      # Insignificant in all regions except OECD, where it has very large effect
      dplyr::filter(factor != "mobileowner")
  } else{
    margins <- model$usage_margins_summary
    margins_temp <- margins %>% 
      dplyr::select(factor, AME, lower, upper)
  }
  
  margins_rev <- margins_temp %>% 
    dplyr::filter(factor != "IMR") %>% 
    tidyr::pivot_longer(c(AME, lower, upper), names_to = "metric") %>% 
    dplyr::mutate(driver = as.factor(factor)) %>% 
    dplyr::mutate(driver = dplyr::recode_factor(
      driver, 
      `mobileowner` = "Mobile Owner",
      `internetaccess` = "Internet Access",
      `inc_qRichest 20%` = "Income: Richest 20%",
      `inc_qFourth 20%` = "Income: Fourth 20%",
      `inc_qMiddle 20%` = "Income: Middle 20%",
      `inc_qSecond 20%` = "Income: Second 20%",
      `inlf` = "Employed",
      `sexFemale` = "Female",
      `eductertiary` = "Education: Tertiary",
      `educsecondary` = "Education: Secondary",
      `age_g60 and older` = "Age: 60 and Up",
      `age_g45-59` = "Age: 45-59",
      `age_g30-44` = "Age: 30-44",
      `e_wage` = "Electronic Wages"
    ))
  
  m_plot <- 
    ggplot(data = margins_rev) + 
    stat_summary(
      mapping = aes(x = driver, y = value),
      fun.min = min, fun.max = max, fun = median
    ) + 
    geom_hline(yintercept = 0, linetype = 2) + 
    theme_bw() + 
    theme(axis.text = element_text(colour = "black")) + 
    labs(
      title = "Marginal effects on digital merchant payment propensity",
      subtitle = "Bands represent 95% confidence intervals",
      caption = "Categorical variable baselines: Age (15-29); Education (Primary); Income (Poorest 20%)",
      x = "", y = ""
    )
  
  if(by_region){
    m_plot <- m_plot + 
      facet_wrap(~region) + 
      coord_flip(ylim = region_limits) + 
      theme(axis.text.y = element_text(size = region_text_size))
  } else{
    m_plot <- m_plot + coord_flip()
  }
  
  return(m_plot)
}

#-----------------------------------------------------------------------------#
# Descriptive analysis
#-----------------------------------------------------------------------------#
