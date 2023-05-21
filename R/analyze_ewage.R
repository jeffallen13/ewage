#-----------------------------------------------------------------------------#
# Pay it forward (digitally)
# Analytical functions
#-----------------------------------------------------------------------------#

#-----------------------------------------------------------------------------#
# Modeling
#-----------------------------------------------------------------------------#


# Model ewage -------------------------------------------------------------

model_ewage <- function(df, region=NULL){
  
  # Remove robustness vars and NAs
  df <- df %>% 
    dplyr::select(-c(debit_dv, mobile_dv, pos_100K, epay_trans_1K,
                     card_payments_pc)) %>% 
    na.omit()
  
  if(!is.null(region)){
    if(!(region %in% unique(as.character(df$region)))){
      stop("Must select a valid region")
    }
    df <- df[df$region == region,]
  }
  

  # Uptake model --------------------------------------------------------
  
  uptake_spec <- paste0(
    "instrument ~ ",
    # Demographics
    "age_g + educ + sex + ",
    # Economics
    "inlf + inc_q + ",
    # Connectivity
    "internetaccess + mobileowner + ",
    # Economic concern
    "worried_age + worried_medical + worried_bills"
    )
  
  # Add regional fixed effects to global model 
  if(is.null(region)){
    uptake_spec <- paste0(uptake_spec, " + region")
  } 
  
  uptake_formula <- formula(uptake_spec)
  
  suppressWarnings(
    uptake <- glm(uptake_formula, family = binomial(link = "probit"),
                  weights = df$wgt, data = df)
    )
  
  df$IMR <- sampleSelection::invMillsRatio(uptake)$IMR1


  # Usage model --------------------------------------------------------

  usage_spec <- paste0(
    # Treatment 
    "epay ~ e_wage + ",
    # Demographics
    "age_g + educ + sex + ",
    # Economics
    "inlf + inc_q + ",
    # Connectivity
    "internetaccess + mobileowner + ",
    # IMR
    "IMR"
  )
  
  # Add regional fixed effects to global model 
  if(is.null(region)){
    usage_spec <- paste0(usage_spec, " + region")
  } 
  
  usage_formula <- formula(usage_spec)
  
  suppressWarnings(
    usage <- glm(usage_formula, 
                 family = binomial(link = "probit"),
                 weights = df[df$instrument == 1,]$wgt, 
                 data = df[df$instrument == 1,])
  )  
  
  usage_vcov <- sandwich::vcovCL(usage, cluster = ~economycode)
  
  usage_cse <- sqrt(diag(usage_vcov))
  
  usage_robust <- lmtest::coeftest(usage, vcov = usage_vcov)
  
  margins_vars <- c("e_wage", "age_g", "educ", "sex", "inlf", "inc_q",
                    "internetaccess", "mobileowner", "IMR")
  
  usage_margins <- margins::margins(usage, 
                                    variables = margins_vars,
                                    vcov = usage_vcov)
  
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
  
  cat_caption <- paste0(
    "Categorical variable baselines: Age (15-29); Education (Primary); ",
    "Income (Poorest 20%)"
  )
  
  appendix_caption <- 
    "IMR effects are captured in the online statistical appendix."
  
  if(by_region){
    caption <- paste0(
      "Model includes Inverse Mills Ratio (not depicted). ",
      appendix_caption, "\n", cat_caption
    )
  } else{
    caption <- paste0(
      "Model includes regional fixed effects and Inverse Mills Ratio ", 
      "(not depicted). ", appendix_caption, "\n", cat_caption
    )
  }
  
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
    theme(plot.caption = element_text(hjust = 0)) +
    labs(
      title = "Marginal effects on likelihood of making digital merchant payments",
      subtitle = "Bands represent 95% confidence intervals",
      caption = caption,
      x = "", y = ""
    )
  
  if(by_region){
    m_plot <- m_plot + 
      facet_wrap(~region) + 
      coord_flip(ylim = region_limits) + 
      theme(axis.text.y = element_text(size = region_text_size))
  } else{
    m_plot <- m_plot + coord_flip(ylim = c(-.2, .2))
  }
  
  return(m_plot)
}

#-----------------------------------------------------------------------------#
# Descriptive analysis
#-----------------------------------------------------------------------------#


# Plot global distribution ------------------------------------------------

plot_global_distribution <- function(df, bin_w = 15){
  
  p1 <- df %>% 
    dplyr::filter(!is.na(epay)) %>% 
    ggplot(aes(x = epay, after_stat(density))) + 
    geom_histogram(binwidth = bin_w) + 
    theme_bw() + 
    labs(x = "Made digital merchant payment (% of adults)",
         y = "Density",
         title = "Global Distribution")
  
  p2 <- df %>% 
    dplyr::filter(!is.na(epay),
                  !is.na(income_group)) %>% 
    ggplot(aes(x = income_group, y = epay)) + 
    geom_boxplot() + 
    theme_bw() + 
    labs(title = "Distribution Across Income Groups",
         x = "", 
         y = "Made digital merchant payment (% of adults)",
         caption = "Source: World Bank Global Findex Database (2021)")
  
  gridExtra::grid.arrange(p1, p2, nrow = 1)
  
}


# Plot relationship -------------------------------------------------------

plot_ewage_dmp <- function(df, smooth = FALSE, by_region = FALSE, 
                           title = ""){
  
  p <- df %>% 
    dplyr::filter(!is.na(epay),
                  !is.na(e_wage)) %>% 
    ggplot(aes(x = e_wage, y = epay)) + 
    geom_point() + 
    theme_bw() + 
    labs(
      title = title,
      caption = "Source: World Bank Global Findex Database (2021)",
      x = "Received wages into an account (% of adults)", 
      y = "Made digital merchant payment (% of adults)"
    )
  
  if(smooth){
    p <- p + geom_smooth(method = "lm")
  }
  
  if(by_region){
    p <- p + facet_wrap(~region, scales = "free")
  }
  
  return(p)
  
}


# Plot regional distributions ---------------------------------------------

plot_regional_distribution <- function(df, bin_w = 15){
  
  df %>% 
    dplyr::select(region, epay) %>% 
    na.omit() %>% 
    ggplot(aes(x = epay, after_stat(density))) + 
    geom_histogram(binwidth = bin_w) + 
    facet_wrap(~region, scales = "free") + 
    theme_bw() + 
    labs(x = "Made digital merchant payment (% of adults)", 
         y = "Density", 
         title = "Figure A.1: Distribution of Digital Payment Usage by Region")
}


# Summarize Sample --------------------------------------------------------

summarize_sample <- function(df){
  
  # Weighted sample
  df_w <- survey::svydesign(ids=~1, data = df, weights = df$wgt)
  
  mean_w <- as.data.frame(
    survey::svymean(~epay + instrument + e_wage + 
                      age_g + educ + sex + 
                      inlf + inc_q + 
                      internetaccess + mobileowner + 
                      worried_age + worried_medical + worried_bills,
                    df_w, na.rm = TRUE))
  
  # Unweighted sample
  df_uw <- suppressWarnings(survey::svydesign(ids=~1, data = df))
  
  mean_uw <- as.data.frame(
    survey::svymean(~epay + instrument + e_wage + 
                      age_g + educ + sex + 
                      inlf + inc_q + 
                      internetaccess + mobileowner + 
                      worried_age + worried_medical + worried_bills,
                    df_uw, na.rm = TRUE))
  
  summary_df <- tibble::tibble(
    Variable = rownames(mean_uw),
    `Raw proportion` = round(mean_uw$mean, 2),
    `Weighted proportion` = round(mean_w$mean, 2)
  )
  
  summary_out <- summary_df %>% 
    dplyr::mutate(Variable = dplyr::case_when(
      Variable == "epay" ~ "Made digital merchant payment",
      Variable == "instrument" ~ "Owns payment instrument",
      Variable == "e_wage" ~ "Received electronic wages",
      Variable == "age_g15-29" ~ "15-29",
      Variable == "age_g30-44" ~ "30-44",
      Variable == "age_g45-59" ~ "45-59",
      Variable == "age_g60 and older" ~ "60 and older",
      Variable == "educprimary" ~ "Primary or less",
      Variable == "educsecondary" ~ "Secondary",
      Variable == "eductertiary" ~ "Tertiary or more",
      Variable == "sexMale" ~ "Male",
      Variable == "sexFemale" ~ "Female",
      Variable == "inlf" ~ "Employed",
      Variable == "inc_qPoorest 20%" ~ "Poorest 20%",
      Variable == "inc_qSecond 20%" ~ "Second 20%",
      Variable == "inc_qMiddle 20%" ~ "Middle 20%",
      Variable == "inc_qFourth 20%" ~ "Fourth 20%",
      Variable == "inc_qRichest 20%" ~ "Richest 20%",
      Variable == "internetaccess" ~ "Internet access",
      Variable == "mobileowner" ~ "Mobile owner",
      Variable == "worried_ageNot at all" ~ "Age: Not at all",
      Variable == "worried_ageSomewhat" ~ "Age: Somewhat",
      Variable == "worried_ageVery" ~ "Age: Very",
      Variable == "worried_medicalNot at all" ~ "Medical: Not at all",
      Variable == "worried_medicalSomewhat" ~ "Medical: Somewhat",
      Variable == "worried_medicalVery" ~ "Medical: Very",
      Variable == "worried_billsNot at all" ~ "Bills: Not at all",
      Variable == "worried_billsSomewhat" ~ "Bills: Somewhat",
      Variable == "worried_billsVery" ~ "Bills: Very",
      TRUE ~ Variable
    ))
  
}

#-----------------------------------------------------------------------------#
# Tables
#-----------------------------------------------------------------------------#


# Uptake Table ------------------------------------------------------------

generate_uptake_table <- function(global_model, regional_model){
  
  uptake_names <- c("Age: 30-44", "Age: 45-59", "Age: 60 and Up",
                    "Education: Secondary", "Education: Tertiary",
                    "Sex: Female", "Employed",
                    "Income: Second 20%", "Income: Middle 20%",
                    "Income: Fourth 20%", "Income: Richest 20%",
                    "Internet Access", "Mobile Owner",
                    "Worried Age: Somewhat", "Worried Age: Very",
                    "Worried Medical: Somewhat", "Worried Medical: Very",
                    "Worried Bills: Somewhat", "Worried Bills: Very")
  
  column_names <- c("Global", "EAP", "ECA", "HI-OECD", "HI-Other", "LAC",
                    "MENA", "SAS", "SSA")
  
  caption <- "Dependent Variable: Electronic Payment Instrument"
  
  # Shorten model names
  # Why? With R > 4.2, stargazer requires shorter model names to avoid error
  m1 <- global_model$uptake
  m2 <- regional_model$`East Asia & Pacific`$uptake
  m3 <- regional_model$`Europe & Central Asia`$uptake
  m4 <- regional_model$`High income: non-OECD`$uptake
  m5 <- regional_model$`High income: OECD`$uptake
  m6 <- regional_model$`Latin America & Caribbean`$uptake
  m7 <- regional_model$`Middle East & North Africa`$uptake
  m8 <- regional_model$`South Asia`$uptake
  m9 <- regional_model$`Sub-Saharan Africa`$uptake
  
  stargazer::stargazer(m1, m2, m3, m4, m5, m6, m7, m8, m9,
                       out = "output/uptake.html",
                       title = "Probit Selection Equations",
                       no.space = TRUE, 
                       align = TRUE,
                       covariate.labels = uptake_names,
                       dep.var.caption = caption,
                       dep.var.labels = "",
                       column.labels = column_names)
}


# Usage Formatting --------------------------------------------------------

format_usage_coefs <- function(model){
  
  margins <- model$usage_margins_summary
  
  names(margins$SE) <- NULL
  
  margins_rev <- margins %>% 
    dplyr::mutate(driver = factor(factor,
                                  levels = c(
                                    "e_wage",
                                    "age_g30-44",
                                    "age_g45-59",
                                    "age_g60 and older",
                                    "educsecondary",
                                    "eductertiary",
                                    "sexFemale",
                                    "inlf",
                                    "inc_qSecond 20%",
                                    "inc_qMiddle 20%",
                                    "inc_qFourth 20%",
                                    "inc_qRichest 20%",
                                    "internetaccess",
                                    "mobileowner",
                                    "IMR"
                                  ))) %>% 
    dplyr::arrange(driver) %>% 
    dplyr::mutate(driver = as.character(driver)) %>% 
    dplyr::select(driver, AME, SE) %>% 
    dplyr::add_row(driver = "(Intercept)",
                   AME = model$usage$coefficients[[1]],
                   SE = model$usage_cse[[1]],
                   .before = 1)
}


# Usage Table -------------------------------------------------------------

generate_usage_table <- function(global_model, regional_model){
  
  models <- list(
    global_model, 
    regional_model$`East Asia & Pacific`,
    regional_model$`Europe & Central Asia`,
    regional_model$`High income: non-OECD`,
    regional_model$`High income: OECD`,
    regional_model$`Latin America & Caribbean`,
    regional_model$`Middle East & North Africa`,
    regional_model$`South Asia`,
    regional_model$`Sub-Saharan Africa`
  )
  
  results <- vector(mode = "list", length = length(models))
  
  for(i in seq_along(results)){
    results[[i]] <- format_usage_coefs(model = models[[i]])
    names(results)[i] <- ifelse(is.null(models[[i]]$region), "Global",
                                models[[i]]$region)
  }
  
  usage_names <- c("Electronic Wages",
                   "Age: 30-44", "Age: 45-59", "Age: 60 and Up",
                   "Education: Secondary", "Education: Tertiary",
                   "Sex: Female", "Employed",
                   "Income: Second 20%", "Income: Middle 20%",
                   "Income: Fourth 20%", "Income: Richest 20%",
                   "Internet Access", "Mobile Owner", "IMR")
  
  column_names <- c("Global", "EAP", "ECA", "HI-OECD", "HI-Other", "LAC",
                    "MENA", "SAS", "SSA")
  
  caption <- "Dependent Variable: Made Digital Merchant Payment"
  
  usage_coefs <- list(
    results$Global$AME,
    results$`East Asia & Pacific`$AME,
    results$`Europe & Central Asia`$AME,
    results$`High income: non-OECD`$AME,
    results$`High income: OECD`$AME,
    results$`Latin America & Caribbean`$AME,
    results$`Middle East & North Africa`$AME,
    results$`South Asia`$AME,
    results$`Sub-Saharan Africa`$AME
  )
  
  usage_se <- list(
    results$Global$SE,
    results$`East Asia & Pacific`$SE,
    results$`Europe & Central Asia`$SE,
    results$`High income: non-OECD`$SE,
    results$`High income: OECD`$SE,
    results$`Latin America & Caribbean`$SE,
    results$`Middle East & North Africa`$SE,
    results$`South Asia`$SE,
    results$`Sub-Saharan Africa`$SE
  )
  
  # Shorten model names
  # Why? With R > 4.2, stargazer requires shorter model names to avoid error
  m1 <- global_model$usage
  m2 <- regional_model$`East Asia & Pacific`$usage
  m3 <- regional_model$`Europe & Central Asia`$usage
  m4 <- regional_model$`High income: non-OECD`$usage
  m5 <- regional_model$`High income: OECD`$usage
  m6 <- regional_model$`Latin America & Caribbean`$usage
  m7 <- regional_model$`Middle East & North Africa`$usage
  m8 <- regional_model$`South Asia`$usage
  m9 <- regional_model$`Sub-Saharan Africa`$usage
  
  stargazer::stargazer(m1, m2, m3, m4, m5, m6, m7, m8, m9,
                       coef = usage_coefs,
                       se = usage_se,
                       out = "output/usage.html",
                       title = "Usage Models",
                       no.space = TRUE, 
                       align = TRUE,
                       covariate.labels = usage_names,
                       dep.var.caption = caption,
                       dep.var.labels = "",
                       column.labels = column_names)
}


#----------------------------------------------------------------------------#
# Robustness checks
#----------------------------------------------------------------------------#

# Conduct robustness check  ------------------------------------------------

check_robustness <- function(df, var = "debit_dv"){
  
  robust <- df[var]
  
  # Remove robustness vars, add back var in focus, remove NAs
  df <- df %>% 
    dplyr::select(-c(debit_dv, mobile_dv, pos_100K, epay_trans_1K,
                     card_payments_pc)) %>% 
    dplyr::bind_cols(robust) %>% 
    na.omit()
  
  
  # Uptake model --------------------------------------------------------
  
  uptake_spec <- paste0(
    "instrument ~ ",
    # Demographics
    "age_g + educ + sex + ",
    # Economics
    "inlf + inc_q + ",
    # Connectivity
    "internetaccess + mobileowner + ",
    # Economic concern
    "worried_age + worried_medical + worried_bills + ",
    # Regional fixed effects
    "region"
  )
  
  if(var %in% c("pos_100K", "epay_trans_1K", "card_payments_pc")){
    uptake_spec <- paste0(uptake_spec, " + ", var)
  }
  
  uptake_formula <- formula(uptake_spec)
  
  suppressWarnings(
    uptake <- glm(uptake_formula, family = binomial(link = "probit"),
                  weights = df$wgt, data = df)
  )
  
  df$IMR <- sampleSelection::invMillsRatio(uptake)$IMR1
  
  
  # Usage model --------------------------------------------------------
  
  usage_spec <- paste0(
    # Treatment 
    "epay ~ e_wage + ",
    # Demographics
    "age_g + educ + sex + ",
    # Economics
    "inlf + inc_q + ",
    # Connectivity
    "internetaccess + mobileowner + ",
    # IMR
    "IMR + ",
    # Regional fixed effects
    "region"
  )
  
  if(var %in% c("pos_100K", "epay_trans_1K", "card_payments_pc")){
    usage_spec <- paste0(usage_spec, " + ", var)
  } else{
    usage_spec <- gsub("epay", var, usage_spec)
  }
  
  usage_formula <- formula(usage_spec)
  
  suppressWarnings(
    usage <- glm(usage_formula, 
                 family = binomial(link = "probit"),
                 weights = df[df$instrument == 1,]$wgt, 
                 data = df[df$instrument == 1,])
  )  
  
  usage_vcov <- sandwich::vcovCL(usage, cluster = ~economycode)
  
  usage_cse <- sqrt(diag(usage_vcov))
  
  usage_robust <- lmtest::coeftest(usage, vcov = usage_vcov)
  
  margins_vars <- c("e_wage", "age_g", "educ", "sex", "inlf", "inc_q",
                    "internetaccess", "mobileowner", "IMR")
  
  if(var %in% c("pos_100K", "epay_trans_1K", "card_payments_pc")){
    margins_vars <- c(margins_vars, var)
  }
  
  usage_margins <- margins::margins(usage, 
                                    variables = margins_vars,
                                    vcov = usage_vcov)
  
  usage_margins_summary <- summary(usage_margins)
  
  list(uptake = uptake,
       usage = usage, 
       usage_vcov = usage_vcov,
       usage_cse = usage_cse,
       usage_robust = usage_robust,
       usage_margins = usage_margins,
       usage_margins_summary = usage_margins_summary)
}
