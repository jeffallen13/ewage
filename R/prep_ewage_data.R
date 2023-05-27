#-----------------------------------------------------------------------------#
# Pay it forward (digitally)
# Data preparation functions
#-----------------------------------------------------------------------------#

#-----------------------------------------------------------------------------#
# Microdata Prep
#-----------------------------------------------------------------------------#


# Load Data ---------------------------------------------------------------

load_micro_data <- function(path="data-raw/micro_world.dta"){
  haven::read_dta(path)
}


# Select Focus Vars -------------------------------------------------------

select_focus_vars <- function(df){
  df %>% 
    dplyr::select(economy, economycode, regionwb, wpid_random, wgt,
                  female, age, educ, emp_in, inc_q, 
                  account, saved, borrowed, 
                  fin44a, fin44b, fin44c,
                  fin32, fin37, fin42,
                  receive_wages, receive_transfers, receive_agriculture,
                  mobileowner, internetaccess, 
                  fin4a, fin8a, fin14_1,
                  fin14b, fin14c, 
                  fin2, fin7) %>% 
    dplyr::rename(sex = female,
                  worried_age = fin44a,
                  worried_medical = fin44b, 
                  worried_bills = fin44c,
                  wages = fin32, 
                  transfers = fin37,
                  agriculture = fin42, 
                  store_debit = fin4a, 
                  store_credit = fin8a, 
                  store_mobile = fin14_1,
                  online_purchase = fin14b,
                  online_method = fin14c,
                  debit_card = fin2, 
                  credit_card = fin7)
}


# Format Variables --------------------------------------------------------

format_vars <- function(df, binary_integers = TRUE){
  
  df_rev <- df %>% 
    # Age: convert to integer and create factor
    dplyr::mutate(age = as.integer(age)) %>% 
    dplyr::mutate(age_g = dplyr::case_when(
      age <= 29 ~ "15-29",
      age <= 44 ~ "30-44",
      age <= 59 ~ "45-59",
      TRUE ~ "60 and older"
    )) %>% 
    dplyr::mutate(age_g = factor(age_g)) %>% 
    # First, create alternative depvars for robustness
    dplyr::mutate(debit_dv = dplyr::case_when(
      store_debit == 1 ~ 1, 
      store_debit == 2 | is.na(store_debit) ~ 0,
      TRUE ~ NA
    )) %>% 
    # There are no NAs for store mobile
    dplyr::mutate(mobile_dv = dplyr::case_when(
      store_mobile == 1 ~ 1, 
      store_mobile == 2 ~ 0,
      TRUE ~ NA
    )) %>% 
    # Convert labelled integers to factors
    dplyr::mutate(dplyr::across(where(haven::is.labelled), haven::as_factor)) %>% 
    # Convert "Don't Know" and "Refused to NA
    dplyr::mutate(dplyr::across(where(is.factor),
                                ~dplyr::recode_factor(
                                  .x,
                                  `(Does not apply)` = NA_character_,
                                  `(DK)` = NA_character_,
                                  `(dk)` = NA_character_,
                                  `(ref)` = NA_character_,
                                  `(rf)` = NA_character_,
                                  `dk/ref` = NA_character_))) %>% 
    # Shorten education names
    dplyr::mutate(educ = dplyr::recode_factor(
      educ,
      `completed primary or less` = "primary",
      `secondary` = "secondary",
      `completed tertiary or more` = "tertiary")) %>% 
    # Rename and re-code 'In the Labor Force' (INLF)
    dplyr::mutate(inlf = dplyr::recode_factor(emp_in,
                                              `out of workforce` = "0",
                                              `in workforce` = "1")) %>% 
    # Account, Saved, Borrowed
    dplyr::mutate(dplyr::across(c(account, saved, borrowed),
                                ~dplyr::recode_factor(
                                  .x,
                                  `0` = "0",
                                  `yes` = "1",
                                  `no` = NA_character_))) %>% 
    # Rename and re-code worried vars
    dplyr::mutate(dplyr::across(starts_with("worried_"),
                                ~dplyr::recode_factor(
                                  .x, 
                                  `Not worried at all` = "Not at all",
                                  `Somewhat worried` = "Somewhat",
                                  `Very worried` = "Very"))) %>% 
    # Re-code those with pattern: yes, no, NA
    dplyr::mutate(dplyr::across(c(wages, transfers, agriculture, mobileowner,
                                  store_debit, store_credit, store_mobile,
                                  online_purchase),
                                ~dplyr::recode_factor(.x,
                                                      `no` = "0", 
                                                      `yes` = "1"))) %>% 
    # Capture online epay
    dplyr::mutate(online_epay = dplyr::recode_factor(online_method,
                                                     `In cash` = "0",
                                                     `Pay online` = "1",
                                                     `(both)` = "1")) %>% 
    # Create new vars representing electronic payments received
    dplyr::mutate(dplyr::across(starts_with("receive_"),
                                ~dplyr::recode_factor(
                                  .x,
                                  `received payments in cash only` = "0",
                                  `received payments using other methods` = "0",
                                  `did not receive payments` = "0",
                                  `received payments into an account` = "1"),
                                .names = "{.col}_account")) %>% 
    # Rename electronic payments received vars
    dplyr::rename(e_wage = receive_wages_account,
                  e_transfer = receive_transfers_account,
                  e_agriculture = receive_agriculture_account) %>% 
    # Format internet access
    dplyr::mutate(internetaccess = dplyr::case_when(
      internetaccess == 2 ~ 0,
      internetaccess == 3 ~ NA_real_,
      internetaccess == 4 ~ NA_real_,
      TRUE ~ internetaccess)) %>% 
    dplyr::mutate(internetaccess = factor(internetaccess))
  
  if(binary_integers){
    df_rev %>% 
      dplyr::mutate(dplyr::across(everything(), convert_binary_vars))
  } else{
    return(df_rev)
  }
}


# Binary as Integers ------------------------------------------------------

convert_binary_vars <- function(x){
  
  if(is.factor(x)){
    if(length(levels(x)) == 2 & levels(x)[1] == "0" & levels(x)[2] == "1"){
      x <- as.integer(as.character(x))
    }
  }
  
  return(x)
}


# Format regions ----------------------------------------------------------

format_regions <- function(df){
  
  df %>% 
    dplyr::mutate(region = dplyr::if_else(economycode == "TWN",
                                          "High income: nonOECD",
                                          regionwb)) %>% 
    dplyr::mutate(region = stringr::str_replace_all(region,
                                                    "\\(excluding high income\\)",
                                                    "")) %>% 
    dplyr::mutate(region = stringr::str_trim(region)) %>% 
    dplyr::mutate(region = dplyr::if_else(region == "High income: nonOECD",
                                          "High income: non-OECD", 
                                          region))
}


# Impute INLF -------------------------------------------------------------

# 3,502 observations are missing emp_in / inlf; all but 2 are from China
# Imputation approach: if NA, take received wages/ag payments

impute_inlf <- function(df) {
  
  df %>% 
    dplyr::mutate(wages_ag = dplyr::case_when(
      wages == 1 | agriculture == 1 ~ 1,
      is.na(wages) & is.na(agriculture) ~ NA_real_,
      TRUE ~ 0
    )) %>% 
    dplyr::mutate(inlf = ifelse(is.na(inlf), wages_ag, inlf))
}


# Create DV ---------------------------------------------------------------

# TODO: add functionality to handle factors

add_depvar <- function(df){
  df %>% 
    dplyr::mutate(epay = dplyr::case_when(
      store_debit==1 | store_credit==1 | store_mobile==1 | online_epay==1 ~ 1,
      is.na(store_debit) & is.na(store_credit) & is.na(store_mobile) & 
        is.na(online_epay) ~ NA_real_,
      TRUE ~ 0
    ))
}


# Add instrument ----------------------------------------------------------

# Account includes financial institution account and mobile money account

add_instrument <- function(df){
  df %>% 
    dplyr::mutate(instrument = dplyr::case_when(
      account == 1 | debit_card == 1 | credit_card == 1 ~ 1,
      TRUE ~ 0
    )) %>% 
    dplyr::mutate(instrument = as.integer(instrument))
}


# Add EPA Controls --------------------------------------------------------

add_epa_controls <- function(df, epa_path = "data-raw/ewage_epa.xlsx"){
  
  # EPA vars: 2015 (most recent available)
  epa <- readxl::read_excel(epa_path, 
                            range = readxl::cell_rows(1:435),
                            sheet = "Data", na = "..") %>% 
    dplyr::filter(Time == 2015) %>% 
    dplyr::rename(
      code = `Country Code`,
      pos_100K = `POS terminals per 100,000 adults [GPSS_4]`,
      epay_trans_1K = `Retail cashless transactions per 1,000 adults [GPSS_2]`) %>% 
    dplyr::select(code, pos_100K, epay_trans_1K) %>% 
    dplyr::filter(!is.na(pos_100K) | !is.na(epay_trans_1K)) %>% 
    dplyr::rename(economycode = code)
  
  df %>% dplyr::left_join(epa, by = "economycode")
}


# Custom order ------------------------------------------------------------

customize_order <- function(df){
  
  if(!("epay" %in% colnames(df))){
    stop("Must add dependent variable!")
  }
  
  df %>% 
    dplyr::select(
      # Identifiers
      economy, economycode, wpid_random, wgt,
      # Regions
      region, 
      # Dependent variables
      epay, instrument,
      # Treatment
      e_wage,
      # Demographics
      sex, educ, inc_q, inlf, age_g,
      # Connectivity
      internetaccess, mobileowner,
      # Worried
      starts_with("worried_"),
      # Alternative DVs
      debit_dv, mobile_dv, 
      # EPA controls
      pos_100K, epay_trans_1K
    )
}


# Prep microdata ----------------------------------------------------------

prep_microdata <- function(df){
  
  df %>% 
    select_focus_vars() %>% 
    format_vars() %>% 
    format_regions() %>% 
    impute_inlf() %>% 
    add_depvar() %>% 
    add_instrument() %>% 
    add_epa_controls() %>% 
    customize_order()
}


#-----------------------------------------------------------------------------#
# Findex country data prep
#-----------------------------------------------------------------------------#

prep_country_data <- function(findex_path = "data-raw/ewage_findex.xlsx",
                              groups_path = "data-raw/CLASS.xlsx"){
  
  findex <- readxl::read_excel(findex_path, 
                               range = readxl::cell_rows(1:162),
                               sheet = "Data", na = "..")
  
  findex_rev <- findex %>% 
    dplyr::rename(country = `Country Name`,
                  code = `Country Code`,
                  e_wage = `Received wages: into an account (% age 15+) [fin34a.34b.34e.t]`,
                  epay = `Made a digital merchant payment (% age 15+) [merchant.pay]`) %>% 
    dplyr::select(country, code, e_wage, epay)
  
  groups <- readxl::read_excel(groups_path,
                               sheet = "List of economies",
                               range = "A1:D219")
  
  groups_rev <- groups %>% 
    dplyr::rename(code = Code,
                  income_group = `Income group`,
                  region = Region) %>% 
    dplyr::select(code, region, income_group) %>% 
    dplyr::filter(!is.na(income_group)) %>% 
    dplyr::mutate(income_group = dplyr::recode_factor(
      income_group,
      `Low income` = "Low",
      `Lower middle income` = "Lower Mid",
      `Upper middle income` = "Upper Mid",
      `High income` = "High"
    ))
  
  # Merge
  ewage_findex <- findex_rev %>% 
    dplyr::left_join(groups_rev, by = "code") %>% 
    dplyr::filter(!is.na(region))
}
