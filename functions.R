# Hardcoded functions to gather and format PUMS tables
library(psrccensus)
library(tidyverse)
library(tidycensus)

# Available:
# 2021 1 year data
# 2020 5 year data

# Not available:
# 2020 1 year data
# 2022 1 year data

create_modsect_pums_table <- function(span, dyear) {
  
  dis_modsect_pums <- get_psrc_pums(span = span,                                # Denoting ACS 5-year estimates; 1-year also available
                                    dyear = dyear,                              # Last data year of span
                                    level = "p",                                # Unit of analysis == household ("p" used for person)
                                    vars = c("LUM_JOBSECTOR",                   # Modeling Sectors
                                             "DIS"                              # Disability recode
                                    ))   
  
  dis_modsect_overview <- psrc_pums_count(dis_modsect_pums, 
                                          group_vars = c("LUM_JOBSECTOR",       # Modeling Sectors
                                                         "DIS"                  # Disability recode
                                          )) 
  
  dis_modsect_overview02 <- dis_modsect_overview %>%
    filter(DIS != 'Total') %>%
    group_by(DATA_YEAR, COUNTY, LUM_JOBSECTOR, DIS) %>%
    summarise(count = sum(count), count_moe = moe_sum(estimate = count, moe = count_moe)) %>%
    mutate(se = count_moe/1.645) %>% 
    mutate(cv = (se/count)) 
  
  dis_modsect_denom <- dis_modsect_overview %>%
    filter(LUM_JOBSECTOR == 'Total') %>%
    select(COUNTY, count_denom = count, moe_denom = count_moe)
  
  dis_modsect_overview03 <- dis_modsect_overview02 %>%
    left_join(dis_modsect_denom, by = 'COUNTY') %>%
    group_by(DATA_YEAR, COUNTY, LUM_JOBSECTOR, DIS) %>%
    summarise(count, count_moe, cv, share = count/count_denom, share_moe = moe_prop(count, count_denom, count_moe, moe_denom))
  
  dis_modsect_overview_fmt <- dis_modsect_overview03 %>%
    select(DATA_YEAR, COUNTY, LUM_JOBSECTOR, DIS, count, count_moe, cv,share, share_moe)
}

# test <- create_modsect_pums_table(span = 1, dyear = 2021)
# test2 <- create_modsect_pums_table(span = 5, dyear = 2020)

create_covempsect_pums_table <- function(span, dyear) {
  
  pums <- get_psrc_pums(span = span,                                            # Denoting ACS 5-year estimates; 1-year also available
                        dyear = dyear,                                          # Last data year of span
                        level = "p",                                            # Unit of analysis == household ("p" used for person)
                        vars = c("STANDARD_JOBSECTOR",                          # Cov Emp Sectors
                                 "DIS"                                          # Disability recode
                        ))   
  
  df <- psrc_pums_count(pums, 
                        group_vars = c("STANDARD_JOBSECTOR",       
                                       "DIS"                  
                        )) %>% 
    mutate(acs_type = paste0('acs', span))

  denom <- df %>% 
    filter(STANDARD_JOBSECTOR == 'Total') %>%
    select(COUNTY, count_denom = count, moe_denom = count_moe)
  
  df_sum <- df %>%
    filter(DIS != 'Total') %>%
    group_by(acs_type, DATA_YEAR, COUNTY, STANDARD_JOBSECTOR, DIS) %>%
    summarise(count = sum(count), count_moe = moe_sum(estimate = count, moe = count_moe)) %>%
    mutate(se = count_moe/1.645) %>%
    mutate(cv = (se/count))

  df_join <- df_sum %>%
    left_join(denom, by = 'COUNTY') %>%
    group_by(acs_type, DATA_YEAR, COUNTY, STANDARD_JOBSECTOR, DIS) %>%
    summarise(count, count_moe, cv, share = count/count_denom, share_moe = moe_prop(count, count_denom, count_moe, moe_denom))

  d <- df_join %>%
    select(acs_type, DATA_YEAR, COUNTY, STANDARD_JOBSECTOR, DIS, count, count_moe, cv,share, share_moe)
  
}

# test <- create_covempsect_pums_table(span = 5, dyear = 2020)