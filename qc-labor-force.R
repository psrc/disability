# QC labor force with disability numbers
# To run this script successfully, install github version of tidycensus: devtools::install_github("walkerke/tidycensus")

library(psrccensus)
library(tidycensus)
library(tidyverse)
library(magrittr)


## 2022 ACS 1-year B18120 Employment Status by Disability Status and Type (Civilian Non-institutionalized Population 18 to 64 Years) ----

x <- get_acs_recs('county', table.names = 'B18120', years = '2022', acs.type = 'acs1')

x_reg <- x %>% 
  filter(variable == 'B18120_004' & name == 'Region')


## PUMS covered employment job sectors ----
span <- 1
dyear <- 2022

pums <- get_psrc_pums(span = span,                                            # Denoting ACS 5-year estimates; 1-year also available
                      dyear = dyear,                                          # Last data year of span
                      level = "p",                                            # Unit of analysis == household ("p" used for person)
                      vars = c("STANDARD_JOBSECTOR",                          # Cov Emp Sectors
                               "DIS",                                         # Disability recode
                               "ESR",                                         # Employment status recode
                               "AGEP"                                         # Age
                      ))   

# filter for Civilian/Armed persons and between 18-64 years
pums <- pums %>% filter((grepl("^(Civilian|Armed) ", as.character(ESR)) & between(AGEP, 18,64)))
# pums <- pums %>% filter(ESR %in% c(1,2,4,5) & between(AGEP, 18,64))

df <- psrc_pums_count(pums, 
                      group_vars = c("STANDARD_JOBSECTOR",       
                                     "DIS"                  
                      )) %>% 
  mutate(acs_type = paste0('acs', span))

## uncomment this line if using CRAN tidycensus
# if(dyear == 2022) {
#   df <- df %>% 
#     mutate(DIS = as.character(DIS)) %>% 
#     mutate(DIS = case_match(DIS,
#                             "1" ~ "With a disability",
#                             "2" ~ "Without a disability",
#                             .default = DIS))
# }

denom <- df %>% 
  filter(STANDARD_JOBSECTOR == 'Total') %>%
  select(COUNTY, count_denom = count, moe_denom = count_moe)

df_sum <- df %>%
  filter(DIS != 'Total') %>%
  mutate(se = count_moe/1.645) %>%
  mutate(cv = (se/count))

d <- df_sum %>%
  left_join(denom, by = 'COUNTY') %>%
  group_by(acs_type, DATA_YEAR, COUNTY, STANDARD_JOBSECTOR, DIS) %>%
  summarise(count, count_moe, cv, share = count/count_denom, share_moe = moe_prop(count, count_denom, count_moe, moe_denom))

# Check, compare to employed in labor force est
d %>% 
  filter(DIS == 'With a disability' & !is.na(STANDARD_JOBSECTOR)) %>% 
  ungroup() %>% 
  summarise(count = sum(count))
