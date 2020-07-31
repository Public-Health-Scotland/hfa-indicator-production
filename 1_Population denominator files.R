# Script to generate scottish population files used as denominators for various HfA indicators


###############################################.
## Packages/Filepaths ----
###############################################.
library(readr)
library(dplyr)

pop_lookup <- "/PHI_conf/ScotPHO/HfA/Data/Lookups/Population/"



###############################################.
## Part 1 - Functions ----
###############################################.

#Function to create age groups needed for standardization
create_agegroups <- function(dataset) {
  dataset %>% mutate(age_grp = as.character(case_when(between(age, 0, 4) ~ 1,
                                                      between(age, 5, 9) ~ 2, between(age, 10, 14) ~ 3, between(age, 15, 19) ~ 4, 
                                                      between(age, 20, 24) ~ 5, between(age, 25, 29) ~ 6, between(age, 30, 34) ~ 7, 
                                                      between(age, 35, 39) ~ 8, between(age, 40, 44) ~ 9, between(age, 45, 49) ~ 10, 
                                                      between(age, 50, 54) ~ 11, between(age, 55, 59) ~ 12, between(age, 60, 64) ~ 13,
                                                      between(age, 65, 69) ~ 14, between(age, 70, 74) ~ 15,  between(age, 75, 79) ~ 16,
                                                      between(age, 80, 84) ~ 17, between(age, 85, 89) ~ 18, between(age, 90, 200) ~ 19)))
}




#Function to create population file from HB population estimates (could use open date platform at some point?)
create_pop <- function(lower, upper, name) {
  
  scot_pop_sex <- readRDS('/conf/linkage/output/lookups/Unicode/Populations/Estimates/HB2019_pop_est_1981_2019.rds') %>%
    setNames(tolower(names(.))) %>% # variables to lower case
    subset(year > 1999 & year <= 2018) %>%
    subset(age >= lower & age <= upper) %>% #selecting age of interest
    mutate(sex_grp = case_when(sex==1 ~ "Male", sex== 2 ~ "Female", TRUE ~"Other")) %>%
    #rename("sex_grp" = "sex") %>%
    create_agegroups() %>%
    group_by(age_grp, sex_grp, year) %>% 
    summarise(denominator =sum(pop)) %>% ungroup()
  
  scot_pop_all <- scot_pop_sex %>%
    group_by(age_grp, year) %>%
    summarise(denominator=sum(denominator)) %>% ungroup() %>%
    mutate(sex_grp="All")
  
  scot_pop <- bind_rows(scot_pop_all,scot_pop_sex)
  
  saveRDS(scot_pop, file=paste0(pop_lookup,'scot_pop_', name,'.rds'))
}
  
  
###############################################.
## Part 2 - Create Population Files ----
###############################################. 

# Scotland all ages.
create_pop(lower = 0, upper = 200, name = "allages")
create_pop(lower = 0, upper = 64, name = "0to64")
create_pop(lower = 65, upper = 200, name = "65+")





#pop <-readRDS("/PHI_conf/ScotPHO/HfA/Data/Lookups/Population/scot_pop_65+.rds")
