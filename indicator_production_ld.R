## indicator production - scotland data ----

## Diseases of the circulatory system, all ages, per 100 000, by sex (age-standardized death rate)

## ICD10 CODES: I00-I99

###############################################.
## Packages/Filepaths ----
###############################################.

library(dplyr) # for data manipulation
library(tidyr) # for data manipulation
library(readr) # writing csv's
library(magrittr) # for other pipe operators


pop_lookup <- "/PHI_conf/ScotPHO/HfA/Data/Lookups/Pops/"



###############################################.
## Functions ----
###############################################.

# creating age groups for EASR
create_agegroups <- function(dataset) {
  dataset %>% mutate(age_grp = case_when(between(age, 0, 4) ~ 1,
                                         between(age, 5, 9) ~ 2, between(age, 10, 14) ~ 3, 
                                         between(age, 15, 19) ~ 4, between(age, 20, 24) ~ 5, 
                                         between(age, 25, 29) ~ 6, between(age, 30, 34) ~ 7, 
                                         between(age, 35, 39) ~ 8, between(age, 40, 44) ~ 9, 
                                         between(age, 45, 49) ~ 10, between(age, 50, 54) ~ 11, 
                                         between(age, 55, 59) ~ 12, between(age, 60, 64) ~ 13,
                                         between(age, 65, 69) ~ 14, between(age, 70, 74) ~ 15,  
                                         between(age, 75, 79) ~ 16, between(age, 80, 84) ~ 17, 
                                         between(age, 85, 89) ~ 18, between(age, 90, 200) ~ 19))
}

# Add European Population for rate calculation
add_epop <- function(dataset) {
  dataset <- dataset %>% 
    mutate(epop = recode(as.character(age_grp), # EASR age group pops
                         "1"=5000, "2"=5500, "3"=5500, "4"=5500, "5"=6000, 
                         "6"=6000, "7"= 6500, "8"=7000, "9"=7000, "10"=7000,
                         "11"=7000, "12"=6500, "13"=6000, "14"=5500, "15"=5000,
                         "16"= 4000, "17"=2500, "18"=1500, "19"=1000)) 
}

# Function to calculate age sex standardized rates
create_rates <- function(dataset, epop_total, sex_grp, cats = NULL ) {
  dataset <- dataset %>%
    mutate(easr_first = numerator*epop/denominator) # easr population
  
  if (sex_grp == T) {
    # aggregating by year, code and time
    dataset <- dataset %>% select(-age_grp) %>%
      group_by_at(c(cats, "year", "sex_grp")) %>% 
      summarize_at(c("numerator", "easr_first"), sum, na.rm = T) %>% ungroup()    
  } else if (sex_grp == F) {
    # aggregating by year, code and time
    dataset <- dataset %>% select(-age_grp, -sex_grp) %>%
      group_by_at(c(cats, "year")) %>% 
      summarize_at(c("numerator", "easr_first"), sum, na.rm = T) %>% ungroup()
  }
  
  # Calculating rates
  dataset <- dataset %>%
    mutate(epop_total = epop_total,  # Total EPOP population
           easr = easr_first/epop_total, # easr calculation
           rate = easr*100000) %>%   # rate calculation
    select(-c(easr_first, epop_total, easr))
}

###############################################.
## Extract data from SMRA ----
###############################################.
# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))


# SQL query for drug deaths 2006-2018
# Deaths for scottish residents are coded as (XS)
circulatory_system_deaths <- tbl_df(dbGetQuery(channel, statement=
                                              "SELECT year_of_registration year, age, SEX sex_grp, UNDERLYING_CAUSE_OF_DEATH cod1, POSTCODE pc7
                                               FROM ANALYSIS.GRO_DEATHS_C 
                                               WHERE date_of_registration between '1 January 2000' and '31 December 2018'
                                               AND age is not NULL
                                               AND sex <> 9
                                               AND country_of_residence='XS'
                                               AND regexp_like(underlying_cause_of_death,'^I[00-99]')
                                               ")) %>%
  setNames(tolower(names(.))) %>% #variables to lower case
  create_agegroups() %>% 
  mutate(sex_grp = recode(sex_grp, "1" = "Male", "2" = "Female"))

# deaths dataframe by gender 
circulatory_system_deaths_sex <- circulatory_system_deaths %>%
  group_by(year, age_grp, sex_grp) %>% count() %>% #aggregating
  ungroup()

# deaths dataframe all
circulatory_system_deaths_all <- circulatory_system_deaths_sex %>%
  group_by(year, age_grp) %>%
  summarise(n =sum(n)) %>% mutate(sex_grp = "All") %>%
  ungroup()

#combined dataframe
circulatory_system_deaths_total <- rbind(circulatory_system_deaths_sex, 
                                         circulatory_system_deaths_all)



## Section below could ultimately become part of function

#parameters
data_indicator <- circulatory_system_deaths_total %>%
  rename(numerator = n)

epop_total <-c(200000,100000)



##read in population file
pop_lookup <-readRDS("/PHI_conf/ScotPHO/HfA/Data/Lookups/Pops/scot_pop_allages.rds")


##full join to add population
data_indicator <- full_join(x = data_indicator, y = pop_lookup, 
                            by = c("year", "sex_grp", "age_grp"))

data_indicator <- data_indicator %>% add_epop() # EASR age group pop

dataset <- data_indicator %>%
  mutate(easr_first = numerator*epop/denominator) %>% # easr population
  # Converting Infinites to NA and NA's to 0s to allow proper functioning
  na_if(Inf) %>% # Caused by a denominator of 0 in an age group with numerator >0
  mutate_at(c("easr_first"), ~replace(., is.na(.), 0))


# aggregating by year, code and time
dataset %<>% subset(select = -c(age_grp)) %>%
  group_by(year, sex_grp) %>% summarise_all(sum, na.rm =T) %>% ungroup() %>%
  mutate(epop_total=case_when(sex_grp=="All" ~ epop_total[1], TRUE~epop_total[2]),
         easr = easr_first/epop_total, # easr calculation
         rate = easr * 100000) %>%  # rate calculation
 select(-c(easr_first, epop_total, easr, epop))





easr_circ_system_deaths <- create_rates(dataset = test_deaths, epop_total = 100000, sex_grp = T)

easr_circ_system_deaths2 <- create_rates(dataset = test_deaths, epop_total = 200000, sex_grp = F)
