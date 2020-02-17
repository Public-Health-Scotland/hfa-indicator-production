## indicator production - scotland data ----

## Diseases of the circulatory system, all ages, per 100 000, by sex (age-standardized death rate)

## ICD10 CODES: I00-I99

###############################################.
## Packages ----
###############################################.
library(foreign) # to read SPSS data.
library(dplyr) # for data manipulation
library(ggplot2) # for plotting
library(tidyr) # for data manipulation
library(RcppRoll) #for moving averages
library(readr) # writing csv's
library(odbc) # for reading oracle databases
library(readxl) # for reading excel
library(rmarkdown) # for data quality checking
library(shiny) # for data quality checking
library(flextable) # for output tables
library(plotly) # for data quality checking
library(htmltools) # for data quality checking
library(magrittr) # for other pipe operators

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
circulatory_system_deaths <- tbl_df(dbGetQuery(channel, statement=
                                                 "SELECT year_of_registration year, age, SEX sex_grp, UNDERLYING_CAUSE_OF_DEATH cod1, POSTCODE pc7, 
                                               REGISTRATION_DISTRICT rdno, ENTRY_NUMBER entry_no
                                               FROM ANALYSIS.GRO_DEATHS_C 
                                               WHERE date_of_registration between '1 January 2000' and '31 December 2018'
                                               AND age is not NULL
                                               AND sex <> 9
                                               AND regexp_like(underlying_cause_of_death,'^I[00-99]')
                                               ")) %>%
  setNames(tolower(names(.))) %>% #variables to lower case
  create_agegroups() %>% 
  mutate(sex_grp = recode(sex_grp, "1" = "Male", "2" = "Female"))

circulatory_system_deaths_sex <- circulatory_system_deaths %>%
  group_by(year, age_grp, sex_grp) %>% count() %>% #aggregating
  ungroup()

circulatory_system_deaths_all <- circulatory_system_deaths_sex %>%
  group_by(year, age_grp) %>%
  summarise(n =sum(n)) %>% mutate(sex_grp = "All") %>%
  ungroup()

circulatory_system_deaths_total <- rbind(circulatory_system_deaths_sex, 
                                         circulatory_system_deaths_all)

scottish_population <- readRDS('/conf/linkage/output/lookups/Unicode/Populations/Estimates/HB2019_pop_est_1981_2018.rds') %>%
  setNames(tolower(names(.))) %>% # variables to lower case
  subset(year > 1999 & year <= 2018) %>%
  rename("sex_grp" = "sex") %>%
  mutate(sex_grp = recode(sex_grp, "1" = "Male", "2" = "Female"))

scottish_population <- scottish_population %>% create_agegroups() %>%
  group_by(age_grp, sex_grp, year) %>% 
  summarise(pop =sum(pop)) %>% ungroup()

test_deaths <- full_join(circulatory_system_deaths_sex, scottish_population, 
                         c("year", "age_grp", "sex_grp")) %>% 
  rename(numerator = n, denominator = pop, year = year) # numerator and denominator used for calculation

test_deaths <- test_deaths %>% add_epop() # EASR age group pops

easr_circ_system_deaths <- create_rates(dataset = test_deaths, epop_total = 100000,
                                        sex_grp = T)

easr_circ_system_deaths2 <- create_rates(dataset = test_deaths, epop_total = 200000,
                                         sex_grp = F)
