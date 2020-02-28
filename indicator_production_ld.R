## indicator production - scotland data ----

## Diseases of the circulatory system, all ages, per 100 000, by sex (age-standardized death rate)

## ICD10 CODES: I00-I99
## For cross referencing purposes comparable figures published by British Heart Foundation for all constituent UK nations
## https://www.bhf.org.uk/what-we-do/our-research/heart-statistics/heart-statistics-publications/cardiovascular-disease-statistics-2019
## BHF figures are not as up to date as figures ISD are able to produce.


###############################################.
## Packages/Filepaths ----
###############################################.

library(dplyr) # for data manipulation
library(tidyr) # for data manipulation
library(readr) # writing csv's
library(magrittr) # for other pipe operators


#pop_lookup <- "/PHI_conf/ScotPHO/HfA/Data/Lookups/Pops/"

source("2_Functions.R") #Normal indicator functions



###############################################.
## Part 1 - Extract data from SMRA ----
###############################################.
# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))

# SQL query for drug deaths 2006-2018
# Deaths for scottish residents are coded as (XS)
circulatory_deaths_extract <- tbl_df(dbGetQuery(channel, statement=
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

# deaths by gender 
circulatory_deaths_sex <- circulatory_deaths_extract %>%
  group_by(year, age_grp, sex_grp) %>% count() %>% #aggregating
  ungroup()

# deaths for all
circulatory_deaths_all <- circulatory_deaths_sex %>%
  group_by(year, age_grp) %>%
  summarise(n =sum(n)) %>% mutate(sex_grp = "All") %>%
  ungroup()

#combine datasets
circulatory_deaths <- rbind(circulatory_deaths_sex,circulatory_deaths_all) %>%
  rename(numerator = n)


###############################################.
## Part 2 - Create the different geographies basefiles ----
###############################################.


# Circulatory deaths - All Ages
saveRDS(circulatory_deaths, file=paste0(data_folder, 'Prepared Data/circulatory deaths_allages_raw.rds'))

# Circulatory deaths - Ages 0 to 64 years
circulatory_deaths_0to64 <-circulatory_deaths %>% subset(as.numeric(age_grp)<=13)
saveRDS(circulatory_deaths, file=paste0(data_folder, 'Prepared Data/circulatory deaths_0to64_raw.rds'))

# Circulatory deaths - Ages 65+ years
circulatory_deaths_0to64 <-circulatory_deaths %>% subset(as.numeric(age_grp)>=14)
saveRDS(circulatory_deaths, file=paste0(data_folder, 'Prepared Data/circulatory deaths_65andover_raw.rds'))



###############################################.
## Part 3 - Call function to calculate rates ----
###############################################.


# Circulatory deaths - all ages
create_rates(filename = "circulatory deaths_allages", pop="allages", epop_total = 100000, ind_id="HFA_101")

# Circulatory deaths - 0-64years
create_rates(filename = "circulatory deaths_0to64", pop="0to64", epop_total = 79000, ind_id="HFA_98")

# Circulatory deaths - 65+years
create_rates(filename = "circulatory deaths_65andover", pop="65+", epop_total = 21000, ind_id="HFA_104")




