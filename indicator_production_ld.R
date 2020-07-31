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
library(odbc)


#pop_lookup <- "/PHI_conf/ScotPHO/HfA/Data/Lookups/Pops/"

source("2_Functions.R") #Normal indicator functions



###############################################.
## Part 1 - Extract data from SMRA ----
###############################################.
# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))

extract_deaths(diag = "^I[00-99]", filename = "circulatory", age064 = F, plus65 = F)


###############################################.
## Part 2 - Create the different geographies basefiles ----
###############################################.





###############################################.
## Part 3 - Call function to calculate rates ----
###############################################.


# Circulatory deaths - all ages
create_rates(filename = "circulatory deaths_allages", pop="allages", epop_total = 100000, ind_id="HFA_101")

# Circulatory deaths - 0-64years
create_rates(filename = "circulatory deaths_0to64", pop="0to64", epop_total = 79000, ind_id="HFA_98")

# Circulatory deaths - 65+years
create_rates(filename = "circulatory deaths_65andover", pop="65+", epop_total = 21000, ind_id="HFA_104")




