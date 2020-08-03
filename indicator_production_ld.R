## indicator production - scotland data ----

## Diseases of the circulatory system, all ages, per 100 000, by sex (age-standardized death rate)

## ICD10 CODES: I00-I99
## For cross referencing purposes comparable figures published by British Heart Foundation for all constituent UK nations
## https://www.bhf.org.uk/what-we-do/our-research/heart-statistics/heart-statistics-publications/cardiovascular-disease-statistics-2019
## BHF figures are not as up to date as figures ISD are able to produce.

###############################################.
## Packages/Filepaths ----
###############################################.
source("2_Functions.R") #Normal indicator functions

###############################################.
## Part 1 - Extract data from SMRA and prepare basefiles ----
###############################################.
# SMRA login information
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"), 
                                      pwd=.rs.askForPassword("SMRA Password:")))

# Circulatory deaths
extract_deaths(diag = "^I[00-99]", filename = "circulatory",  age064 = TRUE, plus65 = TRUE)
# Ischaemic heart disease deaths
extract_deaths(diag = "^I2[0-5]", filename = "ischaemic", age064 = TRUE, plus65 = TRUE)
# Cerebrovascular disease deaths
extract_deaths(diag = "^I6", filename = "cerebrovascular", age064 = TRUE, plus65 = TRUE)

###############################################.
## Part 2 - Call function to calculate rates ----
###############################################.
# Circulatory deaths 
create_rates(filename = "circulatory_deaths_allages", pop="allages", epop_total = 100000, ind_id="HFA_101")
create_rates(filename = "circulatory_deaths_0to64", pop="0to64", epop_total = 79000, ind_id="HFA_98")
create_rates(filename = "circulatory_deaths_65andover", pop="65+", epop_total = 21000, ind_id="HFA_104")
# Ischaemic heart disease deaths 
create_rates(filename = "ischaemic_deaths_allages", pop="allages", epop_total = 100000, ind_id="HFA_110")
create_rates(filename = "ischaemic_deaths_0to64", pop="0to64", epop_total = 79000, ind_id="HFA_107")
create_rates(filename = "ischaemic_deaths_65andover", pop="65+", epop_total = 21000, ind_id="HFA_113")
# Cerebrovascular disease deaths 
create_rates(filename = "cerebrovascular_deaths_allages", pop="allages", epop_total = 100000, ind_id="HFA_119")
create_rates(filename = "cerebrovascular_deaths_0to64", pop="0to64", epop_total = 79000, ind_id="HFA_116")
create_rates(filename = "cerebrovascular_deaths_65andover", pop="65+", epop_total = 21000, ind_id="HFA_122")

