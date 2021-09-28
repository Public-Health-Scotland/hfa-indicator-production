

###############################################.
## File Paths ----
###############################################.
data_folder <- "/PHI_conf/ScotPHO/HfA/Data/"
lookups <- "/PHI_conf/ScotPHO/HfA/Data/Lookups/"

###############################################.
## Packages ----
###############################################.
library(dplyr) # for data manipulation
library(tidyr) # for data manipulation
library(readr) # writing csv's
library(magrittr) # for other pipe operators
library(odbc)


###############################################.
## Functions ----
###############################################.
###############################################.
# Age groups
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



###############################################.
# Function to calculate age sex standardized rates
# Parameters:
# filename : name of indicator being produced
# pop: specify which population (e.g. allages, 65+, 0to64 to ensure populations match)
# epop_total: figure to use for total european standard pop (use the figure for one gender only)
# ind_id: HFA indicator number

create_rates <- function(filename, pop, epop_total, ind_id) {
  
  #read in required population file
  pop_lookup <- readRDS(paste0(lookups,"Population/scot_pop_",pop,".rds"))
  
  #read in indicator data file
  data_indicator <- readRDS(paste0(data_folder,"Prepared Data/",filename,"_raw.rds"))
  
  #full join to add population
  data_indicator <- full_join(x = data_indicator, y = pop_lookup, 
                              by = c("year", "sex_grp", "age_grp"))
  
  #add epop (currently no indicators which don't include full 5 year ageband epop)
  data_indicator %<>%
    mutate(epop = recode(as.character(age_grp),
                         "1" = 5000, "2" = 5500, "3" = 5500, "4" = 5500, 
                         "5" = 6000, "6" = 6000, "7" = 6500, "8" = 7000, 
                         "9" = 7000, "10" = 7000, "11" =7000, "12" = 6500, 
                         "13" = 6000, "14" = 5500, "15" = 5000,
                         "16" = 4000, "17" = 2500, "18" = 1500, "19" = 1000),
           # Epop multiplied by 2 to obtain right one for "All"
           epop = case_when(sex_grp == "All" ~ epop*2, T ~ epop))
  
  # Calculating individual easr and variance
  data_indicator %<>%
    mutate(easr_first = numerator*epop/denominator) %>% # easr population
    # Converting Infinites to NA and NA's to 0s to allow proper functioning
    na_if(Inf) %>% # Caused by a denominator of 0 in an age group with numerator >0
    mutate_at(c("easr_first"), ~replace(., is.na(.), 0))
  
  # aggregating by sex_grp
  data_indicator %<>%
    subset(select = -c(age_grp)) %>% #remove age groups to calculate of all ages by year andsex_grp
    group_by(year, sex_grp) %>% summarise_all(sum, na.rm =T) %>% ungroup() 
  
  data_indicator %<>%
    # epop total population multiplied by 2 for "All" to obtain right number
    mutate(epop_total=case_when(sex_grp == "All" ~ epop_total*2, T ~ epop_total),
           easr = easr_first/epop_total, # easr calculation
           rate = easr * 100000) %>%  # rate calculation
    select(-c(easr_first, epop_total, easr, epop))
 
# Preparing data for Shiny tool
  data_indicator %<>%
    mutate(ind_id = ind_id, country = "Scotland") %>%
    replace_na(list(rate = 0)) %>%
    rename(value = rate, sex = sex_grp) %>%
    select(c(year, value, country, sex, ind_id))

# Making final dataset available outside the function
final_result <<- data_indicator

# Save csv
saveRDS(data_indicator, file = paste0(data_folder, "Data to be checked/",ind_id,"_",filename, ".rds"))

}

###############################################.
# Function to extract deaths data 
# This function extracts the deaths data and saves it formatted as raw
# Parameters:
# diag: diagnosis of the main cause of death to be extracted
# filename: name of the file to be created
# age064, plus65: parameters to create particular age cuts as well as the all ages one
extract_deaths <- function(diag, filename, age064 = F, plus65 = F, age04 = F, 
                           age519 = F, age3069 = F) {

    # If all cause deaths, no diagnosis selected
  diag_query <- case_when(diag == "All" ~ " ",
                          T ~ paste0("AND regexp_like(underlying_cause_of_death,'", diag, "')"))
  
  #Extracting deaths of Scottish residents, with valid sex and age with an specific diagnosis
  # Deaths for scottish residents are coded as (XS)
  deaths_extract <- tbl_df(dbGetQuery(channel, statement=paste0(
 "SELECT count(*) n, year_of_registration year, age, sex sex_grp, underlying_cause_of_death cod
 FROM ANALYSIS.GRO_DEATHS_C 
 WHERE date_of_registration between '1 January 2000' and '31 December 2019'
    AND age is not NULL
    AND sex <> 9
    AND country_of_residence = 'XS'
    ", diag_query, "
 GROUP BY year_of_registration, age, sex, underlying_cause_of_death "))) %>%
    setNames(tolower(names(.))) %>% #variables to lower case
    create_agegroups() %>% 
    mutate(sex_grp = recode(sex_grp, "1" = "Male", "2" = "Female"))
  
  list_diag <- as.vector(sort(unique(deaths_extract$cod)))
  print(paste("List of diagnosis extracted:", paste(list_diag, collapse = ", ")))
  
  # deaths by gender 
  deaths_extract %<>% group_by(year, age_grp, sex_grp) %>% 
    summarise(n = sum(n, na.rm = T)) %>% ungroup()
  
  # deaths for all
  deaths_all <- deaths_extract %>%
    group_by(year, age_grp) %>%
    summarise(n = sum(n, na.rm = T)) %>% mutate(sex_grp = "All") %>% ungroup()
  
  #combine datasets
  deaths <- rbind(deaths_extract, deaths_all) %>% rename(numerator = n)
  
  final_deaths_extract <<- deaths
  
  # Saving files
  # All Ages
  saveRDS(deaths, file=paste0(data_folder, 'Prepared Data/', filename, '_deaths_allages_raw.rds'))
  print(paste0("Saved file ", 'Prepared Data/', filename, '_deaths_allages_raw.rds'))
  
  #  Ages 0 to 64 years
  if (age064 == T) {
    saveRDS(deaths %>% subset(as.numeric(age_grp) <= 13), 
            file=paste0(data_folder, 'Prepared Data/', filename, '_deaths_0to64_raw.rds'))
    
    print(paste0("Saved file ", 'Prepared Data/', filename, '_deaths_0to64r_raw.rds'))
  }
  #  Ages 0 to 4 years
  if (age04 == T) {
    saveRDS(deaths %>% subset(as.numeric(age_grp) == 1), 
            file=paste0(data_folder, 'Prepared Data/', filename, '_deaths_0to4_raw.rds'))
    
    print(paste0("Saved file ", 'Prepared Data/', filename, '_deaths_0to4_raw.rds'))
  }
  #  Ages 5 to 19 years
  if (age519 == T) {
    saveRDS(deaths %>% subset(between(as.numeric(age_grp), 2, 4)), 
            file=paste0(data_folder, 'Prepared Data/', filename, '_deaths_5to19_raw.rds'))
    
    print(paste0("Saved file ", 'Prepared Data/', filename, '_deaths_5to19_raw.rds'))
  }
  #  Ages 30 to 69 years
  if (age3069 == T) {
    saveRDS(deaths %>% subset(between(as.numeric(age_grp), 7, 14)), 
            file=paste0(data_folder, 'Prepared Data/', filename, '_deaths_30to69_raw.rds'))
    
    print(paste0("Saved file ", 'Prepared Data/', filename, '_deaths_30to69_raw.rds'))
  }
  #  Ages 65+ years
  if (plus65 == T) {
    saveRDS(deaths %>% subset(as.numeric(age_grp)>=14), 
            file=paste0(data_folder, 'Prepared Data/', filename, '_deaths_65andover_raw.rds'))
    
    print(paste0("Saved file ", 'Prepared Data/', filename, '_deaths_65andover_raw.rds'))
  }
}

###############################################.
# Function to create population file from HB population estimates (could use open date platform at some point?)
create_pop <- function(lower, upper, name) {
  
  scot_pop_sex <- readRDS('/conf/linkage/output/lookups/Unicode/Populations/Estimates/HB2019_pop_est_1981_2019.rds') %>%
    setNames(tolower(names(.))) %>% # variables to lower case
    subset(age >= lower & age <= upper) %>% #selecting age of interest
    filter(year>=2000) %>% 
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
  
  saveRDS(scot_pop, file=paste0(lookups,'Population/scot_pop_', name,'.rds'))
} 

##END
