

###############################################.
## File Paths ----
###############################################.


data_folder <- "/PHI_conf/ScotPHO/HfA/Data/"
lookups <- "/PHI_conf/ScotPHO/HfA/Data/Lookups/"


###############################################.
## Functions ----
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




# Function to calculate age sex standardized rates

# Parameters:
# filename : name of indicator being produced
# pop: specify which population (e.g. allages, 65+, 0to64 to ensure populations match)
# epop_total: figure to use for total european standard pop
# ind_id: HFA indicator number

create_rates <- function(filename, pop, epop_total, ind_id) {
  
  #read in required population file
  pop_lookup <-readRDS(paste0(lookups,"Population/scot_pop_",pop,".rds"))
  
  #read in indicator data file
  data_indicator <-readRDS(paste0(data_folder,"Prepared Data/",filename,"_raw.rds"))
  
  #full join to add population
  data_indicator <- full_join(x = data_indicator, y = pop_lookup, by = c("year", "sex_grp", "age_grp"))
  
  #add epop (currently no indicators which don't include full 5 year ageband epop)
  data_indicator$epop <- recode(as.character(data_indicator$age_grp),
                                "1" = 5000, "2" = 5500, "3" = 5500, "4" = 5500, 
                                "5" = 6000, "6" = 6000, "7" = 6500, "8" = 7000, 
                                "9" = 7000, "10" = 7000, "11" =7000, "12" = 6500, 
                                "13" = 6000, "14" = 5500, "15" = 5000,
                                "16" = 4000, "17" = 2500, "18" = 1500, "19" = 1000)
  
  
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
    mutate(epop_total=epop_total,
           easr = easr_first/epop_total, # easr calculation
           rate = easr * 100000) %>%  # rate calculation
    select(-c(easr_first, epop_total, easr, epop))
 
# Preparing data for Shiny tool
  data_shiny <- data_indicator %>%
    mutate(ind_id=ind_id,
           country="Scotland") %>%
    rename(value=rate, sex=sex_grp) %>%
    select(c(year, value, country, sex, ind_id))

# Making final dataset available outside the function
final_result <<- data_shiny

# Save csv
#saveRDS(data_shiny, file = paste0(data_folder, "Data to be checked/",ind_id,"_",filename, ".rds"))
write_csv(data_shiny, path = paste0(data_folder, "Data to be checked/",ind_id,"_",filename, ".csv"))

}



