## Life Expectancy data for Health for All Tool ----


###############################################.
## Packages/Filepaths ----
###############################################.
library(dplyr)
library(tidyr) # long to wide format
library(readr)



le_data <- read_csv("https://statistics.gov.scot/downloads/cube-table?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2FLife-Expectancy") %>%
  setNames(tolower(names(.))) %>%
  subset(featurecode=="S92000003")


le_data <- le_data %>%
  subset(featurecode=="S92000003",
         simdquintiles=="All",
         urbanruralclassification="All")


le_data 