# Script to generate scottish population files used as denominators for various HfA indicators

###############################################.
## Packages/Filepaths ----
###############################################.
source("2_Functions.R") #Normal indicator functions

###############################################.
## Part 2 - Create Population Files ----
###############################################. 
# Scotland all ages.
create_pop(lower = 0, upper = 200, name = "allages")
create_pop(lower = 0, upper = 64, name = "0to64")
create_pop(lower = 65, upper = 200, name = "65+")

##END


