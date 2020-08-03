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
create_pop(lower = 0, upper = 4, name = "0to4")
create_pop(lower = 5, upper = 19, name = "5to19")
create_pop(lower = 30, upper = 69, name = "30to69")

##END


