## Code to produce data at Scotland level for death indicators for the HFa database
# It uses a function to extract the data from the deaths catalogue and another
# one to calculate rates and format the data

# TODO:
# Deal in some way with only-female indicators, so they don't have the male rows 
# (cervix uteri cancer)
# Deal with all causes deaths

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
# Malignant neoplasms deaths
extract_deaths(diag = "^C[0-8]|^C9[0-7]", filename = "neoplasms", age064 = TRUE, plus65 = TRUE)
# trachea/bronchus/lung cancer deaths
extract_deaths(diag = "^C3[34]", filename = "trachealung_cancer", age064 = TRUE, plus65 = TRUE)
# Cancer of the cervix uteri deaths
extract_deaths(diag = "^C53", filename = "cervix_cancer", age064 = TRUE, plus65 = TRUE)
# Malignant neoplasm female breast deaths
extract_deaths(diag = "^C50", filename = "breast_cancer", age064 = TRUE, plus65 = TRUE)
# External causes of injury and poisoning deaths
extract_deaths(diag = "^V|^W|^X|^Y", filename = "inj_poison", 
               age064 = T, age04 = T, age519= T, plus65 = T)
# Motor vehicle traffic accidents deaths
extract_deaths(diag = "^V0[2349]|^V1[234]|^V[2-7]|^V8[2-79]", 
               filename = "motor_acc", age064 = TRUE, plus65 = TRUE)
# Suicide and self-inflicted injury
extract_deaths(diag = "^X[67]|^X8[0-4]", filename = "suicide", age064 = TRUE, plus65 = TRUE)
# Homicide and intentional injury
extract_deaths(diag = "^X8[5-9]|^X9|^Y0", filename = "homicide", age064 = TRUE, plus65 = TRUE)
# Transport vehicle accidents
extract_deaths(diag = "^V", filename = "transport_acc")
# All causes deaths
extract_deaths(diag = "", filename = "all_cause", age064 = TRUE, plus65 = TRUE)
# Infectious and parasitic diseases
extract_deaths(diag = "^A|^B", filename = "inf_parasite", age064 = TRUE, plus65 = TRUE)


###############################################.
## Part 2 - Call function to calculate rates ----
###############################################.
# Circulatory deaths 
create_rates(filename = "circulatory_deaths_allages", pop="allages", epop_total = 100000, ind_id="HFA_101")
create_rates(filename = "circulatory_deaths_0to64", pop="0to64", epop_total = 81500, ind_id="HFA_98")
create_rates(filename = "circulatory_deaths_65andover", pop="65+", epop_total = 19500, ind_id="HFA_104")
# Ischaemic heart disease deaths 
create_rates(filename = "ischaemic_deaths_allages", pop="allages", epop_total = 100000, ind_id="HFA_110")
create_rates(filename = "ischaemic_deaths_0to64", pop="0to64", epop_total = 81500, ind_id="HFA_107")
create_rates(filename = "ischaemic_deaths_65andover", pop="65+", epop_total = 19500, ind_id="HFA_113")
# Cerebrovascular disease deaths 
create_rates(filename = "cerebrovascular_deaths_allages", pop="allages", epop_total = 100000, ind_id="HFA_119")
create_rates(filename = "cerebrovascular_deaths_0to64", pop="0to64", epop_total = 81500, ind_id="HFA_116")
create_rates(filename = "cerebrovascular_deaths_65andover", pop="65+", epop_total = 19500, ind_id="HFA_122")
# Malignant neoplasms deaths 
create_rates(filename = "neoplasms_deaths_allages", pop="allages", epop_total = 100000, ind_id="HFA_128")
create_rates(filename = "neoplasms_deaths_0to64", pop="0to64", epop_total = 81500, ind_id="HFA_125")
create_rates(filename = "neoplasms_deaths_65andover", pop="65+", epop_total = 19500, ind_id="HFA_131")
# trachea/bronchus/lung cancer deaths 
create_rates(filename = "trachealung_cancer_deaths_allages", pop="allages", epop_total = 100000, ind_id="HFA_137")
create_rates(filename = "trachealung_cancer_deaths_0to64", pop="0to64", epop_total = 81500, ind_id="HFA_134")
create_rates(filename = "trachealung_cancer_deaths_65andover", pop="65+", epop_total = 19500, ind_id="HFA_140")
# Cancer of the cervix uteri deaths 
create_rates(filename = "cervix_cancer_deaths_allages", pop="allages", epop_total = 100000, ind_id="HFA_144")
create_rates(filename = "trachealung_cancer_deaths_0to64", pop="0to64", epop_total = 81500, ind_id="HFA_143")
create_rates(filename = "trachealung_cancer_deaths_65andover", pop="65+", epop_total = 19500, ind_id="HFA_145")
# Malignant neoplasm female breast deaths
create_rates(filename = "breast_cancer_deaths_allages", pop="allages", epop_total = 100000, ind_id="HFA_147")
create_rates(filename = "breast_cancer_deaths_0to64", pop="0to64", epop_total = 81500, ind_id="HFA_146")
create_rates(filename = "breast_cancer_deaths_65andover", pop="65+", epop_total = 19500, ind_id="HFA_148")
# External causes of injury and poisoning deaths
create_rates(filename = "inj_poison_deaths_allages", pop="allages", epop_total = 100000, ind_id="HFA_158")
create_rates(filename = "inj_poison_deaths_0to64", pop="0to64", epop_total = 81500, ind_id="HFA_149")
create_rates(filename = "inj_poison_deaths_0to4", pop="0to4", epop_total = 5000, ind_id="HFA_152")
create_rates(filename = "inj_poison_deaths_5to19", pop="5to19", epop_total = 16500, ind_id="HFA_155")
create_rates(filename = "inj_poison_deaths_65andover", pop="65+", epop_total = 19500, ind_id="HFA_161")
# Motor vehicle traffic accidents deaths
create_rates(filename = "motor_acc_deaths_allages", pop="allages", epop_total = 100000, ind_id="HFA_167")
create_rates(filename = "motor_acc_deaths_0to64", pop="0to64", epop_total = 81500, ind_id="HFA_164")
create_rates(filename = "motor_acc_deaths_65andover", pop="65+", epop_total = 19500, ind_id="HFA_170")
# Suicide and self-inflicted injury deaths
create_rates(filename = "suicide_deaths_allages", pop="allages", epop_total = 100000, ind_id="HFA_176")
create_rates(filename = "suicide_deaths_0to64", pop="0to64", epop_total = 81500, ind_id="HFA_173")
create_rates(filename = "suicide_deaths_65andover", pop="65+", epop_total = 19500, ind_id="HFA_179")
# Homicide and intentional injury
create_rates(filename = "homicide_deaths_allages", pop="allages", epop_total = 100000, ind_id="HFA_185")
create_rates(filename = "homicide_deaths_0to64", pop="0to64", epop_total = 81500, ind_id="HFA_182")
create_rates(filename = "homicide_deaths_65andover", pop="65+", epop_total = 19500, ind_id="HFA_188")
# Transport vehicle accidents
create_rates(filename = "transport_acc_deaths_allages", pop="allages", epop_total = 100000, ind_id="HFA_191")
# All causes deaths
create_rates(filename = "all_cause_deaths_allages", pop="allages", epop_total = 100000, ind_id="HFA_197")
create_rates(filename = "all_cause_deaths_0to64", pop="0to64", epop_total = 81500, ind_id="HFA_194")
create_rates(filename = "all_cause_deaths_65andover", pop="65+", epop_total = 19500, ind_id="HFA_200")
# Infectious and parasitic diseases
create_rates(filename = "inf_parasite_deaths_allages", pop="allages", epop_total = 100000, ind_id="HFA_206")
create_rates(filename = "inf_parasite_deaths_0to64", pop="0to64", epop_total = 81500, ind_id="HFA_203")
create_rates(filename = "inf_parasite_deaths_65andover", pop="65+", epop_total = 19500, ind_id="HFA_209")