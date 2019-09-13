#install.packages('dplyr')
library(dplyr)
library(scales)

# IMPORT DATA -------------------------------------------
lsoas <- read.csv('Data Inputs/Postcode_to_LSOA.csv')

eimd <- read.csv('Data Inputs/Health Measures/EIMD_Health_2015.csv')
simd <- read.csv('Data Inputs/Health Measures/SIMD_indicator_2016.csv')

cenGHEW <- read.csv('Data Inputs/Health Measures/Census_Gen_Health_EW_2011.csv')
cenGHS <- read.csv('Data Inputs/Health Measures/Census_Gen_Health_S_2011.csv')
cenGHNI <- read.csv('Data Inputs/Health Measures/Census_Gen_Health_NI_2011.csv')

cenMHS <- read.csv('Data Inputs/Health Measures/Census_MH_S_2011.csv')

# Create health
health <- data.frame(lsoa_dz = lsoas$lsoa11cd)
health <- distinct(health)


# *IMD --------------------------------------------------

# EIMD
eimd <- eimd[,-c(2:7)]
names(eimd) <- c('lsoa_dz', 'eimd_indicator_2015')
health <- left_join(health, eimd)

# SIMD
simd <- select(simd, 'Data_Zone', 'DEPRESS')
names(simd) <- c('lsoa_dz', 'simd_prescription_2016')
health <- left_join(health, simd)


# CENSUS GENERAL HEALTH --------------------------------

# England and Wales
cenGHEW <- cenGHEW[, c(3,5,6:10)]
names(cenGHEW) <- c('lsoa_dz', 'census_GH_total', 'census_GH_2011_v_good', 'census_GH_2011_good', 'census_GH_2011_fair', 'census_GH_2011_bad', 'census_GH_2011_v_bad')

# Scotland
cenGHS <- cenGHS[, c(1:7)]
names(cenGHS) <- c('lsoa_dz', 'census_GH_total', 'census_GH_2011_v_good', 'census_GH_2011_good', 'census_GH_2011_fair', 'census_GH_2011_bad', 'census_GH_2011_v_bad')

# Northern Ireland
cenGHNI <- cenGHNI[, c(2:8)]
names(cenGHNI) <- c('lsoa_dz', 'census_GH_total', 'census_GH_2011_v_good', 'census_GH_2011_good', 'census_GH_2011_fair', 'census_GH_2011_bad', 'census_GH_2011_v_bad')

# Append S & NI data to EW and add to health
cenGH <- bind_rows(cenGHEW, cenGHS, cenGHNI)
health <- left_join(health, cenGH)


# CENSUS MH CONDITION SCOTLAND ------------------------
cenMHS <- cenMHS[, c(1, 2, 11)]
names(cenMHS) <- c('lsoa_dz', 'census_S_2011_pop_count', 'census_S_2011_MH_condition_count')
health <- left_join(health, cenMHS)



write.csv(health, 'Data Outputs/healthMeasures.csv')