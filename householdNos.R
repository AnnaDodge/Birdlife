#library(dplyr)
#library(scales)

# IMPORT DATA -------------------------------------------
householdsEW <- read.csv('Data Inputs/Household Numbers/Census_Households_EW_2011.csv')
householdsS <- read.csv('Data Inputs/Household Numbers/Census_Households_S_2011.csv')
householdsNI <- read.csv('Data Inputs/Household Numbers/Census_Households_NI_2011.csv')


# HOUSEHOLD NUMBERS ------------------------------------

# English & Welsh
householdsEW <- householdsEW[, c(3,5)]
names(householdsEW) <- c('lsoa_dz', 'households')

# Scottish
householdsS <- householdsS[, c(1, 2)]
names(householdsS) <- c('lsoa_dz', 'households')

# Northern Irish
householdsNI <- householdsNI[, c(2,3)]
names(householdsNI) <- c('lsoa_dz', 'households')

# COMBINE ----------------------------------------------
households <- bind_rows(householdsEW, householdsS, householdsNI)


write.csv(households, 'Data Outputs/households.csv')