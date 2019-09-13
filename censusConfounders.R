library(dplyr)

# IMPORT DATA -----------------------------------------------

lsoas <- read.csv('Data Inputs/Postcode_to_LSOA.csv')

sexEW <- read.csv('Data Inputs/Census Confounders/Sex_EW.csv')
sexS <- read.csv('Data Inputs/Census Confounders/Sex_S.csv')
sexNI <- read.csv('Data Inputs/Census Confounders/Sex_NI.csv')

ageEW <- read.csv('Data Inputs/Census Confounders/Age_EW.csv')
ageS <- read.csv('Data Inputs/Census Confounders/Age_S.csv')
ageNI <- read.csv('Data Inputs/Census Confounders/Age_NI.csv')

eduEW <- read.csv('Data Inputs/Census Confounders/Education_EW.csv')
eduS <- read.csv('Data Inputs/Census Confounders/Education_S.csv')
eduNI <- read.csv('Data Inputs/Census Confounders/Education_NI.csv')

marEW <- read.csv('Data Inputs/Census Confounders/Marital_EW.csv')
marS <- read.csv('Data Inputs/Census Confounders/Marital_S.csv')
marNI <- read.csv('Data Inputs/Census Confounders/Marital_NI.csv')

#hsEW <- read.csv('Data Inputs/Census Confounders/HouseholdSize_EW.csv')
#hsS <- read.csv('Data Inputs/Census Confounders/HouseholdSize_S.csv')
#hsNI <- read.csv('Data Inputs/Census Confounders/HouseholdSize_NI.csv')

secEW <- read.csv('Data Inputs/Census Confounders/SeC_EW.csv')
secS <- read.csv('Data Inputs/Census Confounders/SeC_S.csv')
secNI <- read.csv('Data Inputs/Census Confounders/SeC_NI.csv')

# CREATE TABLE ---------------------------------------------
confounders <- data.frame(lsoa_dz = lsoas$lsoa11cd)
confounders <- distinct(confounders)


# SEX ------------------------------------------------------

# English & Welsh
sexEW <- sexEW[, c(3,5:7)]
names(sexEW) <- c('lsoa_dz', 'total_ppl_sex', 'male', 'female')

# Scottish
names(sexS) <- c('lsoa_dz', 'total_ppl_sex', 'male', 'female')

# Northern Irish
sexNI <- sexNI[, c(2:5)]
names(sexNI) <- c('lsoa_dz', 'total_ppl_sex', 'male', 'female')

# Combine
sex <- bind_rows(sexEW, sexS, sexNI)

# Add to confounders table
confounders <- left_join(confounders, sex)



# MEAN AGE ------------------------------------------------

# English & Welsh - note needed to calculate mean age (rounded to 2dp)
for (i in 0:100) {
  ageEW[,i+6] <- ageEW[,i+6] * i
}
ageEW$mean_age <- round(rowSums(ageEW[,6:106]) / ageEW[,5], 2)
ageEW <- ageEW[,c(3,5,107)]
names(ageEW) <- c('lsoa_dz', 'total_ppl_age', 'mean_age')

# Scottish
ageS <- ageS[, c(1,2,19)]
names(ageS) <- c('lsoa_dz', 'total_ppl_age', 'mean_age')

# Northern Irish
ageNI <- ageNI[, c(2,3,20)]
names(ageNI) <- c('lsoa_dz', 'total_ppl_age', 'mean_age')

# Combine
age <- bind_rows(ageS, ageNI, ageEW)
confounders <- left_join(confounders, age)



# EDUCATION LEVEL --------------------------------------------

# English & Welsh
eduEW$edu_mid <- rowSums(eduEW[,c(7:10,12)])
eduEW <- eduEW[, c(3,5,6,13,11)]
names(eduEW) <- c('lsoa_dz', 'total_ppl_edu', 'edu_none', 'edu_mid', 'edu_high')

# Scottish
eduS$edu_mid <- rowSums(eduS[,c(4:6)])
eduS <- eduS[,c(1,2,3,10,7)]
names(eduS) <- c('lsoa_dz', 'total_ppl_edu', 'edu_none', 'edu_mid', 'edu_high')

# Northern Irish
eduNI$edu_mid <- rowSums(eduNI[,c(5:8,10)])
eduNI <- eduNI[,c(2:4,26,9)]
names(eduNI) <- c('lsoa_dz', 'total_ppl_edu', 'edu_none', 'edu_mid', 'edu_high')

# Combine
edu <- bind_rows(eduEW, eduS, eduNI)
confounders <- left_join(confounders, edu)



# MARITAL STATUS --------------------------------------------

# English & Welsh
marEW$married <- rowSums(marEW[,c(6,7)])
marEW$no_longer_married <- rowSums(marEW[,c(8:10)])
marEW <- marEW[,c(3,4,5,11,12)]
names(marEW) <- c('lsoa_dz', 'total_ppl_mar', 'never_married', 'married', 'no_longer_married')

# Scottish
marS$married <- rowSums(marS[,c(4,5)])
marS$no_longer_married <- rowSums(marS[,c(6:8)])
marS <- marS[,c(1:3,9,10)]
names(marS) <- c('lsoa_dz', 'total_ppl_mar', 'never_married', 'married', 'no_longer_married')

# Northern Ireland
marNI$married <- rowSums(marNI[,c(5,6)])
marNI$no_longer_married <- rowSums(marNI[,c(7:9)])
marNI <- marNI[,c(2,3,4,16,17)]
names(marNI) <- c('lsoa_dz', 'total_ppl_mar', 'never_married', 'married', 'no_longer_married')

# Combine
mar <- bind_rows(marEW, marS, marNI)
confounders <- left_join(confounders, mar)



# HOUSEHOLD SIZE -------------------------------------------

# English & Welsh
#for (i in 1:8) {
#  hsEW[,i+5] <- hsEW[,i+5] * i
#}
#hsEW$avg_household_size <- round(rowSums(hsEW[,6:13]) / hsEW[,5], 2)
#hsEW <- hsEW[,c(3,5,14)]
#names(hsEW) <- c('lsoa_dz', 'total_occupied_households', 'avg_household_size')

# Scottish
#for (i in 1:8) {
#  hsS[,i+2] <- hsS[,i+2] * i
#}
#hsS$avg_household_size <- round(rowSums(hsS[,3:10]) / hsS[,2], 2)
#hsS <- hsS[,c(1,2,11)]
#names(hsS) <- c('lsoa_dz', 'total_occupied_households', 'avg_household_size')

# Northern Irish
#hsNI <- hsNI[,c(2,12)]
#names(hsNI) <- c('lsoa_dz', 'avg_household_size')

# Combine
#hs <- bind_rows(hsEW, hsS, hsNI)
#confounders <- left_join(confounders, hs)



# SEC ---------------------------------------------------

# English & Welsh
secEW <- secEW[,c(3,5,6,15,23,28,35,40,48,54,57)]
names(secEW) <- c('lsoa_dz', 'total_ppl_sec', 'sec_lvl_1', 'sec_lvl_2', 'sec_lvl_3', 'sec_lvl_4', 'sec_lvl_5', 'sec_lvl_6', 'sec_lvl_7', 'sec_lvl_8', 'sec_lvl_9')

# Scottish
secS <- secS[,c(1:3,6:12,15)]
names(secS) <- c('lsoa_dz', 'total_ppl_sec', 'sec_lvl_1', 'sec_lvl_2', 'sec_lvl_3', 'sec_lvl_4', 'sec_lvl_5', 'sec_lvl_6', 'sec_lvl_7', 'sec_lvl_8', 'sec_lvl_9')

# Northern Ireland
secNI <- secNI[,c(2:4,7:13,16)]
names(secNI) <- c('lsoa_dz', 'total_ppl_sec', 'sec_lvl_1', 'sec_lvl_2', 'sec_lvl_3', 'sec_lvl_4', 'sec_lvl_5', 'sec_lvl_6', 'sec_lvl_7', 'sec_lvl_8', 'sec_lvl_9')

# Combine
sec <- bind_rows(secEW, secS, secNI)
confounders <- left_join(confounders, sec)

write.csv(confounders, 'Data Outputs/censusConfounders.csv')