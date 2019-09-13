library(dplyr)
library(scales)

# IMPORT DATA ---------------------------------------
orig <- read.csv('Data Outputs/COMPILED.csv')

min <- data.frame('lsoa_dz' = orig$lsoa_dz, 
                  'rural_urban' = orig$rural_urban,
                  'avg_birds_2014' = orig$avg_birds_2014,
                  'avg_species_2014' = orig$avg_species_2014,
                  'num_surveys_2014' = orig$num_surveys_2014,
                  'avg_birds_2011' = orig$avg_birds_2011,
                  'avg_species_2011' = orig$avg_species_2011,
                  'num_surveys_2011' = orig$num_surveys_2011,
                  'eimd_indicator_2015' = orig$eimd_indicator_2015,
                  'simd_prescription_2016' = orig$simd_prescription_2016)

# Calc response rates
min$response_rate_2014 <- min$num_surveys_2014 / orig$households
min$response_rate_2011 <- min$num_surveys_2011 / orig$households
min <- min[, c(1,2,3,4,5,11,6,7,8,12,9,10)]


# CALC HEALTH PROPORTIONS ----------------------------
min$census_GH_good <- orig$census_GH_2011_good / orig$census_GH_total
min$census_GH_bad <- orig$census_GH_2011_bad / orig$census_GH_total
min$census_MH_condition <- orig$census_S_2011_MH_condition_count / orig$census_S_2011_pop_count


# CALC CENSUS CONFOUNDER PROPORTIONS -----------------
#min$sex_female <- percent(orig$female / orig$total_ppl_sex)
#min$sex_male <- percent(orig$male / orig$total_ppl_sex)
min$mean_age <- orig$mean_age
min$edu_none <- percent(orig$edu_none / orig$total_ppl_edu)
min$edu_mid <- percent(orig$edu_mid / orig$total_ppl_edu)
min$edu_high <- percent(orig$edu_high / orig$total_ppl_edu)
min$never_married <- percent(orig$never_married / orig$total_ppl_mar)
min$married <- percent(orig$married / orig$total_ppl_mar)
min$no_longer_married <- percent(orig$no_longer_married / orig$total_ppl_mar)
#min$avg_household_size <- orig$avg_household_size
min$sec_lvl_1 <- orig$sec_lvl_1 / orig$total_ppl_sec
min$sec_lvl_2 <- orig$sec_lvl_2 / orig$total_ppl_sec
min$sec_lvl_3 <- orig$sec_lvl_3 / orig$total_ppl_sec
min$sec_lvl_4 <- orig$sec_lvl_4 / orig$total_ppl_sec
min$sec_lvl_5 <- orig$sec_lvl_5 / orig$total_ppl_sec
min$sec_lvl_6 <- orig$sec_lvl_6 / orig$total_ppl_sec
min$sec_lvl_7 <- orig$sec_lvl_7 / orig$total_ppl_sec
min$sec_lvl_8 <- orig$sec_lvl_8 / orig$total_ppl_sec
min$sec_lvl_9 <- orig$sec_lvl_9 / orig$total_ppl_sec


# CALC GREENSPACE PROPORTIONS ------------------------
min$greenspace_percent <- percent(orig$greenspace_proportion)

# DISCARD RURAL LSOAS --------------------------------
min <- subset(min, grepl("Urban|Majority Urban", rural_urban))

# SPLIT INTO TWO TABLES & DISCARD 0/1 RESPONSE RATE --
min14 <- data.frame(min[,-c(7:10)])
min14 <- subset(min14, num_surveys_2014 > 1)
num14 <- nrow(min14)
min14 <- subset(min14, num_surveys_2014 > 2)

min11 <- data.frame(min[,-c(3:6)])
min11 <- subset(min11, num_surveys_2011 > 1)
num11 <- nrow(min11)
min11 <- subset(min11, num_surveys_2011 > 2)

message('Left of 2014: ', nrow(min14) / num14)
message('Left of 2011: ', nrow(min11) / num11)


# EXPORT ---------------------------------------------
write.csv(min, 'Data Outputs/FINALlsoa.csv')
write.csv(min14, 'Data Outputs/FINALlsoa14.csv')
write.csv(min11, 'Data Outputs/FINALlsoa11.csv')

