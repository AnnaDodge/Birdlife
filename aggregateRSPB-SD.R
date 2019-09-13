library(dplyr)

# IMPORT DATA --------------------------------------------

rspb2014 <- read.csv('Data Outputs/cleanedRSPB2014.csv')
rspb2011 <- read.csv('Data Outputs/cleanedRSPB2011.csv')


# AGGREGATE SURVEY DATA ---------------------------------

# Aggregate bird totals for DZ/LSOA
agg2014 <- aggregate(cbind(total_birds_2014, total_species_2014) ~ lsoa_dz, data=rspb2014, sum)
agg2011 <- aggregate(cbind(total_birds_2011, total_species_2011) ~ lsoa_dz, data=rspb2011, sum)

# Count num rows per DZ/LSOA (rename columns)
count2014 <- setNames(aggregate(total_birds_2014 ~ lsoa_dz, data=rspb2014, FUN=length), c("lsoa_dz", "num_surveys_2014"))
count2011 <- setNames(aggregate(total_birds_2011 ~ lsoa_dz, data=rspb2011, FUN=length), c("lsoa_dz", "num_surveys_2011"))

# Merge together again
new2014 <- inner_join(agg2014, count2014)
new2011 <- inner_join(agg2011, count2011)



# CALC AVERAGES & REMOVE OTHER COLS --------------------

new2014$avg_birds_2014 <- new2014$total_birds_2014 / new2014$num_surveys_2014
new2011$avg_birds_2011 <- new2011$total_birds_2011 / new2011$num_surveys_2011

new2014$avg_species_2014 <- new2014$total_species_2014 / new2014$num_surveys_2014
new2011$avg_species_2011 <- new2011$total_species_2011 / new2011$num_surveys_2011

#new2014$total_birds_2014 <- NULL
#new2011$total_birds_2011 <- NULL

#new2014$total_species_2014 <- NULL
#new2011$total_species_2011 <- NULL


# PRINT FINAL TABLES ------------------------------------
write.csv(new2014, 'Data Outputs/finalRSPB2014.csv')
write.csv(new2011, 'Data Outputs/finalRSPB2011.csv')


print('complete')

