library(dplyr)
library(scales)

# IMPORT DATA --------------------------------------------

lsoas <- read.csv('Data Inputs/Postcode_to_LSOA.csv')

rspb2014 <- read.csv('Data Outputs/finalRSPB2014.csv')
rspb2011 <- read.csv('Data Outputs/finalRSPB2011.csv')
rspb2014$X <- NULL
rspb2011$X <- NULL

households <- read.csv('Data Outputs/households.csv')
households$X <- NULL

ru <-read.csv('Data Outputs/ruralUrban.csv')
ru$X <- NULL

health <- read.csv('Data Outputs/healthMeasures.csv')
health$X <- NULL

confounders <- read.csv('Data Outputs/censusConfounders.csv')
confounders$X <- NULL

greenspace <- read.csv('Data Outputs/greenspace.csv')
greenspace$X <- NULL

# CREATE BIG TABLE WITH DZs/LSOAs -------------------------
table <- data.frame(lsoa_dz = lsoas$lsoa11cd)
table <- distinct(table)


# ADD DATA ------------------------------------------------

# Add rural/urban
table <- left_join(table, ru)

# RSPB
table <- left_join(table, rspb2014)
table <- left_join(table, rspb2011)
# Check all allocated to a row in new table
#message('Number of rows of rspb 2014 left: ', nrow(anti_join(rspb2014, table)))
#message('Number of rows of rspb 2011 left: ', nrow(anti_join(rspb2011, table)))
# Make all NA 0
table[is.na(table)] <- 0

# Add household numbers
table <- left_join(table, households)

# Add health measures
table <- left_join(table, health)

# Add census confounders
table <- left_join(table, confounders)

# Add greenspace
table <- left_join(table, greenspace)


# DELETE ROWS -----------------------------------------------
table <- table[!(table$lsoa_dz == 'L99999999'),]
table <- table[!(table$lsoa_dz == 'M99999999'),]


# Print
write.csv(table, 'Data Outputs/COMPILED.csv')