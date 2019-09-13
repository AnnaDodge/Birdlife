library(dplyr)

# IMPORT DATA --------------------------------------------------
oas <- read.csv('Data Inputs/OA_Rural_Urban.csv')
lsoas <- read.csv('Data Inputs/Postcode_to_LSOA.csv')


# Give rural/urban number to OAs (rural = 0, urban = 1)
oas$runo <- ifelse(startsWith(as.character(oas$unified_classif), 'R'), '0', '1')

# Remove unecessary columns
oas <- oas[,c('oa11cd', 'runo')]
oas$runo <- as.numeric(oas$runo)
lsoas <- lsoas[, c('oa11cd','lsoa11cd')]
lsoas[] <- lapply(lsoas[], as.character)

# Assign lsoa to each oa
oas <- inner_join(oas, lsoas)

# Aggregate across lsoas summing rural urban number
agg <- setNames(aggregate(runo ~ lsoa11cd, data=oas, sum), c('lsoa_dz', 'ru_sum'))

# Count number of OAs in each LSOA
count <- setNames(aggregate(oa11cd ~ lsoa11cd, data=oas, FUN=length), c('lsoa_dz', 'oa_count'))

# Merge together again
ru <- inner_join(agg, count)

# Work out if rural or urban using ru_sum and oa_count
ru$rural_urban <- ifelse(ru$ru_sum == 0, 'Rural', 
                  ifelse(ru$ru_sum == ru$oa_count, 'Urban', 
                  ifelse(ru$ru_sum > ru$oa_count / 2, 'Majority Urban', 'Majority Rural'))) 

# Remove unecessary columns again
ru <- ru[,c(1,4)]

# Export
write.csv(ru, 'Data Outputs/ruralUrban.csv')
