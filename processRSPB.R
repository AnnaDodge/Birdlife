library(dplyr)


# IMPORT DATA --------------------------------------------------------

rspb2014 <- read.csv('Data Inputs/RSPB/BGBW_Data_2014.csv', strip.white = TRUE)

rspb2011a <- read.csv('Data Inputs/RSPB/BGBW Science Extract 2011 - 1.csv', strip.white = TRUE)
rspb2011b <- read.csv('Data Inputs/RSPB/BGBW Science Extract 2011 - 2.csv', strip.white = TRUE)
rspb2011c <- read.csv('Data Inputs/RSPB/BGBW Science Extract 2011 - 3.csv', strip.white = TRUE)
rspb2011d <- read.csv('Data Inputs/RSPB/BGBW Paper form Science Extract - 1.csv', strip.white = TRUE)
rspb2011e <- read.csv('Data Inputs/RSPB/BGBW Paper form Science Extract - 2.csv', strip.white = TRUE)

lsoas <- read.csv('Data Inputs/Postcode_to_LSOA.csv', strip.white = TRUE)




# REMOVE DUPLICATE ROWS ---------------------------------------------

# Function to remove duplicates
removeDuplicates <- function(df) {
  # Find original num rows
  origRows <- nrow(df)
  # Remove duplicate rows (checked by survey IDs)
  df <- distinct(df)
  # Print num of duplicate rows removed
  message('Number of duplicate rows removed: ', origRows - nrow(df))
  # Return df without duplicates
  return(df)
}

# Run function
rspb2014 <- removeDuplicates(rspb2014)
rspb2011a <- removeDuplicates(rspb2011a)
rspb2011b <- removeDuplicates(rspb2011b)
rspb2011c <- removeDuplicates(rspb2011c)
rspb2011d <- removeDuplicates(rspb2011d)
rspb2011e <- removeDuplicates(rspb2011e)


# MERGE 2011 DATA ---------------------------------------------------

# Process function to enable merging of tables
process2011 <- function(df) {
  # Remove unecessary columns
  df <- df[, c(1, 11:82, 110, 87)]
  # Convert cols to correct type
  df[, c(2:74)] <- lapply(df[, c(2:74)], as.numeric)
  df[, c(1, 75)] <- lapply(df[, c(1, 75)], as.character)
  # Substitute NA for 0
  df[is.na(df)] <- 0
  # Return altered df
  return(df)
}

# Run function
rspb2011a <- process2011(rspb2011a)
rspb2011b <- process2011(rspb2011b)
rspb2011c <- process2011(rspb2011c)
rspb2011d <- process2011(rspb2011d)
rspb2011e <- process2011(rspb2011e)

# Bind tables together
rspb2011 <- bind_rows(rspb2011a, rspb2011b, rspb2011c, rspb2011d, rspb2011e)



# AGGREGATE BIRD DATA ------------------------------------------------

# Get rid of unecessary cols (already done for 2011)
rspb2014 <- rspb2014[, c(3, 17:88, 106)]
# Turn NA to 0s -> ensures calculations work (already done for 2011)
rspb2014[is.na(rspb2014)] <- 0


# Find total bird prevalence per survey
findPrevalence <- function(df) {
  # Sum birds across rows
  df$total_birds <- rowSums(df[,2:74])
  # Print first 10 to test against manual
  message('Total birds for first 10 rows:')
  for (i in 1:10) {
    print(df[i, 'total_birds'])
  }
  # Return altered df
  return(df)
}

# Run function
rspb2014 <- findPrevalence(rspb2014)
rspb2011 <- findPrevalence(rspb2011)


# Function to find total num of species per survey
findSpecies <- function(df) {
  # Count species across row
  df$total_species <- rowSums(df[,2:74] != 0)
  # Print first 10 to test against manual
  message('Total species for first 10 rows:')
  for (i in 1:10) {
    print(df[i,'total_species'])
  }
  # Return altered df
  return(df)
}

# Run function
rspb2014 <- findSpecies(rspb2014)
rspb2011 <- findSpecies(rspb2011)



# FIND LSOA FOR EACH SURVEY -----------------------------------------

# Get rid of unecessary cols again
rspb2014 <- rspb2014[, c(1,75,76)]
rspb2011 <- rspb2011[, c(1,76,77)]
lsoas <- lsoas[, c('pcds','lsoa11cd')]
lsoas[] <- lapply(lsoas[], as.character)

# Rename cols
names(rspb2014) <- c('postcode', 'total_birds_2014', 'total_species_2014')
names(rspb2011) <- c('postcode', 'total_birds_2011', 'total_species_2011')
names(lsoas) <- c('postcode', 'lsoa_dz')

# Find lsoa/dz for each postcode
output2014 <- inner_join(rspb2014, lsoas)
message('Number of surveys lost due to pcd 2014: ', nrow(anti_join(rspb2014, lsoas)))
output2011 <- inner_join(rspb2011, lsoas)
message('Number of surveys lost due to pcd 2011: ', nrow(anti_join(rspb2011, lsoas)))



# REMOVE OUTLIERS ----------------------------------------------------

# Create vectors of outliers
outTB2014 <- boxplot(output2014$total_birds_2014, plot=FALSE)$out
outTS2014 <- boxplot(output2014$total_species_2014, plot=FALSE)$out
outTB2011 <- boxplot(output2011$total_birds_2011, plot=FALSE)$out
outTS2011 <- boxplot(output2011$total_species_2011, plot=FALSE)$out

# Print number of outliers
message('Number of total bird outliers 2014: ', length(outTB2014))
message('Number of total species outliers 2014: ', length(outTS2014))
message('Number of total bird outliers 2011: ', length(outTB2011))
message('Number of total species outliers 2011: ', length(outTS2011))

# Remove rows with total_birds outliers
output2014 <- output2014[-which(output2014$total_birds_2014 %in% outTB2014),]
output2014 <- output2014[-which(output2014$total_species_2014 %in% outTS2014),]
output2011 <- output2011[-which(output2011$total_birds_2011 %in% outTB2011),]
output2011 <- output2011[-which(output2011$total_species_2011 %in% outTS2011),]



# PRINT FINAL TABLES ------------------------------------------------

# Remove Postcodes from output
output2014$postcode <- NULL
output2011$postcode <- NULL

# Write output to csv
write.csv(output2014, "Data Outputs/cleanedRSPB2014.csv")
write.csv(output2011, "Data Outputs/cleanedRSPB2011.csv")



# Program complete
print('complete')