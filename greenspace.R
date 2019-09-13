library(dplyr)

# IMPORT DATA --------------------------------------------
greenspaceEWS <- read.csv('Data Outputs/Greenspace/BritainBufferAverages.csv')
greenspaceNI <- read.csv('Data Outputs/Greenspace/NIBufferAverages.csv')
greenspaceEWS$X <- NULL
greenspaceNI$X <- NULL

# Rename first column
colnames(greenspaceEWS)[1] <- "lsoa_dz"
colnames(greenspaceNI)[1] <- "lsoa_dz"

# Merge together
greenspace <- bind_rows(greenspaceEWS, greenspaceNI)

# Drop unnecessary columns
names(greenspace) <- c('lsoa_dz', 'greenspace_gpx', 'greenspace_px', 'greenspace_proportion')

# Export
write.csv(greenspace, 'Data Outputs/greenspace.csv')