library(dplyr)

# IMPORT DATA ----------------------------------------------
table <- read.csv('Data Outputs/FINAL.csv')


# SPLIT FUNCTION -------------------------------------------

split <- function(df) {
  
  # English and Welsh
  eng_wel <- subset(df, grepl("^E|^W", lsoa_dz))
  eng_wel$X <- NULL
  eng_wel$simd_prescription_2016 <- NULL
  eng_wel$census_S_2011_MH_percent <- NULL
  write.csv(eng_wel, 'Data Outputs/Countries/ENG_WAL.csv')
  
  # Northern Irish
  n_irish <- subset(df, grepl("^95", lsoa_dz))
  n_irish$X <- NULL
  n_irish$simd_prescription_2016 <- NULL
  n_irish$census_S_2011_MH_percent <- NULL
  write.csv(n_irish, 'Data Outputs/Countries/N_IRELAND.csv')
  
  # Scottish
  scottish <- subset(df, grepl("^S", lsoa_dz))
  scottish$X <- NULL
  scottish$eimd_indicator_2015 <- NULL
  write.csv(scottish, 'Data Outputs/Countries/SCOTLAND.csv')
  
  # NB.just the channel islands left
  #leftover <- anti_join(df, scottish)
  #leftover <- anti_join(leftover, eng_wel)
  #leftover <- anti_join(leftover, n_irish)
  #write.csv(leftover, 'LEFTOVER.csv')
  
}

# Run function
split(table)


print('complete')


