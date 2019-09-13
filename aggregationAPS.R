library(dplyr)

# FUNCTION TO RETURN INTP MEDIAN GIVEN DF -------------------------------------
# Note that df must be in form: col1 = total, cols 2-x = frequency of response
intpMed <- function(df) {
  
  # Calculate number of columns which contain frequencies (ie length of likert scale)
  numLik <- ncol(df) - 1
  
  # Create new columns for cumulative frequency, median, and interpolated median
  newCols <- c()
  for (i in 1:numLik) {
    colName <- paste0('cumu.', i)
    newCols[i] <- colName
  }
  newCols <- append(newCols, c('median', 'intp_median'))
  
  # Add new columns to df
  df[newCols] <- NA
  
  # Calculate cumulative frequencies
  df$cumu.1 <- df[,2]
  for (i in 2:numLik) {
    df[,numLik + 1 + i] <- df[,i+1] + df[,numLik + i]
  }
  
  for (i in 1:nrow(df)) {
    
    # FIND MEDIAN
    # Get mid number of total responses
    z <- (df[i,1] + 1)/2
    # Initialise median to 0
    med <- 0
    # Create vector of cumulative vals in row
    nums <- c()
    for (j in 1:numLik) {
      nums[j] <- df[i,numLik+j+1]
    }
    # If mid number straddles groups
    if ((z %% 1 != 0)  && ((z - 0.5) %in% nums)) {
      # Median is mid-value between groups
      med <- match((z - 0.5),nums) + 0.5
      # If mid number doesn't straddle groups
    } else {
      med <- min(which((z - 0.5) < nums))
    }
    df[i,'median'] <- med
    
    # FIND INTERPOLATED MEDIAN
    # If median is an integer then it needs interpolating
    intpM <- df[i, 'median']
    if (df[i,'median'] %% 1 == 0) {
      # n1 = sum of cats below if median > 1
      n1 <- 0
      if (df[i,'median'] > 1) {
        n1 <- df[i, paste0('cumu.', toString(df[i,'median'] - 1))] 
        
      }
      # n2 = num of responses in med cat
      n2 <- df[i, (df[i,'median']) + 1]
      # Calc intp median
      intpM <- intpM - 0.5 + (0.5*df[i,1] - n1)/n2
    }
    df[i,'intp_median'] <- intpM
  }
  df[,(numLik+1):(2*numLik+1)] <- NULL
  return(df)
}

# IMPORT DATA -----------------------------------
lsoas <- read.csv('Data Outputs/COMPILED.csv')
lads <- read.csv('Data Inputs/Postcode_to_LSOA.csv')

aps <- read.csv('Data Outputs/healthAPS.csv')
aps$X <- NULL


# AGGREGATE TO LAD/UA ---------------------------

# Drop unecessary columns
lsoas$X <- NULL
lsoas <- lsoas[, -which(names(lsoas) %in% c('rural_urban','avg_birds_2014','avg_species_2014',
                    'avg_birds_2011','avg_species_2011','eimd_indicator_2015',
                    'simd_prescription_2016','census_GH_total','census_GH_2011_v_good',
                    'census_GH_2011_good','census_GH_2011_fair','census_GH_2011_bad',
                    'census_GH_2011_v_bad','census_S_2011_pop_count','census_S_2011_MH_condition_count',
                    'greenspace_proportion'))]
lads <- lads[, c('lsoa11cd', 'ladcd')]
names(lads) <- c('lsoa_dz', 'lad_ua')
lads <- distinct(lads)

# Change LAD names to old ones
lads$lsoa_dz <- as.character(lads$lsoa_dz)
lads$lad_ua <- as.character(lads$lad_ua)
lads[lads == 'E06000057'] <- 'E06000048'
lads[lads == 'E07000242'] <- 'E07000097'
lads[lads == 'E07000243'] <- 'E07000101'
lads[lads == 'E08000037'] <- 'E08000020'

# Multiply age & households
lsoas <- add_column(lsoas, sum_age = lsoas$total_ppl_age * lsoas$mean_age, .after = 'mean_age')
#lsoas$sum_household_size <- lsoas$households * lsoas$avg_household_size

# Assign lad to each lsoa
lsoas <- inner_join(lsoas, lads)

# Remove lsoa_dz
lsoas$lsoa_dz <- NULL

# Aggregate across lsoas summing lots of things
agg <- aggregate(. ~ lad_ua, data=lsoas, FUN=sum)

# Create new table
#new <- data.frame(lad_ua = agg$lad_ua)
#new <- distinct(new)



# CALC RSPB PROPORTIONS -------------------------------
agg$avg_birds_2014 = round((agg$total_birds_2014 / agg$num_surveys_2014), 2)
agg$avg_species_2014 = round((agg$total_species_2014 / agg$num_surveys_2014), 2)
agg$response_rate_2014 = agg$num_surveys_2014 / agg$households
agg$avg_birds_2011 <- round((agg$total_birds_2011 / agg$num_surveys_2011), 2)
agg$avg_species_2011 <- round((agg$total_species_2011 / agg$num_surveys_2011), 2)
agg$response_rate_2011 <- agg$num_surveys_2011 / agg$households


# ADD APS DATA ---------------------------------------
agg <- left_join(agg, aps)


# CALC CENSUS PROPORTIONS ----------------------------
agg$mean_age <- round((agg$sum_age / agg$total_ppl_age), 2)
#new$avg_household_size <- round((agg$sum_household_size / agg$households), 2)
agg <- add_column(agg, female_prop = agg$female / agg$total_ppl_sex, .after = 'female')
agg <- add_column(agg, male_prop = agg$male / agg$total_ppl_sex, .after = 'male')
agg <- add_column(agg, no_longer_married_prop = agg$no_longer_married / agg$total_ppl_mar, .after = 'no_longer_married')


# CREATE INTP MEDIANS -----------------------------------

# Education
EDU <- agg[,c('total_ppl_edu', 'edu_none', 'edu_mid', 'edu_high')]
EDU <- intpMed(EDU)
agg <- add_column(agg, edu_intp_median = EDU$intp_median, .after = 'total_ppl_edu')
#table[,c('total_ppl_edu','edu_none','edu_mid','edu_high')] <- NULL

# SeC
agg <- add_column(agg, total_ppl_sec_without9 = new$total_ppl_sec - new$sec_lvl_9, .after = 'total_ppl_sec')
SEC <- agg[,c('total_ppl_sec_without9','sec_lvl_1','sec_lvl_2','sec_lvl_3','sec_lvl_4',
              'sec_lvl_5','sec_lvl_6','sec_lvl_7','sec_lvl_8')]
SEC <- intpMed(SEC)
agg <- add_column(agg, sec_intp_median = SEC$intp_median, .after = 'total_ppl_sec')
#table[,c('total_ppl_sec','total_ppl_sec_without9','sec_lvl_1','sec_lvl_2','sec_lvl_3',
#         'sec_lvl_4','sec_lvl_5','sec_lvl_6','sec_lvl_7','sec_lvl_8')] <- NULL


# CALC GREENSPACE PROPORTIONS ---------------------------
agg$greenspace_percent <- agg$greenspace_gpx / agg$greenspace_px

# Export
write.csv(agg, 'Data Outputs/FINALaps.csv')

