library(tibble)
library(psych)
library(ordinal)

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

# IMPORT DATA ---------------------------------------
table <- read.csv('Data Outputs/COMPILED.csv')
table$X <- NULL

table <- add_column(table, response_rate_2014 = table$num_surveys_2014 / table$households, .after = "num_surveys_2014")
table <- add_column(table, response_rate_2011 = table$num_surveys_2011 / table$households, .after = "num_surveys_2011")


# CALC HEALTH --------------------------------------

# Get interpolated median of GH
GH <- table[,c('census_GH_total','census_GH_2011_v_good','census_GH_2011_good',
               'census_GH_2011_fair','census_GH_2011_bad','census_GH_2011_v_bad')]
GH <- intpMed(GH)
table <- add_column(table, census_GH_intp_median = GH$intp_median, .after = "census_GH_total")
#table[,c('census_GH_total','census_GH_2011_v_good','census_GH_2011_good',
#         'census_GH_2011_fair','census_GH_2011_bad','census_GH_2011_v_bad')] <- NULL


# Calc Scottish MH condition proportion
table <- add_column(table, census_MH_condition_proportion <- table$census_S_2011_MH_condition_count / 
                      table$census_S_2011_pop_count, .after = 'census_S_2011_pop_count')
#table[,c('census_S_2011_MH_condition_count','census_S_2011_pop_count')] <- NULL


# CALC CENSUS --------------------------------------

# Sex proportions
table <- add_column(table, proportion_female = table$female / table$total_ppl_sex, .after = "female")
table <- add_column(table, proportion_male = table$male / table$total_ppl_sex, .after = "male")
#table[,c('male','female')] <- NULL

# Interpolated median education
EDU <- table[,c('total_ppl_edu', 'edu_none', 'edu_mid', 'edu_high')]
EDU <- intpMed(EDU)
table <- add_column(table, edu_intp_median = EDU$intp_median, .after = 'total_ppl_edu')
#table[,c('total_ppl_edu','edu_none','edu_mid','edu_high')] <- NULL

# No longer married proportion
table <- add_column(table, no_longer_mar_prop = table$no_longer_married / table$total_ppl_mar, .after = 'total_ppl_mar')
#table[,c('total_ppl_mar','never_married','married','no_longer_married')] <- NULL

# Interpolated median SeC
table <- add_column(table, total_ppl_sec_without9 = table$total_ppl_sec - table$sec_lvl_9, .after = 'total_ppl_sec')
SEC <- table[,c('total_ppl_sec_without9','sec_lvl_1','sec_lvl_2','sec_lvl_3','sec_lvl_4',
                'sec_lvl_5','sec_lvl_6','sec_lvl_7','sec_lvl_8')]
SEC <- intpMed(SEC)
write.csv(SEC, 'test4.csv')
table <- add_column(table, sec_intp_median = SEC$intp_median, .after = 'total_ppl_sec')
#table[,c('total_ppl_sec','total_ppl_sec_without9','sec_lvl_1','sec_lvl_2','sec_lvl_3',
#         'sec_lvl_4','sec_lvl_5','sec_lvl_6','sec_lvl_7','sec_lvl_8')] <- NULL


# DISCARD RURAL LSOAS --------------------------------
mini <- subset(table, grepl("Urban|Majority Urban", rural_urban))

# SPLIT INTO TWO TABLES & DISCARD 0/1 RESPONSE RATE --
min14 <- mini[,-which(names(mini) %in% c('total_birds_2011','total_species_2011','num_surveys_2011',
                                         'response_rate_2011','avg_birds_2011','avg_species_2011'))]
min14 <- subset(min14, num_surveys_2014 > 1)
min11 <- mini[,-which(names(mini) %in% c('total_birds_2014','total_species_2014','num_surveys_2014',
                                         'response_rate_2014','avg_birds_2014','avg_species_2014'))]
min11 <- subset(min11, num_surveys_2011 > 1)


# SET THRESHOLD ---------------------------------------
num14 <- nrow(min14)
min14 <- subset(min14, num_surveys_2014 > 2)
num11 <- nrow(min11)
min11 <- subset(min11, num_surveys_2011 > 2)

message('Left of 2014: ', nrow(min14) / num14)
message('Left of 2011: ', nrow(min11) / num11)

write.csv(table, 'Data Outputs/FINALlsoa.csv')
write.csv(min14, 'Data Outputs/FINALlsoa14.csv')
write.csv(min11, 'Data Outputs/FINALlsoa11.csv')
