library(ggplot2)
library(reshape2)
library(pastecs)
library(scales)

# IMPORT DATA
table <- read.csv('Data Outputs/FINALlsoa.csv')
table$X <- NULL



# Health 2014
table2014 <- table[, c(1,3:5,9:13)]


# Health 2011
table2011 <- table[, c(1,6:13)]
message('Mean response rate 2014: ', percent(mean(table$response_rate_2014)))
message('Mean response rate 2011: ', percent(mean(table$response_rate_2011)))
#rrBoxplot + geom_boxplot() + labs(x = '', y = 'Response Rate 2014')

new <- subset(table2014, response_rate_2014 >= 0.0005)
new11 <- subset(table2011, response_rate_2011 >= 0.005)

# Plot histogram of avg birds
plot <- ggplot(data=new11, aes(new11$avg_birds_2011)) + 
  geom_histogram(col='blue') +
  labs(title='Histogram of bird frequency') +
  labs(x='Average number of birds', y='Frequency')
print(plot)

# Note: remove norm=TRUE for standard vals
print(stat.desc(new11$avg_birds_2011, norm=TRUE))

#table2014$response_rate_2014
#boxplot(table2014$response_rate_2014)