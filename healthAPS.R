
# IMPORT DATA ------------------------------------
lads <- read.csv('Data Inputs/Postcode_to_LSOA.csv')

life_sat <- read.csv('Data Inputs/Health Measures/APS_life_satisfaction.csv')
worthwhile <- read.csv('Data Inputs/Health Measures/APS_worthwhile.csv')
happiness <- read.csv('Data Inputs/Health Measures/APS_happiness.csv')
anxiety <- read.csv('Data Inputs/Health Measures/APS_anxiety.csv')


# CREATE TABLE OF LADS ---------------------------
aps <- data.frame(lad_ua = lads$ladcd)
aps <- distinct(aps)

# Change LAD names to old ones
aps$lad_ua <- as.character(aps$lad_ua)
aps[aps == 'E06000057'] <- 'E06000048'
aps[aps == 'E07000242'] <- 'E07000097'
aps[aps == 'E07000243'] <- 'E07000101'
aps[aps == 'E08000037'] <- 'E08000020'


# PULL HEALTH DATA -------------------------------

# Life satisfaction
life_sat <- life_sat[,c(1,5)]
names(life_sat) <- c('lad_ua', 'avg_life_sat')
aps <- left_join(aps, life_sat)

# Is life worthwhile
worthwhile <- worthwhile[,c(1,5)]
names(worthwhile) <- c('lad_ua', 'avg_worthwhile')
aps <- left_join(aps, worthwhile)

# Happiness
happiness <- happiness[,c(1,5)]
names(happiness) <- c('lad_ua', 'avg_happiness')
aps <- left_join(aps, happiness)

# Anxiety
anxiety <- anxiety[,c(1,5)]
names(anxiety) <- c('lad_ua', 'avg_anxiety')
aps <- left_join(aps, anxiety)

write.csv(aps, 'Data Outputs/healthAPS.csv')

