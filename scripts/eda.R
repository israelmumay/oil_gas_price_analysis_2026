#------------------------ EDA ------------------------#

source('preprocessing.R')

#--------- STRUCTURE ---------#
# cleaned data
# structure and size
names(clean)
str(clean)
head(clean)
dim(clean) # 50 rows 10 columns

# missing values per column
sum(colSums(is.na(clean))) # 0
sum(duplicated(clean$state)) # 0

# summary stats
summary(clean)

# check the increase columns against the prices
max(abs(clean$increase - (clean$mar19 - clean$prewar))) # 6.661338e-16
max(abs(clean$pct - 100 * clean$increase / clean$prewar)) # 0.04964539

# group sizes
table(clean$region) # 12 midwest states, 11 northeast, 14 south, 13 west
table(clean$coastal) # 27 no, 23 yes

# list of state names for every region
split(clean$state, clean$region)


# missing values in the matched density
sum(is.na(clean$pop_density)) # 0

# how many states share each value
table(clean$increase)

#--------- CHARTS AND PLOTS ---------#
# numeric columns only
num_data = clean[, sapply(clean, is.numeric)]

#--------- HISTOGRAMS ---------#
# histogram for each numeric column
for (var in names(num_data)) {
  hist(num_data[, var], main = paste('Histogram of', var), xlab = var)
}

#--------- BOX PLOTS ---------#
# box plot for each numeric column
for (var in names(num_data)) {
  boxplot(num_data[, var], main = paste('Box Plot of', var), ylab = var)
}

#--------- SCATTER PLOTS ---------#
# number predictors only, response excluded
features = c('prewar', 'pre_trend', 'pop_density')

# scatterplot of the response against each predictor
for (var in features) {
  plot(clean[, var], clean$increase, pch = 16,
       main = paste('Increase vs.', var),
       xlab = var,
       ylab = 'increase (dollars per gallon)')
  abline(lm(clean$increase ~ clean[, var]), col = 'red', lty = 2)
}

#--------- CATEGORICAL BOX PLOTS ---------#
# increase by region
boxplot(increase ~ region, data = clean,
        main = 'Gas Price Increase by Region',
        xlab = 'region',
        ylab = 'increase (dollars per gallon)')

# increase by coastal
boxplot(increase ~ coastal, data = clean,
        main = 'Gas Price Increase by Coastal Access',
        xlab = 'coastal state',
        ylab = 'increase (dollars per gallon)')

#--------- FEATURE TRASFORMATION ---------#
# transformed feature
clean$log_density = log(density) # took the log to better represent the  

# before the log
hist(clean$pop_density, main = 'Population Density',
     xlab = 'people per square mile')

# after the log
hist(clean$log_density, main = 'Log of Population Density',
     xlab = 'log of people per square mile')

#--------- CORRELATIONS ---------#
# new feature list with added log density
features = c('prewar', 'pre_trend', 'pop_density', 'log_density')
# correlation of the response with each predictor
for (var in features) {
  cat(var, round(cor(clean$increase, clean[, var]), 2), '\n')
}

#--------- SUMMARIES ---------#
# columns shown in every table
cols = c('state', 'region', 'prewar', 'pre_trend', 'pop_density', 'increase')

# states with the largest increases
head(clean[order(-clean$increase), cols]) # Hawaii, Cal, Wash...

# states with the smallest increases
tail(clean[order(-clean$increase), cols]) # Missouri, Missi...

# states with the largest pre-war trend
head(clean[order(-clean$pre_trend), cols]) # Washington, Cal...

# states with the smallest pre-war trend
tail(clean[order(-clean$pre_trend), cols]) # Iowa, Kan, TX...

# states with the highest population density
head(clean[order(-clean$pop_density), cols]) # New Jersey, Rhode Is...

# states with the lowest population density
tail(clean[order(-clean$pop_density), cols]) # North Dakota, South Dak...


# final check
dim(clean) # 50 rows 11 cols
