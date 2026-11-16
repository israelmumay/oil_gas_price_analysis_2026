#------------------------ EDA ------------------------#
library(tidyverse)

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
# this confirms that the $ and % increase match the data prices

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

# categorical box plots
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

#--------- SCATTER PLOTS ---------#
# number predictors only (response excluded)
features = c('prewar', 'pre_trend', 'pop_density')

# scatterplot of the response against each predictor
for (var in features) {
  plot(clean[, var], clean$increase, pch = 16,
       main = paste('Increase vs.', var),
       xlab = var,
       ylab = 'increase (dollars per gallon)')
  abline(lm(clean$increase ~ clean[, var]), col = 'red', lty = 2)
}

#--------- FEATURE TRASFORMATION ---------#
# transformed feature
clean$log_density = log(clean$pop_density) # took the log to better represent the data

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

#--------- BASIC SUMMARIES ---------#
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

#------------------------ EDA: DAILY PRICES ------------------------#
#--------- STRUCTURE ---------#
# daily oil and gas prices
# structure and size
names(daily_prices)
str(daily_prices)
head(daily_prices)
dim(daily_prices) # 24 rows 16 columns

# missing values and duplicate dates
sum(colSums(is.na(daily_prices))) # 0
sum(duplicated(daily_prices$date)) # 0

# summary stats
summary(daily_prices)

#--------- PREP ---------#
# short name copy with real dates, ordered by day
daily = daily_prices
daily$date = as.Date(daily$date)
daily = daily[order(daily$date), ]

# phase as an ordered factor with short labels for the plots
daily$phase = factor(daily$phase,
                     levels = unique(daily$phase[order(daily$war_day)]),
                     labels = c('pre-war', 'week 1', 'week 2', 'week 3'))

# before or after the first strike
daily$war = factor(ifelse(daily$war_day > 0, 'yes', 'no'))

# group sizes
table(daily$phase) # 6 pre-war, 6 week 1, 5 week 2, 7 week 3
table(daily$war) # 6 no, 18 yes

# spacing between rows (the series is not evenly spaced)
table(diff(daily$date)) # 17 one day gaps, the rest 2 to 6 days

# check the derived columns against the prices
prewar_brent = daily$brent_usd_barrel[daily$war_day == -1] # 73.1 on feb 27
prewar_gas = daily$us_gas_avg_gallon[daily$war_day == 1] # 2.98 on feb 28

max(abs(daily$brent_vs_prewar_pct - 100 * (daily$brent_usd_barrel - prewar_brent) / prewar_brent)) # 0.04487
max(abs(daily$gas_vs_prewar_pct - 100 * (daily$us_gas_avg_gallon - prewar_gas) / prewar_gas)) # 0.04899
max(abs(daily$gas_change_from_prewar_dollars - (daily$us_gas_avg_gallon - prewar_gas))) # 2.220446e-16
# the brent percent column is measured off feb 27 but the two gas columns are measured off feb 28,
# so the file does not use one common pre-war baseline

#--------- CHARTS AND PLOTS ---------#
# numeric columns only
daily_num = daily[, sapply(daily, is.numeric)]

#--------- HISTOGRAMS ---------#
# histogram for each numeric column
for (var in names(daily_num)) {
  hist(daily_num[, var], main = paste('Histogram of', var), xlab = var)
}
# ship traffic is the odd one, 138 before the war and 4 to 12 after, so it acts like an on off switch

#--------- BOX PLOTS ---------#
# box plot for each numeric column
for (var in names(daily_num)) {
  boxplot(daily_num[, var], main = paste('Box Plot of', var), ylab = var)
}

# categorical box plots
# brent by phase
boxplot(brent_usd_barrel ~ phase, data = daily,
        main = 'Brent Crude by War Phase',
        xlab = 'phase',
        ylab = 'brent (dollars per barrel)')

# gas by phase
boxplot(us_gas_avg_gallon ~ phase, data = daily,
        main = 'US Average Gas Price by War Phase',
        xlab = 'phase',
        ylab = 'gas (dollars per gallon)')

# gas before and after the first strike
boxplot(us_gas_avg_gallon ~ war, data = daily,
        main = 'US Average Gas Price Before and After the War Started',
        xlab = 'war started',
        ylab = 'gas (dollars per gallon)')

# phase medians for the write up
aggregate(cbind(brent_usd_barrel, us_gas_avg_gallon) ~ phase, data = daily, FUN = median)
# brent 71.35, 89.40, 100.20, 104.60 and gas 2.84, 3.15, 3.54, 3.79

#--------- LINE PLOTS OVER TIME ---------#
# brent over time
plot(daily$date, daily$brent_usd_barrel, type = 'l',
     main = 'Brent Crude Over Time',
     xlab = 'date',
     ylab = 'brent (dollars per barrel)')
abline(v = as.Date('2026-02-28'), col = 'red', lty = 2) # first strike

# gas over time
plot(daily$date, daily$us_gas_avg_gallon, type = 'l',
     main = 'US Average Gas Price Over Time',
     xlab = 'date',
     ylab = 'gas (dollars per gallon)')
abline(v = as.Date('2026-02-28'), col = 'red', lty = 2) # first strike

#--------- SCATTER PLOTS ---------#
# numeric predictors only (response and rescaled copies of it excluded)
daily_features = c('brent_usd_barrel', 'wti_usd_barrel', 'dubai_usd_barrel',
                   'us_diesel_avg_gallon', 'strait_hormuz_daily_ships',
                   'iran_production_mbpd', 'war_day')

# scatterplot of gas against each predictor
for (var in daily_features) {
  plot(daily[, var], daily$us_gas_avg_gallon, pch = 16,
       main = paste('US Gas Price vs.', var),
       xlab = var,
       ylab = 'gas (dollars per gallon)')
  abline(lm(daily$us_gas_avg_gallon ~ daily[, var]), col = 'red', lty = 2)
}

#--------- CORRELATIONS ---------#
# full correlation matrix
round(cor(daily_num), 2)

# correlation of gas with each predictor
for (var in daily_features) {
  cat(var, round(cor(daily$us_gas_avg_gallon, daily[, var]), 2), '\n')
}
# brent 0.98, wti 0.96, dubai 0.98, diesel 0.98, ships -0.76, iran production -0.96, war day 0.95

# the three crude benchmarks move together, so only one of them belongs in a model
round(cor(daily[, c('brent_usd_barrel', 'wti_usd_barrel', 'dubai_usd_barrel')]), 3) # 0.987 and up

# these two columns are rescaled copies of the response, not predictors
cor(daily$us_gas_avg_gallon, daily$gas_vs_prewar_pct) # 1
cor(daily$us_gas_avg_gallon, daily$gas_change_from_prewar_dollars) # 1

# ship traffic is a cliff not a slope, so rank correlation fits it better
cor(daily$us_gas_avg_gallon, daily$strait_hormuz_daily_ships, method = 'spearman') # -0.8569

# day over day changes instead of price levels
gas_chg = diff(daily$us_gas_avg_gallon)
brent_chg = diff(daily$brent_usd_barrel)
cor(gas_chg, brent_chg) # 0.5553

plot(brent_chg, gas_chg, pch = 16,
     main = 'Change in Gas vs. Change in Brent',
     xlab = 'brent change (dollars per barrel)',
     ylab = 'gas change (dollars per gallon)')
abline(lm(gas_chg ~ brent_chg), col = 'red', lty = 2)
# much weaker than the 0.98 on levels, both series trend up together so the levels correlation is inflated

# gas against the brent price from the previous row (rows are not all one day apart, so this is a rough lag)
n = nrow(daily)
cor(daily$us_gas_avg_gallon[-1], daily$brent_usd_barrel[-n]) # 0.9773

#--------- BASIC SUMMARIES ---------#
# columns shown in every table
daily_cols = c('date', 'war_day', 'phase', 'brent_usd_barrel', 'us_gas_avg_gallon')

# highest gas days
head(daily[order(-daily$us_gas_avg_gallon), daily_cols], 3) # mar 20, mar 19, mar 18

# lowest gas days
head(daily[order(daily$us_gas_avg_gallon), daily_cols], 3) # feb 9, feb 10, feb 14

# biggest one row jumps
daily[which.max(gas_chg) + 1, daily_cols] # mar 10, gas up 0.12
daily[which.max(brent_chg) + 1, daily_cols] # feb 28, brent up 6.40

# total moves over the window
range(daily$us_gas_avg_gallon) # 2.81 to 3.92
range(daily$brent_usd_barrel) # 67.40 to 110.30


#------------------------ EDA: EVENTS TIMELINE ------------------------#
#--------- STRUCTURE ---------#
# key events
# structure and size
names(events_timeline)
str(events_timeline)
head(events_timeline)
dim(events_timeline) # 11 rows 7 columns

# missing values and duplicate dates
sum(colSums(is.na(events_timeline))) # 0
sum(duplicated(events_timeline$date)) # 1, mar 18 has two events

# summary stats
summary(events_timeline)

#--------- PREP ---------#
# short name copy with real dates and a category factor
events = events_timeline
events$date = as.Date(events$date)
events$category = factor(events$category)
events = events[order(events$date), ]

# group sizes
table(events$category) # 3 energy infrastructure, 3 policy response, 2 price record, 1 each of the other three

# event titles by category
split(events$event_title, events$category)

#--------- CHARTS AND PLOTS ---------#
# only two numeric columns here
events_num = events[, c('war_day', 'brent_price_that_day')]

#--------- HISTOGRAMS ---------#
# histogram for each numeric column
for (var in names(events_num)) {
  hist(events_num[, var], main = paste('Histogram of', var), xlab = var)
}
# 11 rows only, so these show the shape of the timeline, not a distribution to trust

#--------- BOX PLOTS ---------#
# box plot for each numeric column
for (var in names(events_num)) {
  boxplot(events_num[, var], main = paste('Box Plot of', var), ylab = var)
}

# categorical box plot
# brent by event category
boxplot(brent_price_that_day ~ category, data = events, las = 2,
        main = 'Brent Price on Event Days by Category',
        xlab = '',
        ylab = 'brent (dollars per barrel)')
# four of the six categories have one event each, so the boxes are single points

# category medians
aggregate(brent_price_that_day ~ category, data = events, FUN = median)
# energy infrastructure 108.65 is the highest, conflict start 79.50 the lowest

#--------- SCATTER PLOTS ---------#
# brent on event days against the day of the war
plot(events$war_day, events$brent_price_that_day, pch = 16,
     main = 'Brent Price on Event Days vs. War Day',
     xlab = 'war day',
     ylab = 'brent (dollars per barrel)')
abline(lm(events$brent_price_that_day ~ events$war_day), col = 'red', lty = 2)

#--------- CORRELATIONS ---------#
round(cor(events_num), 2)
cor(events$war_day, events$brent_price_that_day) # 0.9669
# brent climbs almost straight through the timeline, so war day alone tracks it

#--------- EVENTS JOINED TO THE DAILY PRICES ---------#
# bring the daily prices onto the event dates
merged = merge(events[, c('date', 'war_day', 'event_title', 'category', 'brent_price_that_day')],
               daily[, c('date', 'phase', 'brent_usd_barrel', 'us_gas_avg_gallon')],
               by = 'date')
dim(merged) # 11 rows, mar 18 appears twice because two events share that date

# the brent price agrees between the two files
max(abs(merged$brent_price_that_day - merged$brent_usd_barrel)) # 0

# events by category and phase
table(merged$category, merged$phase)
# no events fall in the pre-war phase, every event is day 1 or later

# gas price on event days
merged[order(-merged$us_gas_avg_gallon), c('date', 'event_title', 'us_gas_avg_gallon')]

#--------- CLEANED DATA FILES ---------#
# final checks
dim(daily) # 24 rows 17 cols
dim(events) # 11 rows 7 cols

# this creates the cleaned data files
write.csv(clean, 'gas_clean.csv', row.names = FALSE)
write.csv(daily, 'daily_clean.csv', row.names = FALSE)
write.csv(events, 'events_clean.csv', row.names = FALSE)

