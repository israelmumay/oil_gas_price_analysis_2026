#------------------------ PREPROCESSING ------------------------#

#--------- LIBRAIRIES ---------#
library(tidyverse)
library(lawstat)

#--------- LOAD DATA ---------#
# load state gas prices
state_gas = read.csv('iran_war_gas_prices_by_state.csv')

# # load daily prices
# daily_prices = read.csv('iran_war_oil_prices_daily_2026.csv')
# 
# # load events timeline
# events_timeline = read.csv('iran_war_key_events_timeline.csv')

#--------- DATA PREP ---------#
# coastal states
coast = c('Maine', 'New Hampshire', 'Massachusetts', 'Rhode Island', 
          'Connecticut','New York', 'New Jersey', 'Delaware', 'Maryland', 
          'Virginia','North Carolina', 'South Carolina', 'Georgia', 'Florida', 
          'Alabama','Mississippi', 'Louisiana', 'Texas', 'California', 'Oregon',
          'Washington', 'Alaska', 'Hawaii')

# population density from built-in state data (1975 population)
idx = match(state_gas$state, state.name)
density = unname(state.x77[idx, 'Population'] * 1000 / state.x77[idx, 'Area'])

# create a cleaned dataset with short names
clean = data.frame(
  state = state_gas$state,
  region = factor(state_gas$region),
  coastal = factor(ifelse(state_gas$state %in% coast, 'yes', 'no')),
  jan08 = state_gas$gas_price_jan08_2026,
  prewar = state_gas$gas_price_prewar_feb27,
  mar19 = state_gas$gas_price_mar19_2026,
  increase = state_gas$price_increase_since_war,
  pct = state_gas$pct_increase_since_war
)

#--------- FEATURE ENGINEERING ---------#
# engineered features
clean$pre_trend = clean$prewar - clean$jan08 # very weak predictor
clean$pop_density = density # ppl per sq mile

