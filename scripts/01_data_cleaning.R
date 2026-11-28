
# =============================================================================
# Project: Oil and Gasoline Price Analysis in 2026
# Script:  01_data_cleaning.R
# Authors: 
#
# Purpose:
#   Import, validate, clean, and prepare the datasets for statistical analysis.
#
# Note:
#   Run this script from the repository root. All prices are in USD.
# =============================================================================


# 1. Create output directories 

dir.create("data/cleaned", recursive = TRUE, showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)


# 2. Import datasets 

states <- read.csv("data/raw/iran_war_gas_prices_by_state.csv")
oil    <- read.csv("data/raw/iran_war_oil_prices_daily_2026.csv")
events <- read.csv("data/raw/iran_war_key_events_timeline.csv")


# 3. Validate state-level data 

# Check the number of states, duplicate records, and missing values
stopifnot(
  nrow(states) == 50,
  !anyDuplicated(states$state),
  !anyNA(states)
)


# 4. Prepare state-level variables 

# Gasoline price before the conflict
states$prewar <- states$gas_price_prewar_feb27

# Dollar increase in gasoline prices
states$increase <- states$gas_price_mar19_2026 - states$prewar

# Percentage increase in gasoline prices
states$pct <- (states$increase / states$prewar) * 100

# Gasoline price change before the conflict
states$pre_trend <- states$prewar - states$gas_price_jan08_2026

# Verify calculated values against the original dataset
stopifnot(
  max(abs(states$increase - states$price_increase_since_war)) < 1e-8,
  max(abs(states$pct - states$pct_increase_since_war)) <= 0.051
)

# Convert region into a categorical variable
states$region <- factor(states$region)


# 5. Create additional state characteristics 

# Identify states with ocean or Gulf coastal access
coast <- c(
  "Maine", "New Hampshire", "Massachusetts", "Rhode Island",
  "Connecticut", "New York", "New Jersey", "Delaware",
  "Maryland", "Virginia", "North Carolina", "South Carolina",
  "Georgia", "Florida", "Alabama", "Mississippi", "Louisiana",
  "Texas", "California", "Oregon", "Washington", "Alaska", "Hawaii"
)

states$coastal <- factor(
  ifelse(states$state %in% coast, "yes", "no")
)

# Match states with R's built-in state dataset
idx <- match(states$state, state.name)

# Calculate population density (people per square mile)
# Population estimates are from 1975
states$pop_density <- (
  state.x77[idx, "Population"] * 1000
) / state.x77[idx, "Area"]

# Log-transform population density
states$log_density <- log(states$pop_density)


# 6. Prepare oil price and event data 

# Convert dates to Date format
oil$date    <- as.Date(oil$date)
events$date <- as.Date(events$date)

# Sort oil price observations chronologically
oil <- oil[order(oil$date), ]

# Check for invalid dates and duplicate oil price dates
stopifnot(
  !anyNA(oil$date),
  !anyNA(events$date),
  !anyDuplicated(oil$date)
)

# Brent price change between consecutive observations
oil$change <- c(NA, diff(oil$brent_usd_barrel))

# Number of days between consecutive observations
oil$gap <- c(NA, as.numeric(diff(oil$date)))

# Percentage change between consecutive Brent observations
oil$observed_return <- c(
  NA,
  100 * diff(oil$brent_usd_barrel) /
    head(oil$brent_usd_barrel, -1)
)

# Identify dates associated with recorded events
oil$event <- oil$date %in% events$date


# 7. Merge event and oil price datasets 

events_cleaned <- merge(
  events,
  oil[, c("date", "change", "gap", "brent_usd_barrel")],
  by = "date",
  all.x = TRUE
)


# 8. Export cleaned datasets 

write.csv(
  states,
  "data/cleaned/states.csv",
  row.names = FALSE
)

write.csv(
  oil,
  "data/cleaned/oil.csv",
  row.names = FALSE
)

write.csv(
  events_cleaned,
  "data/cleaned/events.csv",
  row.names = FALSE
)


# 9. Data cleaning summary 

cat("\nData Cleaning Summary\n")
cat("----------------------------------------\n")
cat("Number of states:          ", nrow(states), "\n")
cat("Number of oil observations:", nrow(oil), "\n")
cat("Number of event records:   ", nrow(events), "\n")
cat("Missing state values:      ", sum(is.na(states)), "\n")
cat("Duplicate states:          ", sum(duplicated(states$state)), "\n")
cat("Duplicate oil dates:       ", sum(duplicated(oil$date)), "\n")
cat("----------------------------------------\n")
cat("Data cleaning completed successfully.\n")

# Additional integrity checks and regional classification audit
stopifnot(setequal(states$state, state.name),
          all(is.finite(states$pop_density)), all(states$pop_density > 0),
          all(is.finite(states$log_density)), !anyDuplicated(events))
price_cols <- c("gas_price_jan08_2026", "gas_price_prewar_feb27", "gas_price_mar19_2026")
stopifnot(all(is.finite(as.matrix(states[, price_cols]))), all(states[, price_cols] > 0))
oil_cols <- c("brent_usd_barrel", "wti_usd_barrel", "dubai_usd_barrel")
stopifnot(all(is.finite(as.matrix(oil[, oil_cols]))), all(oil[, oil_cols] > 0),
          all(diff(oil$date) > 0))
region_check <- as.character(state.region[match(states$state, state.name)])
region_check[region_check == "North Central"] <- "Midwest"
print(data.frame(state = states$state, original = states$region,
                 standard = region_check)[as.character(states$region) != region_check, ])
print(table(oil$gap, useNA = "ifany"))
print(events_cleaned[!events_cleaned$date %in% oil$date, ])
