
# =============================================================================
# Project: Oil and Gasoline Price Analysis in 2026
# Script:  04_forecasting.R
# Authors: 
#
# Purpose:
#   Develop and evaluate a weekly gasoline price forecasting model
#   using historical Brent crude oil prices, lagged price changes,
#   and conflict-period information.
#
# Data Source:
#   Federal Reserve Economic Data (FRED)
#
# Note:
#   This analysis requires an active internet connection.
#   Run this script from the repository root.
# =============================================================================


# 1. Load required libraries 

library(quantmod)
library(lmtest)
library(sandwich)
library(ggplot2)


# 2. Define analysis parameters 

start_date <- as.Date("2025-01-01")
end_date   <- as.Date("2026-10-31")
war_date   <- as.Date("2026-02-28")

# Number of weekly observations reserved for model evaluation
test_size <- 12


# 3. Download historical data from FRED 

# Daily Brent crude oil prices (USD per barrel)
br <- getSymbols(
  Symbols = "DCOILBRENTEU",
  src = "FRED",
  from = start_date,
  to = end_date,
  auto.assign = FALSE
)

# Weekly U.S. regular gasoline prices (USD per gallon)
ga <- getSymbols(
  Symbols = "GASREGW",
  src = "FRED",
  from = start_date,
  to = end_date,
  auto.assign = FALSE
)


# 4. Prepare historical datasets 

# Convert Brent observations into a data frame
b <- data.frame(
  date = as.Date(index(br)),
  brent = as.numeric(br)
)

# Convert gasoline observations into a data frame
g <- data.frame(
  date = as.Date(index(ga)),
  gas = as.numeric(ga)
)

# Remove missing observations
b <- na.omit(b)
g <- na.omit(g)

# Restrict both datasets to the selected analysis period
b <- b[b$date >= start_date & b$date <= end_date, ]
g <- g[g$date >= start_date & g$date <= end_date, ]

# Sort observations chronologically
b <- b[order(b$date), ]
g <- g[order(g$date), ]

# Validate dates and price observations
stopifnot(
  !anyDuplicated(b$date),
  !anyDuplicated(g$date),
  all(b$brent > 0),
  all(g$gas > 0),
  all(as.POSIXlt(g$date)$wday == 1)
)

cat("\nHistorical Data Summary\n")
cat("---------------------------------------------\n")
cat("Daily Brent observations: ", nrow(b), "\n")
cat("Weekly gasoline observations:", nrow(g), "\n")
cat("Start date:", format(min(g$date)), "\n")
cat("End date:  ", format(max(g$date)), "\n")


# 5. Compare FRED and Kaggle oil prices 

# Load the original Kaggle oil price dataset
k <- read.csv(
  "data/raw/iran_war_oil_prices_daily_2026.csv"
)

k$date <- as.Date(k$date)

# Compare Brent prices on overlapping dates.
# The two sources are not combined into a single series.

source_comparison <- merge(
  k[, c("date", "brent_usd_barrel")],
  b,
  by = "date"
)

names(source_comparison) <- c(
  "date",
  "kaggle_brent",
  "fred_brent"
)

source_comparison$difference <- (
  source_comparison$kaggle_brent -
    source_comparison$fred_brent
)

cat("\nComparison of Kaggle and FRED Brent Prices\n")
print(source_comparison)


# 6. Aggregate daily Brent prices into weekly averages 

# Use only Brent observations dated on or before the Monday
# associated with each weekly gasoline observation.

g$brent <- sapply(
  g$date,
  function(day) {
    
    day <- as.Date(day, origin = "1970-01-01")
    
    weekly_prices <- b$brent[
      b$date > day - 7 &
        b$date <= day
    ]
    
    if (length(weekly_prices) > 0) {
      mean(weekly_prices)
    } else {
      NA_real_
    }
  }
)


# 7. Feature engineering 

# Weekly percentage change in Brent crude oil prices
g$brent_change <- c(
  NA,
  100 * diff(g$brent) / head(g$brent, -1)
)

# Previous week's Brent percentage change
g$lag_brent <- c(
  NA,
  head(g$brent_change, -1)
)

# Weekly percentage change in gasoline prices
g$gas_change <- c(
  NA,
  100 * diff(g$gas) / head(g$gas, -1)
)

# Response variable: next week's gasoline percentage change
g$next_gas_change <- c(
  100 * diff(g$gas) / head(g$gas, -1),
  NA
)

# Binary indicator for the conflict period
# 0 = Before the conflict
# 1 = During the conflict period
g$war <- as.integer(g$date >= war_date)

# Date associated with the forecasting target
g$target_date <- c(
  g$date[-1],
  as.Date(NA)
)

# Confirm consecutive weekly observations for lagged predictors
g$previous_gap <- c(
  NA,
  as.numeric(diff(g$date))
)

g$lag_gap <- c(
  NA,
  head(g$previous_gap, -1)
)

# Retain observations with valid consecutive weekly intervals
g <- g[
  !is.na(g$target_date) &
    g$target_date - g$date == 7 &
    !is.na(g$previous_gap) &
    g$previous_gap == 7 &
    !is.na(g$lag_gap) &
    g$lag_gap == 7,
]

# Remove incomplete observations created by lagging
g <- na.omit(g)

# Confirm sufficient observations for model development
stopifnot(nrow(g) > 30)


# 8. Create training and testing datasets 

# Use a chronological split to preserve the time-series structure.
# The most recent 12 observations are reserved for testing.

train <- head(g, -test_size)
test  <- tail(g, test_size)

stopifnot(
  nrow(test) == test_size,
  max(train$target_date) <= min(test$date)
)

cat("\nTraining and Testing Summary\n")
cat("---------------------------------------------\n")
cat("Total observations:   ", nrow(g), "\n")
cat("Training observations:", nrow(train), "\n")
cat("Testing observations: ", nrow(test), "\n")

cat(
  "Training period:",
  format(min(train$date)), "to",
  format(max(train$target_date)), "\n"
)

cat(
  "Testing period: ",
  format(min(test$target_date)), "to",
  format(max(test$target_date)), "\n"
)


# 9. Fit the forecasting model 

# Predict next week's gasoline percentage change using:
#   - Current Brent percentage change
#   - Previous Brent percentage change
#   - Conflict-period indicator
#   - Interaction between Brent change and conflict period
#
# The model specification is fixed before evaluating the test set.

fit <- lm(
  next_gas_change ~ brent_change +
    lag_brent +
    war +
    brent_change:war,
  data = train
)

cat("\nForecasting Model Summary\n")
print(summary(fit))


# 10. Statistical inference 

# Standard 95% confidence intervals
cat("\n95% Confidence Intervals\n")
print(confint(fit, level = 0.95))

# Newey-West standard errors account for potential
# heteroskedasticity and autocorrelation.

robust_cov <- NeweyWest(fit)

cat("\nNewey-West Robust Coefficient Tests\n")
print(coeftest(fit, vcov. = robust_cov))

cat("\nNewey-West Robust Confidence Intervals\n")
print(
  coefci(
    fit,
    vcov. = robust_cov,
    level = 0.95
  )
)


# 11. Test for residual autocorrelation 

# Breusch-Godfrey test for serial correlation
# H0: No serial correlation up to lag 2.
# H1: Serial correlation exists.

autocorrelation_test <- bgtest(
  fit,
  order = 2
)

cat("\nBreusch-Godfrey Autocorrelation Test\n")
print(autocorrelation_test)


# 12. Generate holdout forecasts 

# Predict weekly gasoline percentage changes
test$forecast <- predict(
  fit,
  newdata = test
)

# Naive benchmark: assume no change in gasoline prices
test$naive_forecast <- 0

# Calculate forecast errors
test$forecast_error <- (
  test$next_gas_change - test$forecast
)

# Convert predicted percentage changes into gasoline prices
test$predicted_gas <- (
  test$gas * (1 + test$forecast / 100)
)

# Calculate actual next-week gasoline prices
test$actual_gas <- (
  test$gas * (1 + test$next_gas_change / 100)
)

# Naive gasoline price forecast
test$naive_gas <- test$gas


# 13. Evaluate forecasting performance 

# Define forecast accuracy metrics
metrics <- function(actual, predicted) {
  
  rmse <- sqrt(
    mean((actual - predicted)^2)
  )
  
  mae <- mean(
    abs(actual - predicted)
  )
  
  c(
    RMSE = rmse,
    MAE = mae
  )
}

# Compare the regression model against the naive benchmark
forecast_performance <- rbind(
  "Regression Model" = metrics(
    test$next_gas_change,
    test$forecast
  ),
  
  "Naive Benchmark" = metrics(
    test$next_gas_change,
    test$naive_forecast
  )
)

cat("\nForecast Performance: Percentage Change\n")
print(round(forecast_performance, 4))

# Evaluate forecasts in dollars per gallon
price_performance <- rbind(
  "Regression Model" = metrics(
    test$actual_gas,
    test$predicted_gas
  ),
  
  "Naive Benchmark" = metrics(
    test$actual_gas,
    test$naive_gas
  )
)

cat("\nForecast Performance: Dollars per Gallon\n")
print(round(price_performance, 4))


# 14. Calculate forecast improvement 

# Percentage reduction in forecast error relative to the benchmark
rmse_improvement <- 100 * (
  1 - forecast_performance[1, "RMSE"] /
    forecast_performance[2, "RMSE"]
)

mae_improvement <- 100 * (
  1 - forecast_performance[1, "MAE"] /
    forecast_performance[2, "MAE"]
)

cat("\nForecast Improvement vs. Naive Benchmark\n")

cat(
  "RMSE improvement:",
  round(rmse_improvement, 2), "%\n"
)

cat(
  "MAE improvement: ",
  round(mae_improvement, 2), "%\n"
)


# 15. Define visualization style 

navy   <- "#10253F"
teal   <- "#087E84"
orange <- "#E59B39"
gray   <- "#64748B"

project_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 17,
      color = navy
    ),
    plot.subtitle = element_text(
      size = 11,
      color = gray
    ),
    plot.caption = element_text(
      size = 9,
      color = gray,
      hjust = 0
    ),
    axis.title = element_text(
      face = "bold",
      color = navy
    ),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.background = element_rect(
      fill = "white",
      color = NA
    )
  )


# 16. Historical gasoline price visualization 

historical_plot <- ggplot(g, aes(x = date, y = gas)) +
  geom_line(color = teal, linewidth = 1) +
  geom_vline(
    xintercept = war_date,
    color = orange,
    linetype = "dashed"
  ) +
  labs(
    title = "Historical U.S. Gasoline Prices",
    subtitle = "Weekly regular gasoline prices during the analysis period",
    x = "Date",
    y = "Gasoline Price ($/gallon)",
    caption = "Source: FRED - GASREGW"
  ) +
  project_theme

print(historical_plot)


# 17. Actual vs. predicted gasoline price changes 

forecast_plot <- ggplot(test, aes(x = target_date)) +
  geom_line(
    aes(y = next_gas_change, color = "Actual"),
    linewidth = 1.1
  ) +
  geom_point(
    aes(y = next_gas_change, color = "Actual"),
    size = 2.5
  ) +
  geom_line(
    aes(y = forecast, color = "Predicted"),
    linewidth = 1.1,
    linetype = "dashed"
  ) +
  geom_line(
    aes(y = naive_forecast, color = "Naive Benchmark"),
    linewidth = 0.8,
    linetype = "dotted"
  ) +
  scale_color_manual(
    values = c(
      "Actual" = teal,
      "Predicted" = orange,
      "Naive Benchmark" = gray
    )
  ) +
  labs(
    title = "Actual vs. Predicted Gasoline Price Changes",
    subtitle = "Out-of-sample evaluation over the final 12 weeks",
    x = "Forecast Date",
    y = "Weekly Price Change (%)",
    color = NULL,
    caption = "Source: FRED | Linear regression forecasting model"
  ) +
  project_theme

print(forecast_plot)


# 18. Actual vs. predicted gasoline prices 

price_plot <- ggplot(test, aes(x = target_date)) +
  geom_line(
    aes(y = actual_gas, color = "Actual"),
    linewidth = 1.1
  ) +
  geom_point(
    aes(y = actual_gas, color = "Actual"),
    size = 2.5
  ) +
  geom_line(
    aes(y = predicted_gas, color = "Predicted"),
    linewidth = 1.1,
    linetype = "dashed"
  ) +
  geom_line(
    aes(y = naive_gas, color = "Naive Benchmark"),
    linewidth = 0.8,
    linetype = "dotted"
  ) +
  scale_color_manual(
    values = c(
      "Actual" = teal,
      "Predicted" = orange,
      "Naive Benchmark" = gray
    )
  ) +
  labs(
    title = "Actual vs. Predicted U.S. Gasoline Prices",
    subtitle = "One-week-ahead gasoline price forecasts",
    x = "Forecast Date",
    y = "Gasoline Price ($/gallon)",
    color = NULL,
    caption = "Source: FRED | Linear regression forecasting model"
  ) +
  project_theme

print(price_plot)


# 19. Forecast performance visualization 

performance_data <- data.frame(
  Model = rep(
    rownames(forecast_performance),
    times = 2
  ),
  
  Metric = rep(
    c("RMSE", "MAE"),
    each = 2
  ),
  
  Error = as.vector(forecast_performance)
)

performance_plot <- ggplot(
  performance_data,
  aes(x = Metric, y = Error, fill = Model)
) +
  geom_col(
    position = "dodge",
    width = 0.65
  ) +
  geom_text(
    aes(label = round(Error, 3)),
    position = position_dodge(width = 0.65),
    vjust = -0.5,
    size = 4
  ) +
  scale_fill_manual(
    values = c(
      "Regression Model" = teal,
      "Naive Benchmark" = gray
    )
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Forecasting Model Performance",
    subtitle = "Regression model compared with the naive benchmark",
    x = "Performance Metric",
    y = "Forecast Error (Percentage Points)",
    fill = NULL,
    caption = "Lower RMSE and MAE indicate better forecasting accuracy."
  ) +
  project_theme

print(performance_plot)


# 20. Forecasting model diagnostics 

dir.create("figures", showWarnings = FALSE)

# Export standard regression diagnostic plots
png(
  filename = "figures/R_forecast_diagnostics.png",
  width = 12,
  height = 9,
  units = "in",
  res = 300,
  bg = "white"
)

par(mfrow = c(2, 2))

plot(
  fit,
  which = c(1, 2, 3, 5),
  col = teal,
  pch = 19
)

dev.off()

# Export the residual autocorrelation plot
png(
  filename = "figures/R_forecast_acf.png",
  width = 10,
  height = 6,
  units = "in",
  res = 300,
  bg = "white"
)

acf(
  residuals(fit),
  main = "Autocorrelation of Forecasting Model Residuals",
  col = teal,
  lwd = 2
)

dev.off()


# 21. Export forecasting visualizations 

plots <- list(
  historical_gasoline = historical_plot,
  forecast_changes    = forecast_plot,
  forecast_prices     = price_plot,
  forecast_metrics    = performance_plot
)

for (nm in names(plots)) {
  
  ggsave(
    filename = paste0("figures/R_", nm, ".png"),
    plot = plots[[nm]],
    width = 10,
    height = 6,
    units = "in",
    dpi = 300,
    bg = "white"
  )
}


# 22. Export forecasting results 

dir.create(
  "data/cleaned",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  g,
  "data/cleaned/weekly_energy_prices.csv",
  row.names = FALSE
)

write.csv(
  source_comparison,
  "data/cleaned/source_comparison.csv",
  row.names = FALSE
)

write.csv(
  test,
  "data/cleaned/forecast_holdout.csv",
  row.names = FALSE
)

write.csv(
  forecast_performance,
  "data/cleaned/forecast_performance.csv"
)

write.csv(
  price_performance,
  "data/cleaned/forecast_price_performance.csv"
)


# 23. Forecasting analysis summary 

cat("\nForecasting Analysis Summary\n")
cat("--------------------------------------------------\n")
cat("Total weekly observations:", nrow(g), "\n")
cat("Training observations:    ", nrow(train), "\n")
cat("Testing observations:     ", nrow(test), "\n")

cat(
  "Training R-squared:        ",
  round(summary(fit)$r.squared, 4), "\n"
)

cat(
  "Regression RMSE:          ",
  round(forecast_performance[1, "RMSE"], 4), "\n"
)

cat(
  "Regression MAE:           ",
  round(forecast_performance[1, "MAE"], 4), "\n"
)

cat(
  "Naive benchmark RMSE:     ",
  round(forecast_performance[2, "RMSE"], 4), "\n"
)

cat(
  "Naive benchmark MAE:      ",
  round(forecast_performance[2, "MAE"], 4), "\n"
)

cat(
  "RMSE improvement:         ",
  round(rmse_improvement, 2), "%\n"
)

cat(
  "MAE improvement:          ",
  round(mae_improvement, 2), "%\n"
)

cat("--------------------------------------------------\n")
cat("Forecasting analysis completed successfully.\n")
