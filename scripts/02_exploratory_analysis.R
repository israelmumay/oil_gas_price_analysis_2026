
# =============================================================================
# Project: Oil and Gasoline Price Analysis in 2026
# Script:  02_exploratory_analysis.R
# Authors: 
#
# Purpose:
#   Explore oil and gasoline prices using descriptive statistics,
#   regional comparisons, correlation analysis, and visualizations.
#
# Note:
#   Run 01_data_cleaning.R before executing this script.
# =============================================================================


# 1. Load required libraries 

library(ggplot2)


# 2. Import cleaned datasets 

states <- read.csv("data/cleaned/states.csv")
oil    <- read.csv("data/cleaned/oil.csv")

oil$date <- as.Date(oil$date)


# 3. State-level descriptive statistics 

# Summary of the main numerical variables
state_summary <- summary(
  states[, c("prewar", "increase", "pct", "pre_trend", "log_density")]
)

print(state_summary)

# Summary of gasoline price increases by region
region_summary <- aggregate(
  increase ~ region,
  data = states,
  FUN = function(x) {
    c(
      n = length(x),
      mean = mean(x),
      sd = sd(x)
    )
  }
)

print(region_summary)

# Rank states by gasoline price increase
state_ranking <- states[
  order(-states$increase),
  c("state", "region", "increase", "pct")
]

print(state_ranking)


# 4. Correlation analysis 

# Examine relationships between numerical state-level variables
correlation_matrix <- cor(
  states[, c("increase", "prewar", "pre_trend", "log_density")]
)

print(round(correlation_matrix, 4))


# 5. Oil price descriptive statistics 

# Rank oil price movements from largest to smallest
oil_movements <- oil[
  order(-oil$change),
  c("date", "change", "gap", "event")
]

print(oil_movements)

# Summary of crude oil benchmarks and percentage returns
oil_summary <- summary(
  oil[, c(
    "brent_usd_barrel",
    "wti_usd_barrel",
    "dubai_usd_barrel",
    "observed_return"
  )]
)

print(oil_summary)

# Compare price volatility before and after the conflict began
volatility_summary <- tapply(
  abs(oil$observed_return),
  oil$date >= as.Date("2026-02-28"),
  median,
  na.rm = TRUE
)

names(volatility_summary) <- c("Pre-War", "War Period")

print(volatility_summary)


# 6. Define visualization style 

# Consistent color palette for regional comparisons
region_colors <- c(
  "Midwest"   = "#427AA1",
  "Northeast" = "#159A9C",
  "South"     = "#E59B39",
  "West"      = "#785AA5"
)

# Main project colors
navy   <- "#10253F"
teal   <- "#087E84"
orange <- "#E59B39"
gray   <- "#64748B"

# Common theme for all project visualizations
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


# 7. Brent crude oil price trends 

oil_plot <- ggplot(oil, aes(x = date, y = brent_usd_barrel)) +
  geom_line(color = teal, linewidth = 1) +
  geom_point(color = teal, size = 2.5) +
  geom_vline(
    xintercept = as.Date("2026-02-28"),
    linetype = "dashed",
    color = orange
  ) +
  labs(
    title = "Brent Crude Oil Price Trends",
    subtitle = "Oil price movements before and during the conflict",
    x = "Date",
    y = "Brent Price ($/barrel)",
    caption = "Source: Iran War Oil Shock 2026 | Brent & Gas Tracker"
  ) +
  project_theme

print(oil_plot)


# 8. Pre-war gasoline price vs. dollar increase 

scatter_plot <- ggplot(
  states,
  aes(x = prewar, y = increase, color = region)
) +
  geom_point(size = 3, alpha = 0.85) +
  geom_smooth(
    aes(group = 1),
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    color = navy,
    fill = gray
  ) +
  scale_color_manual(values = region_colors) +
  labs(
    title = "Pre-War Gasoline Price vs. Dollar Increase",
    subtitle = "Relationship between initial prices and subsequent increases",
    x = "Pre-War Gasoline Price ($/gallon)",
    y = "Price Increase ($/gallon)",
    color = "Region",
    caption = "Source: AAA state gasoline prices via Kaggle"
  ) +
  project_theme

print(scatter_plot)


# 9. Pre-war gasoline price vs. percentage increase 

percent_plot <- ggplot(
  states,
  aes(x = prewar, y = pct, color = region)
) +
  geom_point(size = 3, alpha = 0.85) +
  geom_smooth(
    aes(group = 1),
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    color = navy,
    fill = gray
  ) +
  scale_color_manual(values = region_colors) +
  labs(
    title = "Pre-War Gasoline Price vs. Percentage Increase",
    subtitle = "Relative gasoline price changes across the 50 states",
    x = "Pre-War Gasoline Price ($/gallon)",
    y = "Price Increase (%)",
    color = "Region",
    caption = "Source: AAA state gasoline prices via Kaggle"
  ) +
  project_theme

print(percent_plot)


# 10. Gasoline price increases by region 

region_plot <- ggplot(
  states,
  aes(x = region, y = increase, fill = region)
) +
  geom_boxplot(alpha = 0.75, width = 0.6) +
  scale_fill_manual(values = region_colors) +
  labs(
    title = "Gasoline Price Increases by U.S. Region",
    subtitle = "Comparison of price changes across the four regions",
    x = "Region",
    y = "Price Increase ($/gallon)",
    caption = "Source: AAA state gasoline prices via Kaggle"
  ) +
  project_theme +
  theme(legend.position = "none")

print(region_plot)


# 11. Distribution of gasoline price increases 

distribution_plot <- ggplot(states, aes(x = increase)) +
  geom_histogram(
    binwidth = 0.04,
    fill = teal,
    color = "white"
  ) +
  geom_vline(
    xintercept = mean(states$increase),
    color = orange,
    linetype = "dashed",
    linewidth = 1
  ) +
  labs(
    title = "Distribution of Gasoline Price Increases",
    subtitle = "Dollar price changes across all 50 U.S. states",
    x = "Price Increase ($/gallon)",
    y = "Number of States",
    caption = "Dashed line represents the mean state price increase."
  ) +
  project_theme

print(distribution_plot)


# 12. Oil price movements and key events 

events_plot <- ggplot(
  oil,
  aes(x = date, y = change, fill = event)
) +
  geom_col(na.rm = TRUE) +
  geom_hline(yintercept = 0, color = gray) +
  scale_fill_manual(
    values = c("FALSE" = teal, "TRUE" = orange),
    breaks = c(FALSE, TRUE),
    labels = c("No Recorded Event", "Recorded Event")
  ) +
  labs(
    title = "Brent Oil Price Movements and Key Events",
    subtitle = "Price changes between consecutive available observations",
    x = "Date",
    y = "Brent Price Change ($/barrel)",
    fill = "Event Status",
    caption = "Source: Iran War Oil Shock 2026 | Brent & Gas Tracker"
  ) +
  project_theme

print(events_plot)


# 13. Export visualizations 

dir.create("figures", showWarnings = FALSE)

plots <- list(
  oil          = oil_plot,
  scatter      = scatter_plot,
  percent      = percent_plot,
  regions      = region_plot,
  distribution = distribution_plot,
  events       = events_plot
)

# Save each visualization as a high-resolution PNG file
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


# 14. Exploratory analysis summary 

cat("\nExploratory Data Analysis Summary\n")
cat("---------------------------------------------\n")
cat("Number of states:          ", nrow(states), "\n")
cat("Number of oil observations:", nrow(oil), "\n")
cat("Average price increase:    $", round(mean(states$increase), 2), "\n")
cat("Average percentage change: ", round(mean(states$pct), 2), "%\n")
cat("Visualizations generated:  ", length(plots), "\n")
cat("---------------------------------------------\n")
cat("Exploratory data analysis completed successfully.\n")

# Additional descriptive summaries from the audit
print(aggregate(cbind(increase, pct) ~ region, states, mean))
print(aggregate(cbind(increase, pct) ~ coastal, states, mean))
print(head(states[order(-states$pct), c("state", "region", "pct")], 5))
oil$period <- ifelse(oil$date < as.Date("2026-02-28"), "Pre-War", "Conflict")
print(aggregate(cbind(brent_usd_barrel, wti_usd_barrel, dubai_usd_barrel) ~ period,
                oil, function(x) c(n = length(x), mean = mean(x), sd = sd(x))))
events <- read.csv("data/cleaned/events.csv")
print(events)
