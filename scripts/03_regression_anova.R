
# =============================================================================
# Project: Oil and Gasoline Price Analysis in 2026
# Script:  03_regression_anova.R
# Authors: 
#
# Purpose:
#   Examine the factors associated with gasoline price increases using
#   linear regression, ANOVA, statistical inference, model comparison,
#   regression diagnostics, and sensitivity analysis.
#
# Note:
#   Run 01_data_cleaning.R before executing this script.
# =============================================================================


# 1. Load required libraries 

library(lmtest)
library(sandwich)
library(car)


# 2. Import cleaned dataset 

states <- read.csv("data/cleaned/states.csv")

# Convert categorical variables into factors
states$region  <- factor(states$region)
states$coastal <- factor(states$coastal)


# 3. Simple linear regression 

# Examine whether higher pre-war gasoline prices are associated
# with larger dollar increases.

base <- lm(
  increase ~ prewar,
  data = states
)


# 4. One-way analysis of variance (ANOVA) 

# Examine whether average gasoline price increases differ across
# the four U.S. regions.

region_only <- aov(
  increase ~ region,
  data = states
)


# 5. Multiple linear regression 

# Model 1: Pre-war gasoline price and region
adjusted <- lm(
  increase ~ prewar + region,
  data = states
)

# Model 2: Pre-war gasoline price and pre-war price trend
trend <- lm(
  increase ~ prewar + pre_trend,
  data = states
)

# Model 3: Include geographic characteristics
extended <- lm(
  increase ~ prewar + pre_trend + region + coastal,
  data = states
)

# Model 4: Include all available explanatory variables
full <- lm(
  increase ~ prewar + pre_trend + region + coastal + log_density,
  data = states
)


# 6. Interaction effects 
# Examine whether the relationship between pre-war gasoline prices
# and price increases differs across regions.

interaction <- lm(
  increase ~ prewar * region,
  data = states
)


# 7. Percentage increase regression 

# Examine whether states with higher initial gasoline prices
# experienced larger or smaller percentage increases.

percent <- lm(
  pct ~ prewar,
  data = states
)


# 8. Regression results and statistical inference 
# Organize regression models by name
models <- list(
  "Simple Linear Regression" = base,
  "Region-Adjusted Model"    = adjusted,
  "Pre-War Trend Model"       = trend,
  "Extended Model"           = extended,
  "Full Model"               = full,
  "Interaction Model"        = interaction,
  "Percentage Change Model"  = percent
)

# Display model summaries, confidence intervals,
# and heteroskedasticity-robust coefficient tests.

for (model_name in names(models)) {
  
  fit <- models[[model_name]]
  
  cat("\n============================================================\n")
  cat(model_name, "\n")
  cat("============================================================\n")
  
  # Regression summary
  cat("\nRegression Summary\n")
  print(summary(fit))
  
  # Standard 95% confidence intervals
  cat("\n95% Confidence Intervals\n")
  print(confint(fit, level = 0.95))
  
  # HC3 heteroskedasticity-robust standard errors
  robust_cov <- vcovHC(fit, type = "HC3")
  
  cat("\nRobust Coefficient Tests (HC3)\n")
  print(coeftest(fit, vcov. = robust_cov))
  
  # Robust 95% confidence intervals
  cat("\nRobust 95% Confidence Intervals (HC3)\n")
  print(coefci(fit, vcov. = robust_cov, level = 0.95))
}


# 9. Regional differences and hypothesis testing

# H0: Mean gasoline price increases are equal across all regions.
# H1: At least one regional mean is different.

# Classical one-way ANOVA
cat("\nOne-Way ANOVA\n")
print(anova(region_only))

# Brown-Forsythe test for equality of regional variances
cat("\nBrown-Forsythe Test\n")

variance_test <- leveneTest(
  increase ~ region,
  data = states,
  center = median
)

print(variance_test)

# Welch's ANOVA: does not assume equal regional variances
cat("\nWelch's ANOVA\n")

welch_anova <- oneway.test(
  increase ~ region,
  data = states,
  var.equal = FALSE
)

print(welch_anova)


# 10. Post-hoc regional comparisons 

# Pairwise Welch t-tests with Holm correction
cat("\nPairwise Welch Tests with Holm Adjustment\n")

pairwise_results <- pairwise.t.test(
  x = states$increase,
  g = states$region,
  p.adjust.method = "holm",
  pool.sd = FALSE
)

print(pairwise_results)

# Classical Tukey HSD for comparison
cat("\nTukey's HSD Test\n")

tukey_results <- TukeyHSD(
  region_only,
  conf.level = 0.95
)

print(tukey_results)


# 11. Nested model comparisons 

# Test whether region improves the simple regression model
cat("\nModel Comparison: Baseline vs. Region-Adjusted\n")
print(anova(base, adjusted))

# Test whether the interaction between gasoline price and region
# significantly improves the adjusted model
cat("\nModel Comparison: Adjusted vs. Interaction\n")
print(anova(adjusted, interaction))


# 12. Overall regression model comparison 

# Compare models that use dollar increase as the response.
# The percentage change model is excluded because its response differs.

comparison_models <- list(
  "Baseline"        = base,
  "Region-Adjusted" = adjusted,
  "Pre-War Trend"   = trend,
  "Extended"        = extended,
  "Full"            = full,
  "Interaction"     = interaction
)

model_comparison <- data.frame(
  Model = names(comparison_models),
  
  R_Squared = sapply(
    comparison_models,
    function(fit) summary(fit)$r.squared
  ),
  
  Adjusted_R_Squared = sapply(
    comparison_models,
    function(fit) summary(fit)$adj.r.squared
  ),
  
  Residual_SE = sapply(
    comparison_models,
    function(fit) summary(fit)$sigma
  ),
  
  AIC = sapply(comparison_models, AIC),
  
  BIC = sapply(comparison_models, BIC)
)

# Round numerical results for readability
model_comparison[, -1] <- round(model_comparison[, -1], 4)

rownames(model_comparison) <- NULL

cat("\nRegression Model Comparison\n")
print(model_comparison)


# 13. Multicollinearity assessment 
# Examine predictor overlap using variance inflation factors.
# For multi-level factors, car reports generalized VIF values.

cat("\nVariance Inflation Factors - Full Model\n")

vif_results <- vif(full)

print(vif_results)


# 14. Regression assumption tests 

# Shapiro-Wilk test for normality of residuals
cat("\nShapiro-Wilk Normality Test\n")

normality_test <- shapiro.test(
  residuals(base)
)

print(normality_test)

# Breusch-Pagan test for constant error variance
cat("\nBreusch-Pagan Test\n")

variance_assumption <- bptest(base)

print(variance_assumption)


# 15. Influential observations 
# Calculate Cook's distance for each state
states$cooks_d <- cooks.distance(base)

# Screening threshold for potentially influential observations
cooks_threshold <- 4 / nrow(states)

# Rank states by Cook's distance
influence_ranking <- states[
  order(-states$cooks_d),
  c("state", "region", "cooks_d")
]

cat("\nCook's Distance Ranking\n")
print(influence_ranking)

# Identify potentially influential states
influential_states <- states[
  states$cooks_d > cooks_threshold,
  c("state", "region", "increase", "prewar", "cooks_d")
]

cat("\nPotentially Influential States\n")
print(influential_states)


# 16. Sensitivity analysis 

# Refit the baseline model without potentially influential states.
# This is a sensitivity check, not a permanent removal of observations.

sensitivity <- lm(
  increase ~ prewar,
  data = states,
  subset = cooks_d <= cooks_threshold
)

cat("\nSensitivity Analysis\n")
print(summary(sensitivity))

# Compare the original and sensitivity models
sensitivity_comparison <- data.frame(
  Model = c("Original Model", "Sensitivity Model"),
  
  Observations = c(
    nobs(base),
    nobs(sensitivity)
  ),
  
  Intercept = c(
    coef(base)[1],
    coef(sensitivity)[1]
  ),
  
  Prewar_Coefficient = c(
    coef(base)[2],
    coef(sensitivity)[2]
  ),
  
  R_Squared = c(
    summary(base)$r.squared,
    summary(sensitivity)$r.squared
  ),
  
  Adjusted_R_Squared = c(
    summary(base)$adj.r.squared,
    summary(sensitivity)$adj.r.squared
  )
)

sensitivity_comparison[, -1] <- round(
  sensitivity_comparison[, -1],
  4
)

cat("\nSensitivity Model Comparison\n")
print(sensitivity_comparison)


# 17. Regression diagnostic plots 

dir.create("figures", showWarnings = FALSE)

# Display the four standard regression diagnostic plots
old_par <- par(mfrow = c(2, 2))

plot(
  base,
  which = c(1, 2, 3, 5),
  col = "#087E84",
  pch = 19
)

par(old_par)

# Export the diagnostic plots in high resolution
png(
  filename = "figures/R_diagnostics.png",
  width = 12,
  height = 9,
  units = "in",
  res = 300,
  bg = "white"
)

par(mfrow = c(2, 2))

plot(
  base,
  which = c(1, 2, 3, 5),
  col = "#087E84",
  pch = 19
)

dev.off()


# 18. Cook's distance visualization 

png(
  filename = "figures/R_cooks_distance.png",
  width = 12,
  height = 7,
  units = "in",
  res = 300,
  bg = "white"
)

plot(
  states$cooks_d,
  type = "h",
  lwd = 2,
  col = "#087E84",
  main = "Cook's Distance by State",
  xlab = "Observation",
  ylab = "Cook's Distance"
)

# Add the screening threshold
abline(
  h = cooks_threshold,
  col = "#E59B39",
  lty = 2,
  lwd = 2
)

# Label potentially influential observations
influential_index <- which(
  states$cooks_d > cooks_threshold
)

text(
  x = influential_index,
  y = states$cooks_d[influential_index],
  labels = states$state[influential_index],
  pos = 4,
  cex = 0.75,
  xpd = TRUE
)

dev.off()


# 19. Export statistical results 

dir.create(
  "data/cleaned",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  model_comparison,
  "data/cleaned/model_comparison.csv",
  row.names = FALSE
)

write.csv(
  sensitivity_comparison,
  "data/cleaned/sensitivity_comparison.csv",
  row.names = FALSE
)

write.csv(
  influence_ranking,
  "data/cleaned/influence_ranking.csv",
  row.names = FALSE
)


# 20. Statistical analysis summary 

cat("\nRegression and ANOVA Summary\n")
cat("--------------------------------------------------\n")

cat("Number of states:          ", nrow(states), "\n")
cat("Regression models fitted:  ", length(models), "\n")

cat(
  "Baseline R-squared:        ",
  round(summary(base)$r.squared, 4), "\n"
)

cat(
  "Baseline Adjusted R-squared:",
  round(summary(base)$adj.r.squared, 4), "\n"
)

cat(
  "Welch ANOVA p-value:       ",
  round(welch_anova$p.value, 6), "\n"
)

cat(
  "Influential observations:  ",
  nrow(influential_states), "\n"
)

cat("--------------------------------------------------\n")
cat("Regression and ANOVA completed successfully.\n")
