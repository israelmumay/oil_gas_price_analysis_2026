# Oil and Gas Price Analysis in 2026

**Group:** Adam Mills, Israel Kakumba Mumay, Ryan Norton Stein

## Project Overview

The purpose of this project is to study how oil and gasoline prices changed during the 2026 Iran war. Gasoline prices increased in all 50 U.S. states, but some states experienced larger increases than others.

We use R to explore the data, study the relationships between variables, compare regional differences, and build statistical models to explain gasoline price increases.

The project is divided into three parts:

- Group 1: Data preparation and exploratory data analysis.
- Group 2: Linear regression, ANOVA, and model selection.
- Group 3: Time-series analysis and forecasting.

## Research Questions

1. How did oil prices change before and during the war?
2. How much did gasoline prices increase across the 50 states?
3. Is there a relationship between pre-war gasoline prices and the dollar and percentage increases?
4. Are there significant differences in gasoline price increases across regions?
5. Do major events during the conflict line up with changes in oil prices?
6. How sensitive are our results to influential observations and different model choices?

## Data Sources

The main datasets come from the following Kaggle dataset:

[Iran War Oil Shock 2026: Brent and Gas Tracker](https://www.kaggle.com/datasets/alitaqishah/iran-war-oil-shock-2026-brent-and-gas-tracker)

We use three CSV files:

| Dataset | Description |
|---|---|
| `iran_war_gas_prices_by_state.csv` | Gasoline prices across all 50 U.S. states on January 8, February 27, and March 19, 2026. |
| `iran_war_oil_prices_daily_2026.csv` | 24 recorded observations of Brent, WTI, and Dubai oil prices between February 9 and March 20, 2026. |
| `iran_war_key_events_timeline.csv` | 11 records of major events during the conflict. |

The state gasoline prices in the dataset are attributed to AAA. We also use R's built-in `state.x77` dataset to calculate population density.

## Variables

| Variable | Description |
|---|---|
| `increase` | March 19 price minus February 27 price, in dollars per gallon. This is the main response variable. |
| `pct` | Percentage increase relative to the pre-war price. |
| `prewar` | Gasoline price on February 27. |
| `pre_trend` | Gasoline price change from January 8 to February 27. |
| `region` | Midwest, Northeast, South, or West. |
| `coastal` | Whether a state has access to an ocean or the Gulf. |
| `pop_density` | Population per square mile, based on 1975 data. |
| `log_density` | Natural log of population density. |

## Group 1: Data Preparation and Exploratory Analysis

We checked the datasets for missing values, duplicates, inconsistent values, and incorrect calculations. We also created new variables for the regression analysis.

The state dataset contains 50 observations, with no missing values or duplicate states. The calculated dollar and percentage increases agree with the original data, allowing for rounding differences.

### Main Findings

- Gasoline prices increased in all 50 states.
- The average increase was $0.8062 per gallon, or approximately 26.78%.
- Increases ranged from $0.68 to $1.05 per gallon.
- 36 of the 50 states experienced increases between $0.76 and $0.84.
- Pre-war gasoline price had the strongest observed correlation with the dollar increase (r = 0.8913).
- Pre-war trend had a moderate positive correlation with the increase (r = 0.5943).
- Log population density had a weak correlation with the increase (r = 0.0877).
- Hawaii had the largest dollar increase ($1.05), while Kentucky had the largest percentage increase (30.99%).
- Oregon and Kentucky tied for the fifth-largest dollar increase ($0.88).

### Regional Differences

| Region | Average Dollar Increase | Average Percentage Increase |
|---|---:|---:|
| Midwest | $0.7617 | 27.31% |
| Northeast | $0.8182 | 26.04% |
| South | $0.7743 | 28.11% |
| West | $0.8715 | 25.50% |

The West experienced the largest average dollar increase, while the South experienced the largest average percentage increase.

### Oil Prices

| Oil Type | Pre-war Average | Conflict-period Average |
|---|---:|---:|
| Brent | $70.88 | $97.96 |
| WTI | $65.58 | $89.63 |
| Dubai | $69.95 | $97.42 |

All three oil prices were higher during the conflict period. These averages are based on recorded observations rather than equally spaced daily measurements.

## Group 2: Linear Regression and ANOVA

We used the following statistical methods:

- Simple linear regression
- Multiple linear regression
- t-tests and F-tests
- Confidence intervals
- One-way ANOVA
- Brown-Forsythe test
- Welch's ANOVA
- Tukey and pairwise Welch comparisons
- Interaction models
- Model selection using AIC and BIC
- Five-fold cross-validation
- Regression diagnostics
- Variance Inflation Factor (VIF)
- HC3 robust standard errors
- Cook's distance and sensitivity analysis

### Simple Linear Regression

The dollar increase model is:

`Increase = 0.342287 + 0.153107 × Prewar`

The percentage increase model is:

`Percentage Increase = 36.18456 - 3.10297 × Prewar`

| Model | R-squared | Adjusted R-squared |
|---|---:|---:|
| Dollar increase | 0.7944 | 0.7901 |
| Percentage increase | 0.6028 | 0.5945 |

States with higher pre-war gasoline prices experienced larger dollar increases but smaller percentage increases.

### ANOVA Results

| Test | F-statistic | p-value |
|---|---:|---:|
| Classical ANOVA | 9.2653 | 0.0000663 |
| Brown-Forsythe | 5.4465 | 0.002735 |
| Welch's ANOVA | 9.4091 | 0.0002817 |

The Brown-Forsythe test showed that the equal variance assumption was not met. Because of this, we used Welch's ANOVA as our main test for regional differences.

The results showed significant differences in average gasoline price increases across regions. After controlling for pre-war prices, the additive regional effect was no longer significant. However, the robust interaction test suggests that the relationship between pre-war prices and price increases may differ by region.

### Model Comparison

| Model | R-squared | Adjusted R-squared | CV RMSE |
|---|---:|---:|---:|
| Baseline | 0.7944 | 0.7901 | 0.038038 |
| Region-adjusted | 0.8050 | 0.7876 | 0.040531 |
| Trend | 0.8499 | 0.8435 | 0.028611 |
| Extended | 0.8703 | 0.8522 | 0.028226 |
| Full | 0.8735 | 0.8524 | 0.029003 |
| Interaction | 0.8254 | 0.7963 | 0.041973 |

### Final Selected Model

We selected the Trend model:

`Increase = 0.271419 + 0.210821 × Prewar - 0.288586 × Pre_trend`

| Performance Measure | Result |
|---|---:|
| R-squared | 0.8499 |
| Adjusted R-squared | 0.8435 |
| AIC | -209.3433 |
| BIC | -201.6952 |
| Five-fold CV RMSE | 0.028611 |
| Five-fold CV MAE | 0.019444 |

We selected this model because it has only two predictors, the lowest AIC and BIC, and good cross-validation performance. It explains approximately 85% of the observed variation in gasoline price increases.

### Model Diagnostics

The residuals did not fully meet the normality assumption. However, the Breusch-Pagan test did not show significant evidence of unequal variance.

We used HC3 robust standard errors as an additional precaution. We also checked influential observations using Cook's distance. Removing influential states changed the coefficient estimates but did not change their directions.

We kept all 50 states in the final model.

## Group 3: Time-Series Analysis and Forecasting

This part focuses on the relationship between crude oil and gasoline prices over time and the prediction of future gasoline prices.

The forecasting scripts and related outputs are kept separately from the Group 1 and Group 2 analysis. This section will be finalized as part of Group 3.

## Project Structure

| File or Folder | Description |
|---|---|
| `data/raw/` | Original datasets |
| `data/cleaned/` | Cleaned datasets and statistical results |
| `figures/` | Plots, diagnostics, and model figures |
| `reports/` | Project reports and statistical outputs |
| `scripts/01_data_cleaning.R` | Data cleaning and validation |
| `scripts/02_exploratory_analysis.R` | Exploratory data analysis |
| `scripts/03_regression_anova.R` | Regression, ANOVA, and model selection |
| `scripts/04_forecasting.R` | Forecasting analysis |
| `scripts/preprocessing.R` | Additional data preparation |
| `scripts/eda.R` | Additional exploratory analysis |
| `scripts/modeling.R` | Additional statistical modeling |
| `run_groups12.R` | Runs the Group 1 and Group 2 analysis |
| `run_analysis.R` | Main analysis runner |
| `LM_project.qmd` | Quarto report |
| `LM_project.pdf` | Rendered report |
| `README.md` | Project documentation |

## Software and Tools

- R
- RStudio
- Quarto
- Git and GitHub

The main R packages used for Groups 1 and 2 include:

- tidyverse
- ggplot2
- car
- lawstat
- lmtest
- sandwich
- knitr

## How to Run Groups 1 and 2

Open the RStudio project and make sure the working directory is the repository root.

Install the required packages if they are not already installed:

```r
packages <- c(
  "tidyverse", "ggplot2", "car", "lawstat",
  "lmtest", "sandwich", "knitr", "quarto"
)

missing_packages <- packages[
  !sapply(packages, requireNamespace, quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}
```

Run the analysis:

```r
source("run_groups12.R")
```

Render the report:

```r
quarto::quarto_render(
  input = "LM_project.qmd",
  output_format = "pdf"
)
```

Quarto and a LaTeX installation are required to generate the PDF.

## Limitations

- The oil dataset contains only 24 observations, and the dates are not equally spaced.
- Population density is based on data from 1975.
- Several states share identical price changes, which may reflect rounding or limited measurement precision.
- Percentage increases depend directly on pre-war prices.
- Neighboring states may share similar economic conditions, so observations may not be completely independent.
- The data are observational, so the results show associations rather than causation.

## Project Status

- [x] Data cleaning and validation
- [x] Exploratory data analysis
- [x] Simple linear regression
- [x] Multiple linear regression
- [x] ANOVA and regional comparisons
- [x] Model selection
- [x] Cross-validation
- [x] Regression diagnostics
- [x] Robust inference
- [x] Sensitivity analysis
- [x] Group 1 and Group 2 report
- [ ] Group 3 final integration
- [ ] Final combined project review

## References

- [Iran War Oil Shock 2026: Brent and Gas Tracker — Kaggle](https://www.kaggle.com/datasets/alitaqishah/iran-war-oil-shock-2026-brent-and-gas-tracker)
- [AAA Gas Prices](https://gasprices.aaa.com/)
- [R Documentation — U.S. State Facts and Figures](https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/state.html)
