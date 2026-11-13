# oil_gas_price_analysis_2026

**Group:** Adam Mills, Israel Kakumba Mumay, Ryan Norton Stein

## What this project is about

Statistical analysis of the relationship between crude oil prices and U.S. gasoline prices in 2026 using R, linear regression, ANOVA, and time-series forecasting.

Gas prices went up in every state after the 2026 Iran war began, but not by the same amount. We want to know why.

**Research questions**

1. Why did gas prices rise more in some states than others after the war began?
2. Which state characteristics explain the size of the increase?

**What we expect:** states that were already expensive, and states in the West, rose the most.

## Files

| File | What it is |
|---|---|
| `LM_project2.qmd` | The analysis: data checks, data preparation, plots, and write-up |
| `iran_war_gas_prices_by_state.csv` | The data, one row per state |
| `README.md` | This file |

## The data

The data is the state file from a Kaggle dataset on the 2026 Iran war. It lists AAA state average gas prices in dollars per gallon on three dates: Jan. 8, Feb. 27 (the last pre-war date), and Mar. 19, 2026. There are 50 rows, one per state.

| Variable | What it is |
|---|---|
| `increase` | Mar. 19 price minus Feb. 27 price. This is the response. |
| `pct` | The increase as a percent of the pre-war price |
| `prewar` | Price on Feb. 27 |
| `pre_trend` | Price change from Jan. 8 to Feb. 27 |
| `region` | Midwest, Northeast, South, or West |
| `coastal` | Yes if the state touches an ocean or the Gulf |
| `pop_density` | People per square mile, from R's built-in state data |
| `log_density` | Natural log of `pop_density` |

`pre_trend`, `coastal`, `pop_density`, and `log_density` are created inside the `.qmd`. They are not in the CSV.

**Data checks:** no missing values, no state listed twice, and the increase columns match the prices they are calculated from.

## What we have found so far

- **Every state rose.** The increase runs from $0.68 to $1.05 per gallon, with an average of $0.81 (about 27%). 36 of the 50 states are between $0.76 and $0.84.
- **Pre-war price is the strongest predictor.** States that were more expensive before the war had larger dollar increases (r = 0.89). In percent terms it flips: California had the smallest percent increase (22.3%).
- **The West stands apart.** The five largest increases are all Western states (Hawaii, California, Washington, Nevada, Oregon). They were also the five most expensive states before the war.
- **Density and coastal access do not stand out on their own.** Log density has almost no relationship with the increase (r = 0.09). Coastal and non-coastal states have nearly the same typical increase ($0.80 vs. $0.78).
- **Prices were already rising before the war.** The pre-war trend is related to the increase (r = 0.59), but it overlaps heavily with pre-war price (r = 0.82).
- **Two states break the pattern.** Hawaii had the smallest rise before the war but the largest after. Kentucky was a cheap state ($2.84) but rose $0.88.

## Limits

- Only six states were above $3.50 before the war, and they have a large effect on the trend line.
- Many states share the exact same values (20 have a pre-war trend of $0.30, 11 have an increase of $0.76), so the prices may be rounded or estimated.
- Population density is from 1975.
- There are 50 states and one snapshot in time, so the results show what goes along with bigger increases, not what caused them.

## What is next

1. Multiple linear regression of the increase on pre-war price, region, coastal, and density.
2. Test whether region still matters after pre-war price is in the model.
3. Check predictor overlap with VIF and choose the final model.
4. Check the model assumptions and the influential states.

## How to run it

1. Install R, RStudio, and Quarto.
2. In R, run `install.packages('tidyverse')`.
3. Keep the `.qmd` and the CSV in the same folder.
4. Open `LM_project2.qmd` in RStudio and click Render.

## Timeline

- Proposal presentation: 10 to 15 minutes, about 10 slides
- Final report due: December 10, 2026
