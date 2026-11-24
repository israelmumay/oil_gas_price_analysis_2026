#------------------------ MODELING: LINEAR REGRESSION ------------------------#
# libraries
library(tidyverse)
library(lmtest)   # dwtest(), bgtest(), bptest(), coeftest()
library(sandwich) # NeweyWest() robust standard errors for time series
library(car)      # vif() multicollinearity check

source('preprocessing.R') # gives us clean, daily_prices, events_timeline

set.seed(0) # makes the random split reproducible, same split every run

#--------- WHERE THE PLOTS GO ---------#
# writing to a png fixes 'figure margins too large': the RStudio pane can be smaller
# than the margins a plot asks for, a png device is always the size we set here
out_dir = 'LR_results'
dir.create(out_dir, showWarnings = FALSE) # no warning if it already exists

# save_plot('name', { plot code }) writes LR_results/name.png
# on.exit closes the file even if the plot code fails, so the device never gets stuck
# (a stuck device is why the margin error keeps repeating until you run dev.off())
save_plot = function(name, code, width = 1000, height = 700) {
  path = file.path(out_dir, paste0(name, '.png'))
  png(path, width = width, height = height)
  on.exit(dev.off(), add = TRUE)
  code
  invisible(path)
}

#--------- HELPER FUNCTIONS ---------#
# error metrics, both in the units of the response (dollars) so they are easy to explain
# RMSE punishes big misses more, MAE is the typical miss
rmse = function(actual, pred) sqrt(mean((actual - pred)^2, na.rm = TRUE))
mae = function(actual, pred) mean(abs(actual - pred), na.rm = TRUE)

# NORMALIZING (z-score): (x - mean) / sd, so every feature has mean 0 and sd 1
# the mean and sd are learned from the TRAINING rows only, then applied to the test rows
# if we used all rows, information from the test set would leak into training
# note: lm() does not NEED this. R squared, p-values and predictions are identical
# with or without it. We do it so coefficients are comparable ("dollars per 1 sd")
make_scaler = function(train, cols) {
  list(center = sapply(train[, cols, drop = FALSE], mean, na.rm = TRUE),
       spread = sapply(train[, cols, drop = FALSE], sd, na.rm = TRUE))
}
apply_scaler = function(data, scaler) {
  for (v in names(scaler$center)) {
    data[[v]] = (data[[v]] - scaler$center[[v]]) / scaler$spread[[v]]
  }
  data
}

# turns c('prewar', 'region') into the formula  increase ~ prewar + region
make_formula = function(response, features) {
  if (length(features) == 0) return(as.formula(paste(response, '~ 1'))) # mean only model
  as.formula(paste(response, '~', paste(features, collapse = ' + ')))
}

# SCORE ONE MODEL: the single function that everything else is built on
# R squared ALWAYS goes up when you add a feature, even a useless one, so on its own
# it can never tell us to drop anything. That is why we also track:
#   adj_r2  = R squared with a penalty for each extra feature
#   aic     = fit with a penalty for complexity (lower is better)
#   cv_rmse = error on rows the model did NOT see (lower is better)
# cv = 'kfold'   rows are independent (states), so shuffle them into k folds
# cv = 'rolling' rows are in time order, so NEVER shuffle: train on the first i rows,
#                predict row i + 1, then move forward one row (expanding window)
# the seed is reset every call so every candidate model gets the SAME folds, which
# makes the comparison fair
score_model = function(features, response, data, cv = 'kfold', k = 5, reps = 10,
                       roll_start = 12, seed = 1) {
  f = make_formula(response, features)
  fit = lm(f, data = data)
  n = nrow(data)
  set.seed(seed)
  if (cv == 'kfold') {
    rep_rmse = numeric(reps) # repeat the k folds a few times to smooth out luck
    for (r in 1:reps) {
      folds = sample(rep(1:k, length.out = n))
      pred = rep(NA, n)
      for (fold in 1:k) {
        fit_fold = lm(f, data = data[folds != fold, ])
        pred[folds == fold] = suppressWarnings(predict(fit_fold, newdata = data[folds == fold, ]))
      }
      rep_rmse[r] = rmse(data[[response]], pred)
    }
    cv_rmse = mean(rep_rmse)
  } else {
    pred = rep(NA, n)
    for (i in roll_start:(n - 1)) {
      fit_i = lm(f, data = data[1:i, ])
      pred[i + 1] = suppressWarnings(predict(fit_i, newdata = data[i + 1, ]))
    }
    cv_rmse = rmse(data[[response]], pred)
  }
  data.frame(n_features = length(features),
             r2 = summary(fit)$r.squared,
             adj_r2 = summary(fit)$adj.r.squared,
             aic = AIC(fit),
             cv_rmse = cv_rmse)
}

# DROP ONE AT A TIME: refit the model once without each feature in turn
# the feature whose removal hurts the least (highest adj R squared) is on top
# a factor like region is dropped as a whole, not one dummy column at a time
drop_one_table = function(features, response, data, ...) {
  out = data.frame()
  for (v in features) {
    s = score_model(setdiff(features, v), response, data, ...)
    out = rbind(out, data.frame(dropped = v, s))
  }
  out[order(-out$adj_r2), ]
}

# BACKWARD PRUNING: start with everything, drop the least useful feature, repeat
# until one feature is left. We go all the way down so the whole path is visible
# and YOU choose where to stop (a greedy stop rule can hide a good model)
# returns the history table and the feature set at every step
backward_prune = function(all_features, response, data, ...) {
  current = all_features
  history = data.frame(step = 0, dropped = 'none',
                       score_model(current, response, data, ...),
                       remaining = paste(current, collapse = ', '))
  sets = list(current)
  while (length(current) > 1) {
    candidates = drop_one_table(current, response, data, ...)
    best = candidates[1, ]
    current = setdiff(current, best$dropped)
    history = rbind(history, data.frame(step = nrow(history), best,
                                        remaining = paste(current, collapse = ', ')))
    sets[[length(sets) + 1]] = current
  }
  list(history = history, sets = sets)
}

# PLOT THE PRUNING PATH: pruning runs left to right (many features -> few)
# left plot: R squared (always falls as we prune) vs adjusted R squared (can rise)
# right plot: cross validated error (the lower the better)
plot_prune = function(history, title) {
  par(mfrow = c(1, 2))
  xr = rev(range(history$n_features))
  plot(history$n_features, history$r2, type = 'b', pch = 16, xlim = xr,
       ylim = range(c(history$r2, history$adj_r2)),
       main = paste(title, ': R squared'), xlab = 'number of features', ylab = 'R squared')
  lines(history$n_features, history$adj_r2, type = 'b', pch = 17, col = 'red', lty = 2)
  legend('bottomleft', legend = c('R squared', 'adjusted R squared'),
         col = c('black', 'red'), pch = c(16, 17), lty = c(1, 2), cex = 0.8)
  plot(history$n_features, history$cv_rmse, type = 'b', pch = 16, xlim = xr,
       main = paste(title, ': CV error'), xlab = 'number of features', ylab = 'CV RMSE')
  par(mfrow = c(1, 1))
}

# HOLDOUT SCORE: fit on train, score on rows the model never saw
# r2 here compares against just guessing the training mean (it can be negative)
holdout_scores = function(features, response, train, test) {
  fit = lm(make_formula(response, features), data = train)
  pred = suppressWarnings(predict(fit, newdata = test))
  actual = test[[response]]
  data.frame(rmse = rmse(actual, pred), mae = mae(actual, pred),
             r2 = 1 - sum((actual - pred)^2) / sum((actual - mean(train[[response]]))^2))
}

# DIAGNOSTICS for a fitted model, the standard checks
diagnostics = function(fit) {
  par(mfrow = c(2, 2))
  plot(fit) # residuals vs fitted, QQ, scale-location, leverage
  par(mfrow = c(1, 1))
  print(shapiro.test(residuals(fit))) # small p = residuals not normal
  print(bptest(fit))                  # small p = unequal variance
  # vif above 5 or 10 = a feature is mostly explained by the others
  tryCatch(print(vif(fit)), error = function(e) cat('vif not available:', conditionMessage(e), '\n'))
}

# COOK'S DISTANCE: how much the fit changes if one row is removed
# rows above 4 / n (red line) deserve a look. Not an automatic reason to delete them
cooks_plot = function(fit, labels) {
  cd = cooks.distance(fit)
  thr = 4 / length(cd)
  plot(cd, type = 'h', main = "Cook's Distance", xlab = 'row', ylab = "Cook's distance")
  abline(h = thr, col = 'red', lty = 2)
  big = which(cd > thr)
  if (length(big) > 0) text(big, cd[big], labels = labels[big], pos = 3, cex = 0.7)
}

# COEFFICIENT PLOT: estimate and 95% confidence interval for each term
# if the interval crosses the red line at 0, we cannot rule out "no effect"
# on z-scored data the numeric terms are comparable. Dummy terms (region, war)
# are in dollars for "being in that group", so compare those among themselves
coef_plot = function(fit, title) {
  est = coef(fit)[-1]
  ci = confint(fit)[-1, , drop = FALSE]
  ypos = seq_along(est)
  # left margin scaled to the longest feature name so the labels always fit
  # capped at 20 lines so the margin can never be wider than the device
  left = max(4, min(20, 0.55 * max(nchar(names(est))) + 2))
  op = par(mar = c(5, left, 4, 2))
  plot(est, ypos, pch = 16, yaxt = 'n', xlim = range(ci), ylab = '',
       xlab = 'coefficient (dollars per gallon)', main = title)
  segments(ci[, 1], ypos, ci[, 2], ypos)
  axis(2, at = ypos, labels = names(est), las = 1, cex.axis = 0.8)
  abline(v = 0, col = 'red', lty = 2)
  par(op)
}


#=========================================================================#
#------------------ PART 1: STATE LEVEL MODEL (50 STATES) ----------------#
#=========================================================================#

#--------- SETUP ---------#
# the states are not ordered in time (one snapshot), so a random split is fine here
states = clean
states$log_density = log(states$pop_density) # same transform as eda.R

response = 'increase' # dollars per gallon, Mar 19 minus Feb 27

# EVERY candidate feature goes in this list. Add or remove names to experiment
# left out on purpose:
#   mar19 and pct  -> the response is computed from them (they would "predict" it perfectly)
#   jan08          -> pre_trend = prewar - jan08, so adding it is exact duplication
# pop_density and log_density are the same information twice. Both go in so the
# pruning has to pick
all_features = c('prewar', 'pre_trend', 'region', 'coastal', 'log_density', 'pop_density')
num_features = c('prewar', 'pre_trend', 'log_density', 'pop_density') # the ones we z-score

#--------- TRAIN TEST SPLIT ---------#
# 80 / 20, done inside each region (stratified)
# why: if the test set held ALL of a small region, the training data would never see
# that region and predict() would fail. This also keeps the regions balanced
# 50 rows is small, so the 10 test states are a noisy judge. That is why the
# pruning below uses cross validation on the training rows and the test set is
# looked at ONCE at the end
test_idx = c()
for (r in levels(states$region)) {
  rows = which(states$region == r)
  test_idx = c(test_idx, sample(rows, size = round(0.2 * length(rows))))
}
train = states[-test_idx, ]
test = states[test_idx, ]
dim(train); dim(test) # 40 and 10 rows
table(train$region); table(test$region)

#--------- NORMALIZE (TRAIN ONLY) ---------#
scaler = make_scaler(train, num_features)
train_z = apply_scaler(train, scaler)
test_z = apply_scaler(test, scaler) # the test set uses the TRAIN mean and sd
round(sapply(train_z[, num_features], mean), 3) # 0 by construction
round(sapply(train_z[, num_features], sd), 3)   # 1 by construction

# proof that scaling does not change the model: same predictions raw vs scaled
f_check = make_formula(response, c('prewar', 'region'))
all.equal(unname(predict(lm(f_check, data = train), test)),
          unname(predict(lm(f_check, data = train_z), test_z))) # TRUE

#--------- STEP 1: THE FULL MODEL (ALL FEATURES) ---------#
full_fit = lm(make_formula(response, all_features), data = train_z)
summary(full_fit)
vif(full_fit) # expect big values for log_density and pop_density (same information)

#--------- STEP 2: PRUNE ONE FEATURE AT A TIME ---------#
# pruning is done on the TRAINING rows only. If we pruned using the test rows,
# the test score would no longer be an honest test
prune = backward_prune(all_features, response, train_z, cv = 'kfold')
print(prune$history, digits = 3)
# read it top to bottom: "dropped" is the feature removed at that step, and the
# other columns describe the model AFTER removing it
# what to look for:
#   - r2 only goes down, that is expected and is NOT a reason to keep a feature
#   - adj_r2 and cv_rmse: while they stay flat or improve, the dropped features
#     were not adding anything. When they get clearly worse, we pruned too far

save_plot('state_01_pruning_path', plot_prune(prune$history, 'State model'))

# pick the step with the best adjusted R squared (cv_rmse is the tie breaker to check)
best_step = which.max(prune$history$adj_r2)
chosen_features = prune$sets[[best_step]]
# to choose by hand instead, uncomment and edit:
# chosen_features = c('prewar', 'pre_trend')
chosen_features
which.min(prune$history$cv_rmse) == best_step # TRUE means both criteria agree
# when two models are close, prefer the one with fewer features, it is easier to explain

#--------- STEP 3: DID PRUNING HELP ON UNSEEN STATES? ---------#
# the first and only look at the test set. Three models, same 10 test states:
# mean only = the dumbest guess, every state gets the average increase
# a useful model has to beat that
test_table = rbind(
  data.frame(model = 'mean only', holdout_scores(character(0), response, train_z, test_z)),
  data.frame(model = 'all features', holdout_scores(all_features, response, train_z, test_z)),
  data.frame(model = 'pruned', holdout_scores(chosen_features, response, train_z, test_z)))
print(test_table, digits = 3)

#--------- STEP 4: FINAL MODEL ON ALL 50 STATES ---------#
# the choice of features is made, so now we use all the data to estimate coefficients
states_z = apply_scaler(states, make_scaler(states, num_features))
final = lm(make_formula(response, chosen_features), data = states_z)
summary(final)
confint(final)

# which terms are worth keeping? F test for removing each whole term (region counts as one)
drop1(final, test = 'F')

save_plot('state_02_coefficients',
          coef_plot(final, 'State model: coefficients (numeric features in sd units)'))

#--------- STEP 5: INSPECT THE MODEL OUTPUT ---------#
# 5a. standard diagnostics
save_plot('state_03_diagnostics', diagnostics(final), height = 900) # test output prints below

# 5b. influential states
save_plot('state_04_cooks_distance', cooks_plot(final, states$state))
influence_table = data.frame(state = states$state, region = states$region,
                             actual = states$increase, fitted = fitted(final),
                             residual = residuals(final), leverage = hatvalues(final),
                             cooks = cooks.distance(final))
head(influence_table[order(-influence_table$cooks), ], 6)

# 5c. does the answer survive without the 3 most influential states?
drop_states = influence_table$state[order(-influence_table$cooks)][1:3]
final_drop = lm(make_formula(response, chosen_features),
                data = states_z[!(states_z$state %in% drop_states), ])
round(cbind(all_states = coef(final), without_top3 = coef(final_drop)), 4)

# 5d. actual vs fitted, points on the dashed line are perfect predictions
save_plot('state_05_actual_vs_fitted', {
  plot(fitted(final), states$increase, pch = 16, col = as.integer(states$region),
       main = 'State model: Actual vs Fitted', xlab = 'fitted increase', ylab = 'actual increase')
  abline(0, 1, lty = 2)
  legend('topleft', legend = levels(states$region), col = 1:4, pch = 16, cex = 0.8)
  worst = order(-abs(residuals(final)))[1:4] # the states the model misses the most
  text(fitted(final)[worst], states$increase[worst], states$state[worst], pos = 3, cex = 0.7)
})

# 5e. out of fold predictions: every state predicted by a model that never saw it
# this is the honest version of the plot above
set.seed(1)
folds = sample(rep(1:5, length.out = nrow(states)))
oof = rep(NA, nrow(states))
for (fold in 1:5) {
  fit_fold = lm(make_formula(response, chosen_features), data = states_z[folds != fold, ])
  oof[folds == fold] = predict(fit_fold, newdata = states_z[folds == fold, ])
}
c(rmse = rmse(states$increase, oof), mae = mae(states$increase, oof),
  r2 = 1 - sum((states$increase - oof)^2) / sum((states$increase - mean(states$increase))^2))
save_plot('state_06_actual_vs_out_of_fold', {
  plot(oof, states$increase, pch = 16, col = as.integer(states$region),
       main = 'State model: Actual vs Out of Fold Prediction',
       xlab = 'predicted increase', ylab = 'actual increase')
  abline(0, 1, lty = 2)
  legend('topleft', legend = levels(states$region), col = 1:4, pch = 16, cex = 0.8)
})

# 5f. residuals by region, is the model wrong in a pattern? (links to the ANOVA part)
save_plot('state_07_residuals_by_region', {
  boxplot(residuals(final) ~ states$region, main = 'State model: Residuals by Region',
          xlab = 'region', ylab = 'residual (dollars per gallon)')
  abline(h = 0, col = 'red', lty = 2)
})

#--------- SAVE RESULTS ---------#
write.csv(prune$history, file.path(out_dir, 'state_pruning_history.csv'), row.names = FALSE)
write.csv(test_table, file.path(out_dir, 'state_holdout_scores.csv'), row.names = FALSE)
write.csv(influence_table, file.path(out_dir, 'state_influence_table.csv'), row.names = FALSE)


#=========================================================================#
#-------------- PART 2: DAILY TIME SERIES (THE OIL SPIKE) ----------------#
#=========================================================================#
# DIFFERENT RULES for time series:
#   1. rows are in time order, so NO random shuffling. Test = the LAST rows
#   2. cross validation is "rolling": train on the past, predict the next row
#   3. neighbouring rows are correlated, so p-values from plain lm() are too optimistic
#      (we check this with Durbin Watson and use Newey West standard errors)
#   4. only 24 rows, and the gaps between them are uneven (17 are one day, the
#      rest 2 to 6 days). "previous row" is NOT always "yesterday". Treat results as exploratory

#--------- SETUP ---------#
daily = daily_prices
daily$date = as.Date(daily$date)
daily = daily[order(daily$date), ]
daily$war = ifelse(daily$war_day > 0, 1, 0) # 0 before the first strike, 1 after
events = events_timeline
events$date = as.Date(events$date)

n_all = nrow(daily)
daily$lag_brent = c(NA, daily$brent_usd_barrel[-n_all]) # brent from the previous row
daily$lag_gas = c(NA, daily$us_gas_avg_gallon[-n_all])  # gas from the previous row

# row to row changes, because trending LEVELS make unrelated series look related
# (eda.R: levels correlate at 0.98, changes only at 0.56)
daily$gas_chg = c(NA, diff(daily$us_gas_avg_gallon))
base_feats = c('brent_usd_barrel', 'wti_usd_barrel', 'dubai_usd_barrel',
               'us_diesel_avg_gallon', 'strait_hormuz_daily_ships', 'iran_production_mbpd')
for (v in base_feats) {
  daily[[paste0(v, '_chg')]] = c(NA, diff(daily[[v]]))
}

#--------- INSPECT THE SPIKE ---------#
# how unusual is each day's Brent move compared with the calm weeks before the war?
# brent_pct = % change from the previous row. spike_z = how many "calm sds" away it is
# caution: the calm baseline is only 5 changes, so this is a rough flag and not a test
daily$brent_pct = 100 * daily$brent_usd_barrel_chg / daily$lag_brent
calm = daily$war == 0 & !is.na(daily$brent_pct)
calm_mean = mean(daily$brent_pct[calm])
calm_sd = sd(daily$brent_pct[calm])
daily$spike_z = (daily$brent_pct - calm_mean) / calm_sd
daily[which(abs(daily$spike_z) > 2), c('date', 'brent_usd_barrel', 'brent_pct', 'spike_z')]

# did the dated events line up with the big moves? (alignment, NOT proof of cause)
merge(events[, c('date', 'event_title', 'category')],
      daily[, c('date', 'brent_pct', 'spike_z')], by = 'date')

# prices over time, grey dotted lines = key events, red dashed line = first strike
save_plot('ts_01_prices_over_time', {
  par(mfrow = c(2, 1))
  plot(daily$date, daily$brent_usd_barrel, type = 'b', pch = 16,
       main = 'Brent Crude Over Time', xlab = '', ylab = 'brent (dollars per barrel)')
  abline(v = events$date, col = 'grey70', lty = 3)
  abline(v = as.Date('2026-02-28'), col = 'red', lty = 2)
  plot(daily$date, daily$us_gas_avg_gallon, type = 'b', pch = 16,
       main = 'US Average Gas Price Over Time', xlab = 'date', ylab = 'gas (dollars per gallon)')
  abline(v = events$date, col = 'grey70', lty = 3)
  abline(v = as.Date('2026-02-28'), col = 'red', lty = 2)
  par(mfrow = c(1, 1))
}, height = 850)

# daily % change in brent, dashed lines = calm mean +/- 2 sd, red bars = after the first strike
save_plot('ts_02_brent_percent_change', {
  barplot(daily$brent_pct[-1], names.arg = format(daily$date[-1], '%m-%d'), las = 2, cex.names = 0.7,
          col = ifelse(daily$war[-1] == 1, 'tomato', 'grey60'),
          main = 'Brent Percent Change per Row', ylab = 'percent change')
  abline(h = calm_mean + c(-2, 2) * calm_sd, lty = 2)
})

# brent vs gas, did the relationship change after the war started? (only 6 pre-war points)
save_plot('ts_03_gas_vs_brent', {
  plot(daily$brent_usd_barrel, daily$us_gas_avg_gallon, pch = 16, col = ifelse(daily$war == 1, 'red', 'blue'),
       main = 'Gas vs Brent: Before (blue) and After (red) the War Started',
       xlab = 'brent (dollars per barrel)', ylab = 'gas (dollars per gallon)')
  abline(lm(us_gas_avg_gallon ~ brent_usd_barrel, data = daily[daily$war == 0, ]), col = 'blue', lty = 2)
  abline(lm(us_gas_avg_gallon ~ brent_usd_barrel, data = daily[daily$war == 1, ]), col = 'red', lty = 2)
})

#--------- CHOOSE WHAT TO MODEL ---------#
# 'levels'  = predict the gas PRICE from the prices (easy to read, but strongly autocorrelated)
# 'changes' = predict the row to row CHANGE in gas from changes in the features
# run the whole part once for each mode and compare. Metrics from the two modes are in
# different units (dollars vs change in dollars), so do not compare them directly
ts_mode = 'levels'

# EVERY candidate feature goes in these lists, same idea as Part 1
# note: diesel is not a cause of gas, it is another pump price that moves with it, so
# expect it to look great. Be careful claiming it "explains" gas
# note: war_day is left out on purpose, it is just a clock (a straight line in time)
if (ts_mode == 'levels') {
  ts_response = 'us_gas_avg_gallon'
  ts_features = c(base_feats, 'lag_brent', 'war')
} else {
  ts_response = 'gas_chg'
  ts_features = c(paste0(base_feats, '_chg'), 'war')
}
ts_num = setdiff(ts_features, 'war') # z-score everything except the 0/1 war flag

# every model must use the SAME rows or the R squared values are not comparable
# lag and change columns are NA in the first row, so we drop it for all models
tsd = daily[complete.cases(daily[, c(ts_response, ts_features)]), ]
nrow(tsd)

#--------- CHRONOLOGICAL TRAIN TEST SPLIT ---------#
# the test set is the LAST 6 rows (Mar 15 to Mar 20), the infrastructure strikes week
# so the model is tested on the sharpest part of the spike, a stress test
n_test = 6
n_tsd = nrow(tsd)
ts_train = tsd[1:(n_tsd - n_test), ]
ts_test = tsd[(n_tsd - n_test + 1):n_tsd, ]
range(ts_train$date); range(ts_test$date)

# the test brent prices go ABOVE anything in training. A linear model has never seen
# prices that high, so it has to extrapolate. This is the structural break problem
rbind(train = range(ts_train$brent_usd_barrel), test = range(ts_test$brent_usd_barrel))

#--------- NORMALIZE (TRAIN ONLY) ---------#
ts_scaler = make_scaler(ts_train, ts_num)
ts_train_z = apply_scaler(ts_train, ts_scaler)
ts_test_z = apply_scaler(ts_test, ts_scaler)
tsd_z_train = apply_scaler(tsd, ts_scaler) # whole series on the train scale, for plots

#--------- STEP 1: THE FULL MODEL (ALL FEATURES) ---------#
ts_full = lm(make_formula(ts_response, ts_features), data = ts_train_z)
summary(ts_full)
# with 17 training rows and about 8 features this will fit very well and mean very little
# the three crude benchmarks move together (correlation about 0.99), so expect huge vif
# or aliased (NA) coefficients
tryCatch(print(vif(ts_full)), error = function(e) cat('vif not available:', conditionMessage(e), '\n'))

#--------- STEP 2: PRUNE ONE FEATURE AT A TIME ---------#
# time series version: rolling origin cross validation, starting from 10 training rows
ts_prune = backward_prune(ts_features, ts_response, ts_train_z, cv = 'rolling', roll_start = 10)
print(ts_prune$history, digits = 3)
save_plot(paste0('ts_04_pruning_path_', ts_mode),
          plot_prune(ts_prune$history, 'Time series model'))

ts_best_step = which.max(ts_prune$history$adj_r2)
ts_chosen = ts_prune$sets[[ts_best_step]]
# to choose by hand instead, uncomment and edit:
# ts_chosen = c('brent_usd_barrel', 'war')
ts_chosen
which.min(ts_prune$history$cv_rmse) == ts_best_step # do both criteria agree?

#--------- STEP 3: HOLDOUT TEST ON THE SPIKE WEEK ---------#
# naive baseline: "tomorrow looks like today"
# levels  -> predict the previous row's gas price
# changes -> predict zero change
# a model that cannot beat this is not adding value
if (ts_mode == 'levels') naive_pred = ts_test$lag_gas else naive_pred = rep(0, nrow(ts_test))
naive_rmse = rmse(ts_test[[ts_response]], naive_pred)

ts_test_table = rbind(
  data.frame(model = 'all features', holdout_scores(ts_features, ts_response, ts_train_z, ts_test_z)),
  data.frame(model = 'pruned', holdout_scores(ts_chosen, ts_response, ts_train_z, ts_test_z)))
print(ts_test_table, digits = 3)
naive_rmse # the number to beat (note: levels models also see the same day's brent, the naive one does not)

#--------- STEP 4: FINAL MODEL ON ALL ROWS ---------#
tsd_z_all = apply_scaler(tsd, make_scaler(tsd, ts_num))
ts_final = lm(make_formula(ts_response, ts_chosen), data = tsd_z_all)
summary(ts_final)
confint(ts_final)
save_plot(paste0('ts_05_coefficients_', ts_mode),
          coef_plot(ts_final, 'Time series model: coefficients (numeric features in sd units)'))

#--------- STEP 5: INSPECT THE MODEL OUTPUT ---------#
# 5a. standard diagnostics
save_plot(paste0('ts_06_diagnostics_', ts_mode), diagnostics(ts_final), height = 900)

# 5b. serial correlation, the key time series check
# Durbin Watson near 2 = fine, near 0 = residuals are strongly positively correlated
# small p in either test = the plain lm() p-values and intervals are too optimistic
dwtest(ts_final)
bgtest(ts_final, order = 1)
# bars outside the blue lines = leftover pattern
save_plot(paste0('ts_07_acf_residuals_', ts_mode), acf(residuals(ts_final), main = 'ACF of Residuals'))

# 5c. Newey West standard errors: corrected for correlated residuals. Compare with summary()
# a term that is significant in summary() but not here was getting credit from autocorrelation
coeftest(ts_final, vcov = NeweyWest(ts_final, lag = 2, prewhite = FALSE))

# 5d. residuals over time, red = after the war started. A pattern here = something missing
save_plot(paste0('ts_08_residuals_over_time_', ts_mode), {
  plot(tsd$date, residuals(ts_final), type = 'b', pch = 16, col = ifelse(tsd$war == 1, 'red', 'blue'),
       main = 'Residuals Over Time', xlab = 'date', ylab = 'residual')
  abline(h = 0, lty = 2)
  abline(v = as.Date('2026-02-28'), col = 'red', lty = 2)
})

# 5e. which days pull the model the most? (expect the spike days)
save_plot(paste0('ts_09_cooks_distance_', ts_mode), cooks_plot(ts_final, format(tsd$date, '%m-%d')))

# 5f. actual vs predicted over time. The model is fit on the TRAIN rows only, so the
# shaded part is a real out of sample prediction of the spike week
ts_train_fit = lm(make_formula(ts_response, ts_chosen), data = ts_train_z)
ts_pred = suppressWarnings(predict(ts_train_fit, newdata = tsd_z_train))
save_plot(paste0('ts_10_actual_vs_predicted_', ts_mode), {
  plot(tsd$date, tsd[[ts_response]], type = 'b', pch = 16,
       ylim = range(c(tsd[[ts_response]], ts_pred), na.rm = TRUE),
       main = 'Actual vs Predicted (fit on unshaded rows only)', xlab = 'date', ylab = ts_response)
  rect(min(ts_test$date) - 0.5, par('usr')[3], max(ts_test$date) + 0.5, par('usr')[4],
       col = rgb(0, 0, 0, 0.08), border = NA)
  lines(tsd$date, ts_pred, type = 'b', pch = 17, col = 'red', lty = 2)
  legend('topleft', legend = c('actual', 'predicted'), col = c('black', 'red'),
         pch = c(16, 17), lty = c(1, 2), cex = 0.8)
})

# 5g. predicted vs actual on the test rows only
ts_test_pred = predict(ts_train_fit, newdata = ts_test_z)
data.frame(date = ts_test$date, actual = ts_test[[ts_response]], predicted = ts_test_pred,
           error = ts_test[[ts_response]] - ts_test_pred, naive = naive_pred)

#--------- SAVE RESULTS ---------#
write.csv(ts_prune$history, file.path(out_dir, paste0('ts_pruning_history_', ts_mode, '.csv')), row.names = FALSE)
write.csv(ts_test_table, file.path(out_dir, paste0('ts_holdout_scores_', ts_mode, '.csv')), row.names = FALSE)


#=========================================================================#
#------------- PART 3: PICK THE BEST MODEL AND TEST IT --------------------#
#=========================================================================#
# PASTE THIS AT THE VERY BOTTOM OF modeling.R. Nothing above needs to change.
# It reuses the pruning results from Part 1 and Part 2 (prune and ts_prune), so run
# those parts first. The plots made earlier were for the default pick (best adjusted R
# squared). The plots here are remade for the FINAL model we decide to report
#
# WHAT IT DOES
#   1. a decision rule that picks ONE model out of the pruning path
#   2. a table comparing every model on the path, with the winner marked
#   3. formal tests on the winner: formula, hypotheses, test statistic, decision
#   4. all the diagnostic plots again, labelled FINAL MODEL

#--------- DECISION RULE: HOW WE PICK THE WINNER ---------#
# the model with the lowest CV error is the most accurate on unseen rows, but a model with
# one more feature that is only a tiny bit worse is NOT really worse. With so few rows the
# difference is noise, so we prefer the simplest model that is close to the best
# rule: among all models whose CV error is within tol of the lowest, take the one with the
# FEWEST features. tol = 0.03 means "within 3 percent". Set tol = 0 for the strict best
pick_best = function(history, tol = 0.03) {
  ok = which(history$cv_rmse <= min(history$cv_rmse, na.rm = TRUE) * (1 + tol))
  history$step[ok][which.min(history$n_features[ok])]
}

#--------- THE COMPARISON TABLE ---------#
# one row per model on the pruning path (6 features, 5, 4, ... 1)
# r2, adj_r2, aic, bic, cv_rmse = how well the model fits and predicts, computed on training rows
# f_pvalue = overall F test p value for that model (is the model better than just the mean?)
# test_rmse and test_r2 = score on the held out rows. REPORTING ONLY. We never choose using
# the test rows, because then they stop being a fair test
model_table = function(prune, response, train, test, best_step) {
  rows = data.frame()
  for (i in seq_along(prune$sets)) {
    feats = prune$sets[[i]]
    fit = lm(make_formula(response, feats), data = train)
    fs = summary(fit)$fstatistic
    hold = holdout_scores(feats, response, train, test)
    rows = rbind(rows, data.frame(
      step = i - 1,
      n_features = length(feats),
      features = paste(feats, collapse = ' + '),
      r2 = summary(fit)$r.squared,
      adj_r2 = summary(fit)$adj.r.squared,
      aic = AIC(fit),
      bic = BIC(fit),
      cv_rmse = prune$history$cv_rmse[i],
      f_pvalue = pf(fs[1], fs[2], fs[3], lower.tail = FALSE),
      test_rmse = hold$rmse,
      test_r2 = hold$r2))
  }
  rows$selected = ifelse(rows$step == best_step, '<-- BEST', '')
  rows
}

#--------- FORMAL TESTS ON THE WINNER ---------#
# the regression equation in the ORIGINAL units (dollars), the easiest thing to explain
equation = function(fit, response) {
  b = coef(fit)
  pieces = paste0(ifelse(b[-1] >= 0, ' + ', ' - '), round(abs(b[-1]), 4), ' * ', names(b[-1]), collapse = '')
  cat(response, '=', round(b[1], 4), pieces, '\n')
}

# TEST 1, OVERALL F TEST: is the model as a whole any better than just using the average?
# H0: every slope is 0 (the features explain nothing)   Ha: at least one slope is not 0
overall_f_test = function(fit, alpha = 0.05) {
  fs = summary(fit)$fstatistic
  p = pf(fs[1], fs[2], fs[3], lower.tail = FALSE)
  cat('\nOVERALL F TEST\n')
  cat('  H0: all slopes are 0 (the features explain nothing)\n')
  cat('  Ha: at least one slope is not 0\n')
  cat('  F =', round(fs[1], 2), ' df =', fs[2], 'and', fs[3], ' p =', signif(p, 3), '\n')
  cat('  decision at alpha =', alpha, ':', ifelse(p < alpha, 'REJECT H0, the model explains something',
                                                  'FAIL TO REJECT H0, no evidence the model beats the mean'), '\n')
}

# TEST 2, T TEST FOR EACH SLOPE: does this one feature matter, with the others held fixed?
# H0: slope = 0   Ha: slope is not 0. t = estimate / standard error
# also gives a 95 percent confidence interval. If it excludes 0 we reject H0 (same decision)
# vcov_fn is for time series: it swaps in robust (Newey West) standard errors
hypothesis_table = function(fit, vcov_fn = NULL, alpha = 0.05) {
  if (is.null(vcov_fn)) ct = coef(summary(fit)) else ct = coeftest(fit, vcov = vcov_fn(fit))
  ci = confint(fit)
  out = data.frame(term = rownames(ct), estimate = ct[, 1], std_error = ct[, 2],
                   t_value = ct[, 3], p_value = ct[, 4], ci_low = ci[, 1], ci_high = ci[, 2])
  out$decision = ifelse(out$p_value < alpha, 'reject H0: effect is not zero',
                        'fail to reject H0: no evidence of an effect')
  rownames(out) = NULL
  out
}

# TEST 3, NESTED F TEST: did pruning throw away anything that mattered?
# compares the winner (small model) with the full model (all features)
# H0: the dropped features all have slope 0, so the small model is enough
# Ha: at least one dropped feature matters. Large p = pruning was fine
nested_f_test = function(small, big, alpha = 0.05) {
  a = anova(small, big)
  p = a[['Pr(>F)']][2]
  cat('\nNESTED F TEST (small model vs full model)\n')
  cat('  H0: the dropped features all have slope 0, the small model is enough\n')
  cat('  Ha: at least one dropped feature matters\n')
  cat('  F =', round(a[['F']][2], 2), ' p =', signif(p, 3), '\n')
  cat('  decision at alpha =', alpha, ':', ifelse(p < alpha, 'REJECT H0, pruning lost something real',
                                                  'FAIL TO REJECT H0, pruning was fine'), '\n')
}

# plot: how CV error changes as we prune, with the winner circled and the tolerance band
plot_selection = function(history, best_step, tol, title) {
  plot(history$n_features, history$cv_rmse, type = 'b', pch = 16, xlim = rev(range(history$n_features)),
       main = title, xlab = 'number of features', ylab = 'CV RMSE')
  abline(h = min(history$cv_rmse, na.rm = TRUE) * (1 + tol), col = 'grey50', lty = 2) # "close enough" line
  win = history[history$step == best_step, ]
  points(win$n_features, win$cv_rmse, col = 'red', pch = 1, cex = 2.5, lwd = 2)
  legend('topleft', legend = c('CV error', paste0('within ', tol * 100, '% of best'), 'selected model'),
         col = c('black', 'grey50', 'red'), pch = c(16, NA, 1), lty = c(1, 2, NA), cex = 0.8)
}


#---------------------------------------------------------------------------#
#-------------------- PART 3A: BEST STATE MODEL ----------------------------#
#---------------------------------------------------------------------------#
tol = 0.03 # how close to the best CV error counts as "as good". 0 = strict best

#--------- 1. PICK THE WINNER ---------#
pick_step = pick_best(prune$history, tol)
pick_feats = prune$sets[[pick_step + 1]] # sets[[1]] is step 0
# to choose by hand instead, uncomment and edit:
# pick_feats = c('prewar', 'pre_trend')
pick_feats

#--------- 2. THE COMPARISON TABLE ---------#
state_model_table = model_table(prune, response, train_z, test_z, pick_step)
print(state_model_table, digits = 3)
# how to read it: look down cv_rmse for the lowest, then at adj_r2 and the f_pvalue. The test
# columns are a sanity check at the end: a good model should also look good there
save_plot('final_state_01_model_selection',
          plot_selection(prune$history, pick_step, tol, 'State model: choosing the final model'))

#--------- 3. FIT THE WINNER ON ALL 50 STATES ---------#
# raw units (dollars) for the equation and tests, z scored version for the comparable plot
final_formula = make_formula(response, pick_feats)
best_fit = lm(final_formula, data = states)
best_fit_z = lm(final_formula, data = states_z)
full_fit_all = lm(make_formula(response, all_features), data = states)

cat('\nFINAL STATE MODEL\n')
equation(best_fit, response)
summary(best_fit)$r.squared; summary(best_fit)$adj.r.squared

#--------- 4. FORMULA, HYPOTHESES, TEST, DECISION ---------#
overall_f_test(best_fit)
state_tests = hypothesis_table(best_fit)
print(state_tests, digits = 3) # one row per term: H0 is "this slope is 0"
drop1(best_fit, test = 'F') # same idea per whole feature (this is the right test for region)
if (length(pick_feats) < length(all_features)) nested_f_test(best_fit, full_fit_all)
# NOTE: we chose these features using the same states, so the p values are a bit optimistic.
# CV error and the test rows are the honest checks. Say that in the write up

#--------- 5. PLOTS FOR THE FINAL MODEL ---------#
save_plot('final_state_02_coefficients',
          coef_plot(best_fit_z, 'FINAL state model: coefficients (numeric features in sd units)'))
# residual plots, QQ plot, shapiro, bptest, vif
save_plot('final_state_03_diagnostics', diagnostics(best_fit), height = 900)
save_plot('final_state_04_cooks_distance', cooks_plot(best_fit, states$state))

# actual vs fitted for the final model, dashed line = perfect prediction
save_plot('final_state_05_actual_vs_fitted', {
  plot(fitted(best_fit), states$increase, pch = 16, col = as.integer(states$region),
       main = 'FINAL state model: Actual vs Fitted', xlab = 'fitted increase', ylab = 'actual increase')
  abline(0, 1, lty = 2)
  legend('topleft', legend = levels(states$region), col = 1:4, pch = 16, cex = 0.8)
  worst_best = order(-abs(residuals(best_fit)))[1:4]
  text(fitted(best_fit)[worst_best], states$increase[worst_best], states$state[worst_best], pos = 3, cex = 0.7)
})

# out of fold: every state predicted by a model that never saw it
set.seed(1)
folds = sample(rep(1:5, length.out = nrow(states)))
oof_best = rep(NA, nrow(states))
for (fold in 1:5) {
  fit_fold = lm(final_formula, data = states[folds != fold, ])
  oof_best[folds == fold] = predict(fit_fold, newdata = states[folds == fold, ])
}
save_plot('final_state_06_actual_vs_out_of_fold', {
  plot(oof_best, states$increase, pch = 16, col = as.integer(states$region),
       main = 'FINAL state model: Actual vs Out of Fold Prediction',
       xlab = 'predicted increase', ylab = 'actual increase')
  abline(0, 1, lty = 2)
  legend('topleft', legend = levels(states$region), col = 1:4, pch = 16, cex = 0.8)
})
c(rmse = rmse(states$increase, oof_best), mae = mae(states$increase, oof_best))

# residuals by region: is the final model wrong in a regional pattern? (links to the ANOVA)
save_plot('final_state_07_residuals_by_region', {
  boxplot(residuals(best_fit) ~ states$region, main = 'FINAL state model: Residuals by Region',
          xlab = 'region', ylab = 'residual (dollars per gallon)')
  abline(h = 0, col = 'red', lty = 2)
})

write.csv(state_model_table, file.path(out_dir, 'state_model_selection_table.csv'), row.names = FALSE)
write.csv(state_tests, file.path(out_dir, 'state_final_model_tests.csv'), row.names = FALSE)


#---------------------------------------------------------------------------#
#------------------ PART 3B: BEST DAILY TIME SERIES MODEL ------------------#
#---------------------------------------------------------------------------#
# same steps, time series version. Uses the ts_mode you set in Part 2 (levels or changes)
# run the whole file once per mode and compare

#--------- 1. PICK THE WINNER ---------#
ts_pick_step = pick_best(ts_prune$history, tol)
ts_pick_feats = ts_prune$sets[[ts_pick_step + 1]]
# to choose by hand instead, uncomment and edit:
# ts_pick_feats = c('brent_usd_barrel', 'war')
ts_pick_feats

#--------- 2. THE COMPARISON TABLE ---------#
# with so few rows the rolling CV only has a handful of predictions, so treat it as rough
ts_model_table = model_table(ts_prune, ts_response, ts_train_z, ts_test_z, ts_pick_step)
print(ts_model_table, digits = 3)
save_plot(paste0('final_ts_01_model_selection_', ts_mode),
          plot_selection(ts_prune$history, ts_pick_step, tol, 'Time series model: choosing the final model'))

#--------- 3. FIT THE WINNER ON ALL ROWS ---------#
ts_final_formula = make_formula(ts_response, ts_pick_feats)
ts_best = lm(ts_final_formula, data = tsd)
ts_best_z = lm(ts_final_formula, data = tsd_z_all)
ts_full_all = lm(make_formula(ts_response, ts_features), data = tsd)

cat('\nFINAL TIME SERIES MODEL (', ts_mode, ')\n')
equation(ts_best, ts_response)
summary(ts_best)$r.squared; summary(ts_best)$adj.r.squared

#--------- 4. FORMULA, HYPOTHESES, TEST, DECISION ---------#
# CAUTION: these tests assume the errors are independent. Neighbouring days are not, so the
# regular p values are too optimistic. We show them AND the Newey West version, and the
# decision should follow the robust one
overall_f_test(ts_best)
dwtest(ts_best) # H0: no first order autocorrelation. Small p = the problem is real
bgtest(ts_best, order = 1)
ts_tests_plain = hypothesis_table(ts_best)
ts_tests_robust = hypothesis_table(ts_best, vcov_fn = function(m) NeweyWest(m, lag = 2, prewhite = FALSE))
print(ts_tests_plain, digits = 3)
print(ts_tests_robust, digits = 3) # use this one for the decision
if (length(ts_pick_feats) < length(ts_features)) nested_f_test(ts_best, ts_full_all)

#--------- 5. PLOTS FOR THE FINAL MODEL ---------#
save_plot(paste0('final_ts_02_coefficients_', ts_mode),
          coef_plot(ts_best_z, 'FINAL time series model: coefficients (numeric features in sd units)'))
save_plot(paste0('final_ts_03_diagnostics_', ts_mode), diagnostics(ts_best), height = 900)
save_plot(paste0('final_ts_04_cooks_distance_', ts_mode), cooks_plot(ts_best, format(tsd$date, '%m-%d')))
save_plot(paste0('final_ts_05_acf_residuals_', ts_mode),
          acf(residuals(ts_best), main = 'FINAL time series model: ACF of Residuals'))

save_plot(paste0('final_ts_06_residuals_over_time_', ts_mode), {
  plot(tsd$date, residuals(ts_best), type = 'b', pch = 16, col = ifelse(tsd$war == 1, 'red', 'blue'),
       main = 'FINAL time series model: Residuals Over Time', xlab = 'date', ylab = 'residual')
  abline(h = 0, lty = 2)
  abline(v = as.Date('2026-02-28'), col = 'red', lty = 2)
})

# actual vs predicted over time, fit on the unshaded rows only, the shaded rows are the spike week
ts_best_train = lm(ts_final_formula, data = ts_train_z)
ts_best_pred = suppressWarnings(predict(ts_best_train, newdata = tsd_z_train))
save_plot(paste0('final_ts_07_actual_vs_predicted_', ts_mode), {
  plot(tsd$date, tsd[[ts_response]], type = 'b', pch = 16,
       ylim = range(c(tsd[[ts_response]], ts_best_pred), na.rm = TRUE),
       main = 'FINAL time series model: Actual vs Predicted', xlab = 'date', ylab = ts_response)
  rect(min(ts_test$date) - 0.5, par('usr')[3], max(ts_test$date) + 0.5, par('usr')[4],
       col = rgb(0, 0, 0, 0.08), border = NA)
  lines(tsd$date, ts_best_pred, type = 'b', pch = 17, col = 'red', lty = 2)
  legend('topleft', legend = c('actual', 'predicted'), col = c('black', 'red'),
         pch = c(16, 17), lty = c(1, 2), cex = 0.8)
})

write.csv(ts_model_table, file.path(out_dir, paste0('ts_model_selection_table_', ts_mode, '.csv')), row.names = FALSE)
write.csv(ts_tests_robust, file.path(out_dir, paste0('ts_final_model_tests_', ts_mode, '.csv')), row.names = FALSE)

