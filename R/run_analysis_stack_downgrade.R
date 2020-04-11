start <- proc.time()
library(here)
library(tidyverse)
library(data.table)
devtools::install_github("tlverse/sl3@timeseries-overhaul")
library(sl3)

# load helper package
devtools::document("tlversecovidforecast")
devtools::load_all("tlversecovidforecast")

### base library 
grid_params <- list(
  max_depth = c(2, 4, 6, 8),
  eta = c(0.001, 0.01, 0.1, 0.2, 0.3)
)
grid <- expand.grid(grid_params, KEEP.OUT.ATTRS = FALSE)
params_default <- list(nthread = getOption("sl.cores.learners", 1))
xgb_learners <- apply(grid, MARGIN = 1, function(params_tune) {
  do.call(Lrnr_xgboost$new, c(params_default, as.list(params_tune)))
})
lrnr_lasso <- make_learner(Lrnr_glmnet)
lrnr_glm <- make_learner(Lrnr_glm)
lrnr_ranger <- make_learner(Lrnr_ranger)
lrnr_earth <- make_learner(Lrnr_earth)
lrnr_polspline <- make_learner(Lrnr_polspline)

# time series learners
lrnr_alan <- make_learner(Lrnr_alan_pois)
lrnr_gts <- make_learner(Lrnr_gts)
lrnr_arima <- make_learner(Lrnr_arima)
lrnr_expSmooth <- make_learner(Lrnr_expSmooth)

# do one by one for now
lrnr_arima_strat <- Lrnr_multiple_ts$new(learner = lrnr_arima)
lrnr_expSmooth_strat <- Lrnr_multiple_ts$new(learner = lrnr_expSmooth)
### stack of base learners
stack <- make_learner(
  Stack, 
  unlist(list(xgb_learners, lrnr_glm, lrnr_lasso, lrnr_ranger, lrnr_earth,
              lrnr_gts, lrnr_alan, lrnr_arima_strat, lrnr_polspline,
              lrnr_expSmooth_strat), recursive = TRUE)
)

### screeners
screener_lasso <- make_learner(Lrnr_screener_coefs, lrnr_lasso, 
                               threshold = 1e-1)
screener_lasso_flex <- make_learner(Lrnr_screener_coefs, lrnr_lasso, 
                                    threshold = 1e-3)
# pipelines
screen_lasso_pipe <- make_learner(Pipeline, screener_lasso, stack)
screen_lasso_flex_pipe <- make_learner(Pipeline, screener_lasso_flex, stack)

### final stack
stack <- make_learner(Stack, screen_lasso_pipe, screen_lasso_flex_pipe)

# simple covariate screening
sl3_debug_mode()
sl <- generate_learners(stack = stack)
data <- read.csv(here("Data", "training_processed.csv"))
test_data <- read.csv(here("Data", "test_processed.csv"))

# generate case preds
log_cases_task <- generate_task(data, "log_cases")
test_log_cases_task <- generate_task(test_data, "log_cases")

cases_fit <- sl$train(log_cases_task)
cv_risk_table <- cases_fit$cv_risk(loss_squared_error)
print(paste0("CV Risk for Log Cases Outcome", cv_risk_table))

test_preds <- cases_fit$predict(test_log_cases_task)
test_cases_preds <- exp(test_preds) - 1

# generate fatalities preds
log_fatalities_task <- generate_task(data, "log_fatalities")
test_log_fatalities_task <- generate_task(test_data, "log_fatalities")

fatalities_fit <- sl$train(log_fatalities_task)
cv_risk_table2 <- fatalities_fit$cv_risk(loss_squared_error)
print(paste0("CV Risk for Log Fatalities Outcome", cv_risk_table2))
test_preds <- fatalities_fit$predict(test_log_fatalities_task)
test_fatalities_preds <- exp(test_preds) - 1

# generate submission
ex_submission <- fread(here("Data", "week3", "submission.csv"))
names(ex_submission)

submission <- data.table(ForecastId = test_data$forecastid,
                         ConfirmedCases = test_cases_preds,
                         Fatalities = test_fatalities_preds)

submission <- submission[order(ForecastId)]
write.csv(submission, here("Data", "our_submission.csv"), row.names = FALSE)

print(paste0("Run time: ", proc.time()-start))