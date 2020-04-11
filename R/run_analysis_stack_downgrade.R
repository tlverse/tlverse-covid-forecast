library(here)
library(tidyverse)
library(data.table)
remotes::install_github("tlverse/origami@devel")
remotes::install_github("tlverse/sl3@timeseries-overhaul")
library(future)
library(sl3)

# get/set miniconda python for kerasR
reticulate::use_python(system("which python3"))

# data.table threading vs. future (probably unnecessary)
dt_cores <- as.integer(unname(round(0.2 * availableCores())))
future_cores <- as.integer(unname(availableCores()) - dt_cores)
setDTthreads(dt_cores)
plan(multiprocess, workers = future_cores)

# load helper package
devtools::document(here("tlversecovidforecast"))
devtools::load_all(here("tlversecovidforecast"))

### base library 
grid_params <- list(
  max_depth = c(2, 5, 8),
  eta = c(0.001, 0.05, 0.2),
  nrounds = c(50, 100)
) %>% expand.grid(KEEP.OUT.ATTRS = FALSE)
params_default <- list(nthread = getOption("sl.cores.learners", 1))
xgb_lrnrs <- apply(grid_params, MARGIN = 1, function(params_tune) {
  do.call(Lrnr_xgboost$new, c(params_default, as.list(params_tune)))
})
mean_lrnr <- Lrnr_mean$new()
glm_lrnr <- Lrnr_glm$new()
ridge_lrnr <- Lrnr_glmnet$new(alpha = 0, nfolds = 3)
lasso_lrnr <- Lrnr_glmnet$new(alpha = 1, nfolds = 3)
enet_lrnr_reg25 <- Lrnr_glmnet$new(alpha = 0.25, nfolds = 3)
enet_lrnr_reg50 <- Lrnr_glmnet$new(alpha = 0.50, nfolds = 3)
enet_lrnr_reg75 <- Lrnr_glmnet$new(alpha = 0.75, nfolds = 3)
ranger_lrnr_base <- Lrnr_ranger$new()
ranger_lrnr_nt50 <- Lrnr_ranger$new(num.trees = 50)
ranger_lrnr_nt100 <- Lrnr_ranger$new(num.trees = 100)
ranger_lrnr_nt500 <- Lrnr_ranger$new(num.trees = 500)
earth_lrnr <- make_learner(Lrnr_earth)

# time series learners
alan_lrnr <- make_learner(Lrnr_alan_pois)
gts_lrnr <- make_learner(Lrnr_gts)
arima_lrnr <- make_learner(Lrnr_arima)
expsmooth_lrnr <- make_learner(Lrnr_expSmooth)
arima_strat_lrnr <- Lrnr_multiple_ts$new(learner = arima_lrnr)
expsmooth_strat_lrnr <- Lrnr_multiple_ts$new(learner = expsmooth_lrnr)

# library for Stack
stack_lib <- unlist(list(mean_lrnr, glm_lrnr, ridge_lrnr, lasso_lrnr,
                         enet_lrnr_reg25, enet_lrnr_reg50, enet_lrnr_reg75,
                         ranger_lrnr_base, ranger_lrnr_nt50, ranger_lrnr_nt100,
                         ranger_lrnr_nt500, earth_lrnr, xgb_lrnrs, alan_lrnr,
                         gts_lrnr, arima_strat_lrnr, expsmooth_strat_lrnr),
                    recursive = TRUE)

# make stack for SL
stack_lrnrs <- make_learner(Stack, stack_lib)

# LASSO screener
#screener_lasso <- make_learner(Lrnr_screener_coefs, lrnr_lasso,
                               #threshold = 1e-1)
#screener_lasso_flex <- make_learner(Lrnr_screener_coefs, lrnr_lasso,
                                    #threshold = 1e-3)
screener_lasso <- make_learner(Lrnr_screener_coefs, lasso_lrnr,
                               threshold = 1e-2)

# pipelines
#screen_lasso_pipe <- make_learner(Pipeline, screener_lasso, stack)
#screen_lasso_flex_pipe <- make_learner(Pipeline, screener_lasso_flex, stack)
screen_lasso_pipe <- make_learner(Pipeline, screener_lasso, stack_lrnrs)

# final stack for SL
#stack_screeners <- make_learner(Stack, screen_lasso_pipe,
                                #screen_lasso_flex_pipe)
sl_stack_run_forrest_run <- make_learner(Stack, screen_lasso_pipe)

# simple covariate screening
sl3_debug_mode()
sl <- generate_learners(stack = sl_stack_run_forrest_run)
data <- read_csv(here("Data", "training_processed.csv"))
test_data <- read_csv(here("Data", "test_processed.csv"))

# generate case task and training predictions
log_cases_task <- generate_task(data, "log_cases")
cases_fit <- sl$train(log_cases_task)
cv_risk_table_cases <- cases_fit$cv_risk(loss_squared_error)
print(cv_risk_table_cases)

# for testing data, generate case task and predictions
test_log_cases_task <- generate_task(test_data, "log_cases")
test_preds <- cases_fit$predict(test_log_cases_task)
test_cases_preds <- exp(test_preds) - 1

# train on fatalities
log_fatalities_task <- generate_task(data, "log_fatalities")
fatalities_fit <- sl$train(log_fatalities_task)
cv_risk_table_fatalities <- fatalities_fit$cv_risk(loss_squared_error)
print(cv_risk_table_fatalities)

# for testing data, generate case task and predictions
test_log_fatalities_task <- generate_task(test_data, "log_fatalities")
test_preds <- fatalities_fit$predict(test_log_fatalities_task)
test_fatalities_preds <- exp(test_preds) - 1

# generate submission
ex_submission <- fread(here("Data", "week2", "submission.csv"))
submission <- as.data.table(list(test_data$forecastid, test_cases_preds,
                                 test_fatalities_preds))
setnames(submission, names(ex_submission))
submission <- submission[order(ForecastId)]
write_csv(submission, here("Data", "tlverse_submission.csv"))
