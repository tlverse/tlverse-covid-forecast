library(here)
library(tidyverse)
library(data.table)
remotes::install_github("tlverse/sl3@timeseries-overhaul")
library(future)
library(sl3)

# data.table threading vs. future (probably unnecessary)
dt_cores <- as.integer(unname(round(0.25 * availableCores())))
future_cores <- as.integer(availableCores() - dt_cores)
setDTthreads(dt_cores)
plan(multiprocess, workers = future_cores)

# load helper package
devtools::document(here("tlversecovidforecast"))
devtools::load_all(here("tlversecovidforecast"))

# read data, make learners 
sl3_debug_mode()
sl <- generate_learners()
data <- read_csv(here("Data", "training_processed.csv"))
test_data <- read_csv(here("Data", "test_processed.csv"))

# generate case preds
log_cases_task <- generate_task(data, "log_cases")
test_log_cases_task <- generate_task(test_data, "log_cases")

cases_fit <- sl$train(log_cases_task)
cv_risk_table <- cases_fit$cv_risk(loss_squared_error)
print(cv_risk_table)

test_preds <- cases_fit$predict(test_log_cases_task)
test_cases_preds <- exp(test_preds) - 1

# generate fatalities preds
log_fatalities_task <- generate_task(data, "log_fatalities")
test_log_fatalities_task <- generate_task(test_data, "log_fatalities")

fatalities_fit <- sl$train(log_fatalities_task)
test_preds <- fatalities_fit$predict(test_log_fatalities_task)
test_fatalities_preds <- exp(test_preds) - 1

# generate submission
ex_submission <- fread(here("Data", "week2", "submission.csv"))
names(ex_submission)

submission <- data.table(ForecastId = test_data$forecastid,
                         ConfirmedCases = test_cases_preds,
                         Fatalities = test_fatalities_preds)
submission <- submission[order(ForecastId)]
write_csv(submission, here("Data", "tlverse_submission.csv"))
