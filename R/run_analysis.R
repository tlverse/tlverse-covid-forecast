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
setDTthreads(2)
plan(multiprocess, workers = availableCores() - getDTthreads())

# load helper package
devtools::document(here("tlversecovidforecast"))
devtools::load_all(here("tlversecovidforecast"))

# make SL learners and read in data
sl3_debug_mode()
sl <- generate_learners()
data <- read_csv(here("Data", "training_processed.csv"))
test_data <- read_csv(here("Data", "test_processed.csv"))

# generate case task and training predictions
log_cases_task <- generate_task(data, "log_cases")
cases_fit <- sl$train(log_cases_task)
cv_risk_table <- cases_fit$cv_risk(loss_squared_error)
print(cv_risk_table)

# for testing data, generate case task and predictions
test_log_cases_task <- generate_task(test_data, "log_cases")
test_preds <- cases_fit$predict(test_log_cases_task)
test_cases_preds <- exp(test_preds) - 1

# generate fatalities predictions
log_fatalities_task <- generate_task(data, "log_fatalities")
fatalities_fit <- sl$train(log_fatalities_task)
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
