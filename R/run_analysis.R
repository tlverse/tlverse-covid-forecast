library(here)
library(tidyverse)
library(data.table)
remotes::install_github("tlverse/origami@devel")
remotes::install_github("tlverse/sl3@timeseries-overhaul")
library(future)
library(sl3)

# TODO: why aren't these being loaded automatically?
library(hts)
library(earth)

batch_size <- 5
# get/set miniconda python for kerasR
reticulate::use_python(system("which python3"))

# data.table threading vs. future (probably unnecessary)
# setDTthreads(2)
# plan(multiprocess, workers = availableCores() - getDTthreads())

# load helper package
devtools::document(here("tlversecovidforecast"))
devtools::load_all(here("tlversecovidforecast"))

# make SL learners and read in data, 
# both fread and read_csv switch of facs to chars, read.csv avoids this
sl3_debug_mode()
sl <- generate_learners()
load(here("Data", "training.Rdata"))
data <- training
load(here("Data", "test.Rdata"))
test_data <- test
setDT(data)
setDT(test_data)
data <- data[!(days %in% unique(test_data$days))]

# generate case task and training predictions
log_cases_task <- generate_task(data, "log_cases", batch = batch_size)
cases_fit <- sl$train(log_cases_task)
cv_risk_table <- cases_fit$cv_risk(loss_squared_error)
cv_risk_table$coefficients <- c(strat_metalearner_coefs(cases_fit), NA)
print(cv_risk_table)

# for testing data, generate case task and predictions
test_log_cases_task <- generate_task(test_data, "log_cases")
test_preds <- cases_fit$predict(test_log_cases_task)
test_cases_preds <- exp(test_preds) - 1

# generate fatalities predictions
log_fatalities_task <- generate_task(data, "log_deaths", batch = batch_size)
fatalities_fit <- sl$train(log_fatalities_task)
test_log_fatalities_task <- generate_task(test_data, "log_deaths")
test_preds <- fatalities_fit$predict(test_log_fatalities_task)
test_fatalities_preds <- exp(test_preds) - 1

# generate tabular output
tbl <- as.data.table(list(test_data$countyFIPS, test_data$date, test_cases_preds,
                          test_fatalities_preds))
setnames(tbl, c("countyFIPS", "date", "cases_preds", "deaths_preds"))
tbl_ordered <- setorder(tbl, "countyFIPS", "date")
write_csv(tbl_ordered, here("Data", "predictions.csv"))
