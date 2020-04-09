library(here)
library(tidyverse)
library(data.table)
devtools::install_github("tlverse/sl3@timeseries-overhaul")
library(sl3)
library(ggplot2)
# load helper package
devtools::document("tlversecovidforecast")
devtools::load_all("tlversecovidforecast")

# simple covariate screening
sl3_debug_mode()
data <- read.csv(here("Data", "training_processed.csv"))
test_data <- read.csv(here("Data", "test_processed.csv"))

setDT(data)

# generate case preds
log_cases_task <- generate_task(data, "log_cases")
lrnr_alan <- make_learner(Lrnr_alan_pois)
lrnr_alan_cv <- make_learner(Lrnr_cv, lrnr_alan)

# fit on italy and check resubstitution performance
italy_task <- log_cases_task[data$region=="Italy"]
italy_fit <- lrnr_alan$train(italy_task)
italy_data <- data[data$region=="Italy",]
italy_data$preds <- italy_fit$predict()

ggplot(italy_data,aes(x=days,y=log_cases))+geom_point()+geom_line(aes(y=preds))

# fit on full data and check xcv performance
cv_fit <- lrnr_alan_cv$train(log_cases_task)
cv_risk_table <- cv_fit$cv_risk(loss_squared_error)
print(cv_risk_table)