library(devtools)
library(here)
document("tlversecovidforecast")
load_all("tlversecovidforecast")
# run this to regenerate processed data
# setup_data()


# simple covariate screening
sl <- generate_learners()
sl3_debug_mode()


data <- fread(here("Data/training_processed.csv"))
test_data <- fread(here("Data/test_processed.csv"))

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
test_preds <- fit$predict(test_log_fatalities_task)
test_fatalities_preds <- exp(test_preds) - 1

# generate submission
ex_submission <- fread(here("Data/week2/submission.csv"))
names(ex_submission)

submission <- data.table(ForecastId=test_data$forecastid,
                         ConfirmedCases=test_cases_preds,
                         Fatalities=test_fatalities_preds)

submission <- submission[order(ForecastId)]
write.csv(submission, here("Data/our_submission.csv"),row.names=FALSE)
