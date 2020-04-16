library(here)
library(tidyverse)
library(data.table)
remotes::install_github("tlverse/origami@devel")
remotes::install_github("tlverse/sl3@timeseries-overhaul")
library(future)
library(sl3)

data <- read_csv(here("Data", "training_processed.csv"))
test_data <- read_csv(here("Data", "test_processed.csv"))
test_data$ForecastId <- test_data$forecastid
submission <- fread("Data/tlverse_submission_na.csv")
submission <- merge(test_data, submission, by=c("ForecastId"))
submission[,per_cases:=mean(ConfirmedCases,na.rm=T)/mean(population2020),by=list(continent, days)]
submission[,per_fats:=mean(Fatalities,na.rm=T)/mean(population2020),by=list(continent, days)]
submission[,imp_cases:=per_cases*population2020]
submission[,imp_fats:=per_fats*population2020]
submission[is.na(ConfirmedCases),ConfirmedCases:=imp_cases]
submission[is.na(Fatalities),ConfirmedCases:=imp_fats]
submission[is.na(Fatalities)]
# generate submission
ex_submission <- fread(here("Data", "week2", "submission.csv"))
submission <- as.data.table(list(test_data$forecastid, test_cases_preds,
                                 test_fatalities_preds))
setnames(submission, names(ex_submission))
submission <- submission[order(ForecastId)]
write_csv(submission, here("Data", "tlverse_submission.csv"))
