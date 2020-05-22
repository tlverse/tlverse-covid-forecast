# TODO: edit python script to read data from Yu group url, save merged file 
# TODO: handle cancelling of restrictions
# TODO: handle list_cols, in particular "neighbor_deaths" & "neighbor_cases" 

# library(data.table)
# library(zoo)
# library(here)
# library(reshape2)
# library(tidyverse)
# library(readr)
# library(dplyr)
# library(tidyr)

#' @import zoo
#' @import data.table
#' @import zoo
#' @import here
#' @import reshape2
#' @import tidyverse
#' @import readr
#' @import dplyr
#' @import tidyr
#
setup_data <- function() {
  
  ############################# preprocess #####################################
  print("1/6: Loading Yu group data")
  
  wide_data <- suppressMessages(data.table(
    read_csv(here::here("Data", "Yu_merged_data", "merged_data_20200507-143721.csv"),
             guess_max = 5000)))
  # wide_data <- suppressMessages(data.table(
  #   read_csv(here::here("Data", "Yu_merged_data", "merged_data.csv"), 
  #            guess_max = 5000)))
  
  if(length(unique(wide_data$countyFIPS)) != nrow(wide_data)){
    stop("Error: Number of unique counties does not equal number of rows in the data")
  }
  
  # format restriction dates
  dates <- c("stay at home", ">50 gatherings", ">500 gatherings", 
             "public schools", "restaurant dine-in", "entertainment/gym", 
             "federal guidelines", "foreign travel ban")
  date_data <- wide_data[, lapply(.SD, function(x) as.Date(x,origin="0-12-30")),
                         .SDcols = dates]
  colnames(date_data) <- c(
    "init_restrict_stayhome","init_restrict_gather50","init_restrict_gather500", 
    "init_restrict_school","init_restrict_dining",
    "init_restrict_entertain", "init_restrict_federal", 
    "init_restrict_travel"
    )
  
  # remove irrelevant and list cols
  list_cols <- c("deaths", "cases", "neighbor_deaths", "neighbor_cases")
  to_rm <- c("tot_deaths", "tot_cases")
  wide_data <- cbind(wide_data[,-c(list_cols, to_rm, dates), with = FALSE],
                     date_data)
  
  ########################## create long data ##################################
  # deaths
  wide_deaths <- wide_data[, !grepl("#Cases", colnames(wide_data)), with=FALSE]
  id_deaths <- colnames(wide_deaths)[!grepl("#Deaths", colnames(wide_deaths))]
  long_deaths <- data.table(melt(wide_deaths, id.vars = id_deaths, 
                                 variable.name = "date", value.name = "deaths"))
  long_deaths$date <- as.Date(sub("#Deaths_", "", long_deaths$date, fixed = T),
                              format = "%m-%d-%Y")
  # cases
  wide_cases <- wide_data[, !grepl("#Deaths", colnames(wide_data)), with=FALSE]
  id_cases <- colnames(wide_cases)[!grepl("#Cases", colnames(wide_cases))]
  long_cases <- data.table(melt(wide_cases, id.vars = id_cases, 
                                variable.name = "date", value.name = "cases"))
  long_cases$date <- as.Date(sub("#Cases_", "", long_cases$date, fixed = T),
                             format = "%m-%d-%Y")
  
  # merge
  training <- suppressMessages(data.table(full_join(long_deaths, long_cases)))

  ##################### create 14-day ahead test data ##########################
  print("2/6: Creating 14-day ahead test data")
  
  # add covs in training
  covs <- distinct(training[,-c("date", "deaths", "cases"), with = FALSE])
  if(nrow(covs) != nrow(wide_data)){
    stop("Error: Cannot make 14-day ahead test data due to time-varying covariates.")
  }
  test <- covs[rep(seq_len(nrow(covs)), 14), ]
  test <- setorder(test, "countyFIPS")
  future_dates <- training[,lapply(.SD, function(x) max(x)+1:14), 
                           .SDcols = "date", by = "countyFIPS"]
  future_dates <- setorder(future_dates, "countyFIPS", "date")
  test$date <- future_dates$date
  test$cases <- as.numeric(rep(NA, nrow(test)))
  test$deaths <- as.numeric(rep(NA, nrow(test)))

  nrow_test_data <- nrow(test)
  nrow_training_data <- nrow(training) 
  
  all <- suppressMessages(data.table(full_join(training, test)))
  all <- setorder(all, "countyFIPS", "date")
  
  ################################ add features ################################
  print("3/6: Adding features")
  
  all$weekday <- as.factor(weekdays(all$date))
  
  all_lags <- all %>%
    group_by(countyFIPS) %>%
    mutate(deaths_lag14 = lag(deaths, 14, default = 0)) %>%
    mutate(cases_lag14 = lag(cases, 14, default = 0)) %>%
    mutate(deaths_lag14 = lag(deaths, 15, default = 0)) %>%
    mutate(cases_lag14 = lag(cases, 15, default = 0)) %>%
    mutate(deaths_lag14 = lag(deaths, 16, default = 0)) %>%
    mutate(cases_lag14 = lag(cases, 16, default = 0)) %>%
    mutate(deaths_lag15total = lag(cumsum(deaths), 17, default = 0)) %>%
    mutate(cases_lag15total = lag(cumsum(cases), 17, default = 0))
  
  all_restrictions <- all_lags %>%
    group_by(countyFIPS) %>%
    mutate(restrict_stayhome = ifelse((is.na(init_restrict_stayhome) | 
                                      (date <= init_restrict_stayhome)), 0, 1)) %>%
    mutate(restrict_gather50 = ifelse((is.na(init_restrict_gather50) | 
                                      (date <= init_restrict_gather50)), 0, 1)) %>%
    mutate(restrict_gather500 = ifelse((is.na(init_restrict_gather500) |
                                       (date <= init_restrict_gather500)), 0, 1)) %>%
    mutate(restrict_school = ifelse((is.na(init_restrict_school) |
                                    (date <= init_restrict_school)), 0, 1)) %>%
    mutate(restrict_dining = ifelse((is.na(init_restrict_dining) | 
                                    (date <= init_restrict_dining)), 0, 1)) %>%
    mutate(restrict_entertain = ifelse((is.na(init_restrict_entertain) | 
                                       (date <= init_restrict_entertain)), 0, 1)) %>%
    mutate(restrict_federal = ifelse((is.na(init_restrict_federal) | 
                                     (date <= init_restrict_federal)), 0, 1)) %>%
    mutate(restrict_travel = ifelse((is.na(init_restrict_travel) | 
                                    (date <= init_restrict_travel)), 0, 1)) 
  
  data <- data.table(all_restrictions)
  
  case_days_or_zero <- function(date, first_date) {
    case_days <- as.numeric(difftime(date, first_date, unit = "days"))
    case_days[which(is.na(case_days) | (!is.finite(case_days)) | (case_days < 0))] <- 0
    return(case_days)
  }
  
  data[, days := as.numeric(difftime(date, min(date), unit = "days"))]
  data[, first_case_date := min(date[cases > 0], na.rm = TRUE), by = list(countyFIPS)]
  data[, tenth_case_date := min(date[cases > 10], na.rm = TRUE), by = list(countyFIPS)]
  data[, hundreth_case_date := min(date[cases > 100], na.rm = TRUE), by = list(countyFIPS)]
  data[, first_death_date := min(date[deaths > 0], na.rm = TRUE), by = list(countyFIPS)]
  data[, tenth_death_date := min(date[deaths > 10], na.rm = TRUE), by = list(countyFIPS)]
  data[, hundreth_death_date := min(date[deaths > 100], na.rm = TRUE), by = list(countyFIPS)]
  data[, case_days := case_days_or_zero(date, first_case_date)]
  data[, case10_days := case_days_or_zero(date, tenth_case_date)]
  data[, case100_days := case_days_or_zero(date, hundreth_case_date)]
  data[, death_days := case_days_or_zero(date, first_death_date)]
  data[, death10_days := case_days_or_zero(date, tenth_death_date)]
  data[, death100_days := case_days_or_zero(date, hundreth_death_date)]
  data[, restrict_stayhome_days := case_days_or_zero(date, init_restrict_stayhome)]
  data[, restrict_gather50_days := case_days_or_zero(date, init_restrict_gather50)]
  data[, restrict_gather500_days := case_days_or_zero(date, init_restrict_gather500)]
  data[, restrict_school_days := case_days_or_zero(date, init_restrict_school)]
  data[, restrict_dining_days := case_days_or_zero(date, init_restrict_dining)]
  data[, restrict_entertain_days := case_days_or_zero(date, init_restrict_entertain)]
  data[, restrict_federal_days := case_days_or_zero(date, init_restrict_federal)]
  data[, restrict_travel_days := case_days_or_zero(date, init_restrict_travel)]

  ################################## imputation ################################
  print("4/6: Imputing covariates, stratified by state")
  
  # sl3-style imputation by state
  dates <- c("init_restrict_travel","init_restrict_federal","first_case_date",
             "init_restrict_entertain","init_restrict_dining","tenth_case_date",
             "init_restrict_school","init_restrict_gather500","first_death_date",
             "init_restrict_gather50", "init_restrict_stayhome","tenth_death_date",
             "hundreth_case_date", "hundreth_death_date", "date")
  outcomes <- c("cases", "deaths")
  geo_names <- c("State", "CountyName", "StateName", "CensusRegionName", 
                 "CensusDivisionName")
  X <- data[,-c(dates, outcomes, geo_names), with = FALSE]
  processedX <- process_data(X, strata = "STATEFP")
  if(sum(is.na(processedX)) > 0){
    print("NA remain after stratified imputation, imputing with all data")
    processedX <- process_data(processedX, "delta_")
  }
  
  final <- data.table(data[,c(dates,outcomes,geo_names),with=FALSE],processedX)

  ############################### log features #################################
  print("5/6: Adding logged continuous features")
  
  final[, log_cases := log(cases + 1)]
  final[, log_deaths := log(deaths + 1)]
  
  to_log <- c(
    "deaths_lag14", "cases_lag14", "deaths_lag15total", "cases_lag15total", 
    "Rural-UrbanContinuumCode2013", "PopulationEstimate2018", "PopTotalMale2017", 
    "PopTotalFemale2017", "FracMale2017", "PopulationEstimate65+2017", 
    "PopulationDensityperSqMile2010", "CensusPopulation2010", "MedianAge2010", 
    "#EligibleforMedicare2018", "MedicareEnrollment,AgedTot2017", 
    "DiabetesPercentage", "HeartDiseaseMortality", "StrokeMortality", 
    "Smokers_Percentage", "RespMortalityRate2014", "#FTEHospitalTotal2017", 
    "TotalM.D.'s,TotNon-FedandFed2017", "#HospParticipatinginNetwork2017", 
    "#Hospitals", "#ICU_beds", "dem_to_rep_ratio", "PopMale<52010", 
    "PopFmle<52010", "PopMale5-92010", "PopFmle5-92010", "PopMale10-142010", 
    "PopFmle10-142010", "PopMale15-192010", "PopFmle15-192010", "PopMale20-242010", 
    "PopFmle20-242010", "PopMale25-292010", "PopFmle25-292010", "PopMale30-342010", 
    "PopFmle30-342010", "PopMale35-442010", "PopFmle35-442010", "PopMale45-542010",
    "PopFmle45-542010", "PopMale55-592010", "PopFmle55-592010", "PopMale60-642010", 
    "PopFmle60-642010", "PopMale65-742010", "PopFmle65-742010", "PopMale75-842010", 
    "PopFmle75-842010", "PopMale>842010", "PopFmle>842010", "3-YrMortalityAge45-54Years2015-17", 
    "3-YrMortalityAge55-64Years2015-17", "3-YrMortalityAge65-74Years2015-17", 
    "3-YrMortalityAge75-84Years2015-17", "3-YrMortalityAge85+Years2015-17", 
    "HPSAShortage", "HPSAServedPop", "HPSAUnderservedPop"
    )
  logged <- data[, lapply(.SD, function(x) log(x + 1)), .SDcols = to_log]
  log_names <- sprintf("log_%s", to_log)
  setnames(logged, log_names)
  all_final <- data.table(cbind(data, logged))

  ############################## final save ####################################
  print("6/6: Final save")
  
  all_final <- setorder(all_final, "countyFIPS", "date")
  training_final <- all_final[!is.na(all_final$cases), ]
  test_final <- all_final[is.na(all_final$cases), ]
  
  if((nrow_training_data != nrow(training_final)) | (nrow_test_data != nrow(test_final))){
    stop("Error: Final training/test nrows != original training/test nrows")
  }
  
  #write.csv(all_final, file = here("Data", "all.csv"), row.names = FALSE)
  write.csv(training_final, file = here("Data", "training.csv"), row.names = FALSE)
  write.csv(test_final, file = here("Data", "test.csv"), row.names = FALSE)
}
