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
setup_state_data <- function(data_path = "data") {

  ############################# preprocess #####################################
  
  yu_path <- file.path(data_path, "yu_merged_data.csv")
  
  print(paste0("1/7: Loading yu data from ",yu_path))
  wide_data <- fread(yu_path)
  
  # unclear why some states are missing their names
  missing_states <- c("VA","HI","AK")
  missing_states_names <- c("Virginia","Hawaii","Alaska")
  wide_data[StateName%in%missing_states,
            State:=missing_states_names[match(StateName,missing_states)]]
  
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
  
  restriction_cols <- colnames(date_data)
  
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
  
  long_deaths$deaths <- as.numeric(long_deaths$deaths)
  # cases
  wide_cases <- wide_data[, !grepl("#Deaths", colnames(wide_data)), with=FALSE]
  id_cases <- colnames(wide_cases)[!grepl("#Cases", colnames(wide_cases))]
  long_cases <- data.table(melt(wide_cases, id.vars = id_cases, 
                                variable.name = "date", value.name = "cases"))
  long_cases$cases <- as.numeric(long_cases$cases)
  long_cases$date <- as.Date(sub("#Cases_", "", long_cases$date, fixed = T),
                             format = "%m-%d-%Y")
  
  # merge
  training <- suppressMessages(data.table(full_join(long_deaths, long_cases)))

  ##################### Aggregate to State level ##########################
  print("1.5/7: Aggregate to state level")
  
  to_sum <- c( "PopulationEstimate2018", 
               "PopTotalMale2017", "PopTotalFemale2017",
               "PopulationEstimate65+2017", "CensusPopulation2010", 
               "#EligibleforMedicare2018", "MedicareEnrollment,AgedTot2017",
               "3-YrDiabetes2015-17","HeartDiseaseMortality","#FTEHospitalTotal2017",
               "TotalM.D.'s,TotNon-FedandFed2017", "#HospParticipatinginNetwork2017",
               "#Hospitals", "#ICU_beds",
               "PopMale<52010", "PopFmle<52010", "PopMale5-92010", 
               "PopFmle5-92010", "PopMale10-142010", "PopFmle10-142010", "PopMale15-192010", 
               "PopFmle15-192010", "PopMale20-242010", "PopFmle20-242010", "PopMale25-292010", 
               "PopFmle25-292010", "PopMale30-342010", "PopFmle30-342010", "PopMale35-442010", 
               "PopFmle35-442010", "PopMale45-542010", "PopFmle45-542010", "PopMale55-592010", 
               "PopFmle55-592010", "PopMale60-642010", "PopFmle60-642010", "PopMale65-742010", 
               "PopFmle65-742010", "PopMale75-842010", "PopFmle75-842010", "PopMale>842010", 
               "PopFmle>842010", 
               "3-YrMortalityAge15-24Years2015-17", 
               "3-YrMortalityAge25-34Years2015-17", "3-YrMortalityAge35-44Years2015-17", 
               "3-YrMortalityAge45-54Years2015-17", "3-YrMortalityAge55-64Years2015-17", 
               "3-YrMortalityAge65-74Years2015-17", "3-YrMortalityAge75-84Years2015-17", 
               "3-YrMortalityAge85+Years2015-17", "mortality2015-17Estimated",
               "HPSAShortage", "HPSAServedPop", "HPSAUnderservedPop", 
               "deaths", "cases")
  
  # TODO: maybe be more careful with some of these (e.g. lat/lon, restriction dates)
  to_mean <- c("Rural-UrbanContinuumCode2013","POP_LATITUDE", "POP_LONGITUDE", "FracMale2017",
               "PopulationDensityperSqMile2010", "MedianAge2010","DiabetesPercentage",
               "StrokeMortality","Smokers_Percentage","RespMortalityRate2014",
               "dem_to_rep_ratio","SVIPercentile",
               "init_restrict_stayhome", "init_restrict_gather50", "init_restrict_gather500", 
               "init_restrict_school", "init_restrict_dining", "init_restrict_entertain", 
               "init_restrict_federal", "init_restrict_travel")
  
  to_first <- c("CensusRegionName", 
                "CensusDivisionName")
  
  state_means <- training[,lapply(.SD,mean,na.rm=T),.SDcols=to_mean, by=list(State, date)]
  state_sums <- training[,lapply(.SD,sum,na.rm=T),.SDcols=to_sum, by=list(State, date)]
  state_first <- training[,lapply(.SD,function(x)x[1]),.SDcols=to_first, by=list(State, date)]
  
  ids <- c("State")
  state_data <- merge(state_first, state_sums, by=c(ids,"date"))
  state_data <- merge(state_data, state_means, by=c(ids,"date"))

  data <- state_data
  outcomes <- c("cases","deaths")
  # generate diff'd outcomes
  diff_outcomes <- data[,lapply(.SD,function(x)c(0,diff(x))),by=ids,.SDcols=outcomes]
  diff_outcome_names <- sprintf("new_%s",outcomes)
  set(data, , diff_outcome_names, diff_outcomes[,outcomes, with=FALSE])
  outcomes <- c(outcomes, diff_outcome_names)
  
  to_sum <- setdiff(to_sum,outcomes)
  baseline_covariates <- setdiff(c(to_sum,to_mean,to_first),  restriction_cols)
  

  # generate tv covariates
  case_days_or_zero <- function(first_date, date) {
    case_days <- as.numeric(difftime(date, first_date, unit = "days"))
    case_days[which(is.na(case_days) | (!is.finite(case_days)) | (case_days < 0))] <- 0
    return(case_days)
  }
  
  data[, days := as.numeric(difftime(date, min(date), unit = "days"))]
  data[, first_case_date := min(date[cases > 0], na.rm = TRUE), by = ids]
  data[, tenth_case_date := min(date[cases > 10], na.rm = TRUE), by = ids]
  data[, hundreth_case_date := min(date[cases > 100], na.rm = TRUE), by = ids]
  data[, first_death_date := min(date[deaths > 0], na.rm = TRUE), by = ids]
  data[, tenth_death_date := min(date[deaths > 10], na.rm = TRUE), by = ids]
  data[, hundreth_death_date := min(date[deaths > 100], na.rm = TRUE), by = ids]
  
  case_date_cols <- c("first_case_date","tenth_case_date","hundreth_case_date")
  death_date_cols <- c("first_death_date","tenth_death_date","hundreth_death_date")
  to_case_day_cols <- c(restriction_cols, case_date_cols, death_date_cols)
  case_days <- data[,lapply(.SD, case_days_or_zero, date),by=ids, .SDcols=to_case_day_cols]
  indicators <- case_days[,lapply(.SD, function(x)as.numeric(x>0)),by=ids, .SDcols=to_case_day_cols]
  cd_names <- sprintf("cd_%s",to_case_day_cols)
  ind_names <- sprintf("ind_%s",to_case_day_cols)
  
  set(data,,cd_names, case_days[,to_case_day_cols,with=FALSE])  
  set(data,,ind_names, indicators[,to_case_day_cols,with=FALSE])  
  
  tv_covariates <- c("days",cd_names, ind_names, outcomes)

  ##########
  # add logged covariates
  
  bl_log_names <- sprintf("log_%s",to_sum)
  logged_baseline <- data[,lapply(.SD, function(x)log(x+1)),.SDcols=to_sum]
  set(data, , bl_log_names, logged_baseline[,to_sum,with=FALSE])
  baseline_covariates <- c(baseline_covariates, bl_log_names)
  
  to_log_tv <- outcomes
  tv_log_names <- sprintf("log_%s",to_log_tv)
  logged_tv <- data[,lapply(.SD, function(x)log(pmax(x,0)+1)),.SDcols=to_log_tv]
  set(data, , tv_log_names, logged_tv[,to_log_tv,with=FALSE])
  tv_covariates <- c(tv_covariates, tv_log_names)
  
  outcomes <- c(outcomes, tv_log_names)
  
  
  ##################### create lagged outcomes ##########################
  print("2/7: Creating lagged outcomes")
  # this also creates test data by populating the final rows with NAs
  lag_days <- 7
  
  # TODO: consider multiple lags
  lagged_outcomes <- data[,lapply(.SD,lead,lag_days),by=ids,.SDcols=outcomes]
  lo_names <- sprintf("%s_lag%d",outcomes,lag_days)
  set(data,,lo_names, lagged_outcomes[,outcomes,with=FALSE])
  data$outcome_date <- data$date+lubridate::days(lag_days)
  # these are the real outcomes
  outcomes <- lo_names
  
  
  test_indexes <- which(is.na(lagged_outcomes$cases))
  training_indexes <- which(!is.na(lagged_outcomes$cases))
  ################################## imputation ################################
  print("4/7: Imputing covariates, stratified by state")
  
  # I think just rolling things up handled NAs so ignoring this for now
  covariate_NAs <- unlist(data[,lapply(.SD,function(x)sum(is.na(x))),.SDcols=c(baseline_covariates, tv_covariates)])
  covariate_NAs[which(covariate_NAs!=0)]
  if(any(covariate_NAs!=0)){
    stop("found NAs in covariates. Update script to handle this")
  }


  
  ############################## final save ####################################
  output_file <- file.path(data_path, "state_data.rdata")
   
  processed <- list(data=data,
                 ids=ids,
                 outcomes=outcomes,
                 tv_covariates=tv_covariates,
                 baseline_covariates=baseline_covariates,
                 training_indexes = training_indexes,
                 test_indexes = test_indexes,
                 lag = lag_days)
  save(processed, file=output_file)
}
