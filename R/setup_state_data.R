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
  
  
  # TODO: incorporate
  rollback_cols <- c("stay at home rollback", ">50 gatherings rollback", ">500 gatherings rollback", 
                      "restaurant dine-in rollback", "entertainment/gym rollback")
  
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
  
  write.csv(t(as.matrix(wide_data[sample(nrow(wide_data),10),])),"data/column_names.csv")
  ########################## create long data ##################################
  # deaths
  ids <- c("countyFIPS", "CountyName", "StateName",  "State")
  death_cols <- grep("#Deaths",names(wide_data),value=T)
  long_deaths <- melt(wide_data, id.vars = ids, measure.vars=death_cols, 
                      variable.name = "date", value.name = "deaths")
  long_deaths$date <- as.Date(sub("#Deaths_", "", long_deaths$date, fixed = T),
                              format = "%m-%d-%Y")
  
  long_deaths$deaths <- as.numeric(long_deaths$deaths)
  
  # cases
  case_cols <- grep("#Cases",names(wide_data),value=T)
  long_cases <- melt(wide_data, id.vars = ids, measure.vars=case_cols,
                     variable.name = "date", value.name = "cases")
  long_cases$cases <- as.numeric(long_cases$cases)
  long_cases$date <- as.Date(sub("#Cases_", "", long_cases$date, fixed = T),
                             format = "%m-%d-%Y")
  

  # merge
  training <- suppressMessages(data.table(full_join(long_deaths, long_cases)))

  ##################### Aggregate to State level (wide format data) ##########################
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
               "HPSAShortage", "HPSAServedPop", "HPSAUnderservedPop")
  
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
  
  ids <- c("countyFIPS", "CountyName", "StateName", "State", "lat", "lon", 
           "STATEFP", "COUNTYFP", "CensusRegionCode", "CensusDivisionCode", 
           "FederalRegionCode", "SSABeneficiaryCode", "CoreBasedStatAreaCode(CBSA)Metropolitan/Micropolitan2018", 
           "CoreBasedStatAreaName(CBSA)Metropolitan/Micropolitan2018", "CBSAIndicatorCode0=Not,1=Metro,2=Micro2018", 
           "CBSACountyStatusCentralorOutlying2018", "MetropolitanDivisionCode2018", 
           "MetropolitanDivisionName2018", "CombinedStatisticalAreaCode2018", 
           "CombinedStatisticalAreaName2018", "UrbanInfluenceCode2013", 
           "Economic-DependntTypologyCode2015", "Farming-DependentTypologyCode2015", 
           "Mining-DependentTypologyCode2015", "Manufacturing-DepTypologyCode2015", 
           "Fed/StGovt-DepdntTypolgyCodeFederal/StateGovernment2015", "RecreationTypolpgyCode2015", 
           "Nonspecializd-DepTypologyCode2015", "LowEducationTypologyCode2015", 
           "LowEmploymentTypologyCode2015", "HighPovertyTypologyCode2014", 
           "PersistentPovrtyTypologyCode2014", "PersistentChildPovTypolCodeRelatedChildren2015", 
           "PopulationLossTypologyCode2015", "RetirementDestnatnTyplgyCode2015", 
           "BEAEconomicAreaCode2004", "BEAComponentEconomcAreaCode2004", 
           "BEAEconomicAreaName2004", "BEAComponentEconomcAreaName2004", 
           "HPSACode-PrimaryCare05/191=Whole,2=PartCounty2019", "HPSACode-PrimaryCare05/181=Whole,2=PartCounty2018", 
           "HPSACode-PrimaryCare05/171=Whole,2=PartCounty2017", "HPSACode-PrimaryCare05/161=Whole,2=PartCounty2016", 
           "HPSACode-PrimaryCare06/151=Whole,2=PartCounty2015", "HPSACode-PrimaryCare12/101=Whole,2=PartCounty2010", 
           "HPSACode-Dentists05/191=Whole,2=PartCounty2019", "HPSACode-Dentists05/181=Whole,2=PartCounty2018", 
           "HPSACode-Dentists05/171=Whole,2=PartCounty2017", "HPSACode-Dentists05/161=Whole,2=PartCounty2016", 
           "HPSACode-Dentists06/151=Whole,2=PartCounty2015", "HPSACode-Dentists12/101=Whole,2=PartCounty2010", 
           "HPSACode-MentalHealth05/191=Whole,2=PartCounty2019", "HPSACode-MentalHealth05/181=Whole,2=PartCounty2018", 
           "HPSACode-MentalHealth05/171=Whole,2=PartCounty2017", "HPSACode-MentalHealth05/161=Whole,2=PartCounty2016", 
           "HPSACode-MentalHealth06/151=Whole,2=PartCounty2015", "HPSACode-MentalHealth12/101=Whole,2=PartCounty2010", 
           "ContiguousCounty#1", "ContiguousCounty#2", "ContiguousCounty#3", 
           "ContiguousCounty#4", "ContiguousCounty#5", "ContiguousCounty#6", 
           "ContiguousCounty#7", "ContiguousCounty#8", "ContiguousCounty#9", 
           "ContiguousCounty#10", "ContiguousCounty#11", "ContiguousCounty#12", 
           "ContiguousCounty#13", "ContiguousCounty#14")
  
  # try to guess aggregation for new columns
  # TODO: go through these by hand and check how to handle the aggregation
  other_cols <- setdiff(names(wide_data),c(ids, to_sum,to_mean,to_first, case_cols, death_cols))
  other_cols <- setdiff(other_cols, c("Primary Care Physicians Ratio", "Dentist Ratio", "Mental Health Provider Ratio"))
  # drop anything with 2020 (probably time varying)
  other_cols <- other_cols[!grepl("2020",other_cols)]
  other_cols <- setdiff(other_cols, rollback_cols)
  oc_classes <- wide_data[,sapply(.SD,data.class),.SDcols=other_cols]
  other_cols <- other_cols[oc_classes%in%c("numeric","integer")]
  probably_integer <- wide_data[,sapply(.SD,function(x)all((x>=0) & (floor(x)==x),na.rm=T)),.SDcols = other_cols]

  to_sum <- c(to_sum, other_cols[which(probably_integer)])
  to_mean <- c(to_mean, other_cols[which(!probably_integer)])
  
  
  state_means <- wide_data[,lapply(.SD,mean,na.rm=T),.SDcols=to_mean, by=list(State)]
  state_sums <- wide_data[,lapply(.SD,sum,na.rm=T),.SDcols=to_sum, by=list(State)]
  state_first <- wide_data[,lapply(.SD,function(x)x[1]),.SDcols=to_first, by=list(State)]
  
  ids <- c("State")
  state_data <- merge(state_first, state_sums, by=ids)
  state_data <- merge(state_data, state_means, by=ids)

  data <- state_data
  
  
  # add in outcomes from long format data
  outcomes <- c("cases","deaths")
  state_training <- training[,lapply(.SD,sum,na.rm=T),.SDcols=outcomes, by=list(State,date)]
  data <- merge(data,state_training, by = ids)
  
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
  
  # google mobility data 
  # https://github.com/Yu-Group/covid19-severity-prediction/tree/master/data/county_level/raw/google_mobility/README.md
  gmd_format <- "[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}_.*"
  gmd_cols <- grep(gmd_format, names(wide_data), value=T)
  long_gmd <- melt(wide_data, id.vars = ids, measure.vars=gmd_cols,
                   variable.name = "variable", value.name = "value")
  
  long_gmd[,date:=as.Date(sub("_.*", "", variable, fixed = T),
                          format = "%Y-%m-%d")]
  long_gmd[,gmd_location:=gsub(".*_","",variable)]
  state_gmd <- long_gmd[,list(value=mean(value,na.rm=T)),by=list(State,date,gmd_location)]
  gmd_processed <- dcast(state_gmd, State+date~gmd_location, value.var="value", fill=0)
  gmd_covs <- setdiff(names(gmd_processed),c("State","date"))
  data <- merge(data, gmd_processed, by=c("State","date"), all.x=TRUE)
  
  tv_covariates <- c("days",cd_names, ind_names, outcomes,gmd_covs)

  ##########
  # add logged covariates
  # data runs out of allocated columns about here
  data <- alloc.col(data,length(to_sum)*3)
  
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
  print("4/7: Imputing covariates")
  
  
  covariates <- c(baseline_covariates, tv_covariates)
  X <- data[,covariates, with = FALSE]
  processedX <- process_data(X, strata = NULL)
  set(data, , names(processedX), processedX)
  
  # I think just rolling things up handled NAs so ignoring this for now
  covariate_NAs <- unlist(data[,lapply(.SD,function(x)sum(is.na(x))),.SDcols=c(baseline_covariates, tv_covariates)])
  cv_w_missing <- names(covariate_NAs[which(covariate_NAs!=0)])
  if(any(covariate_NAs!=0)){
    warning("found NAs in covariates. dropping these for now")
    baseline_covariates <- setdiff(baseline_covariates, cv_w_missing)
    tv_covariates <- setdiff(tv_covariates, cv_w_missing)
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
