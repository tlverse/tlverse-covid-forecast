#' @import data.table
generate_analytic_data <- function(){
  main_data <- fread("Data/covid19-global-forecasting-week-1/train.csv")
  old_names <- c("Id", "Province/State", "Country/Region", "Lat", "Long", "Date", 
                 "ConfirmedCases", "Fatalities")
  new_names <- c("id","state", "country", "lat", "long", "date", "cases", "deaths")
  
  setnames(main_data, old_names, new_names)
  main_data$date <- as.Date(main_data$date)
  
  ###TODO: merge in helper datasets
  #1. Add intervention data (school, restrictions,quarantine)
  #2. Add population, density, urban population, median age, sex info
  #3. Add test and test population
  #4. Add percent smokers and lung disease info
  
  restrictions_data <- fread("Data/covid19-global-forecasting-week-1/restrictions_info_data.csv")
  
  main_data$date <- as.Date(main_data$date)
  restrictions_data$schools_national_date <- as.Date(restrictions_data$schools_national_date)
  restrictions_data$schools_localized_date <- as.Date(restrictions_data$schools_localized_date)
  restrictions_data$restrictions_date <- as.Date(restrictions_data$restrictions_date)
  restrictions_data$quarantine_date <- as.Date(restrictions_data$quarantine_date)
  
  main_data <- merge(main_data, restrictions_data[,c("country","population","tests","testpop","density",
                                                     "median_age","urbanpop","smokers", "sex_ratio",  
                                                     "quarantine_date","restrictions_date",
                                                     "schools_national_date","schools_localized_date",
                                                     "hospital_bed","lung_disease")], by = c("country"), all.x = TRUE)
  
  main_data <- main_data %>%
    group_by(country) %>%
    mutate(intervention_school_national = ifelse(date < schools_national_date,0,1)) %>%
    mutate(intervention_school_local = ifelse(date < schools_localized_date,0,1)) %>%
    mutate(intervention_restriction = ifelse(date < restrictions_date,0,1)) %>%
    mutate(intervention_quarantine = ifelse(date < quarantine_date,0,1)) %>%
    select(-c(schools_national_date,schools_localized_date,restrictions_date,quarantine_date))
  
  data <- main_data
  
  #TODO: add in features
  data[, region:=paste(country, state)]
  data[, first_case_date:=min(date[cases>0]), by=list(region)]
  data[, case_days:=as.numeric(difftime(date, first_case_date,unit="days"))]
  data[, log_case_days:=ifelse(case_days>0, log10(case_days), 0)]
  data[, tenth_case_date:=min(date[cases>10]), by=list(region)]
  data[, case10_days:=as.numeric(difftime(date, tenth_case_date,unit="days")), by=list(region)]
  data[, hundreth_case_date:=min(date[cases>100]), by=list(region)]
  data[, case100_days:=as.numeric(difftime(date, hundreth_case_date,unit="days")), by=list(region)]
  data[, max_cases:=max(cases), by=list(region)]
  
  return(data)
}

generate_task <- function(){
  covariates <- c("case_days","log_case_days")
  
  #TODO: carefully consider fold structure
  folds <- make_folds(n=NULL, t=NULL, id=NULL, time=NULL, 
                      first_window = NULL, validation_size=30,
                      gap = 0, batch=1, 
                      timefold_fun = folds_rolling_origin_pooled)
  # (time series structures, but also consider multiple regions)
  task <- make_sl3_Task(main_data, outcome="cases", covariates = covariates, id="region")
  return(task)
}


loss_competition <- function(pred, observed){

  loss <- (log(pred+1)-log(observed + 1))^2

  return(loss)
}

metalearner_linear_bound <- function(alpha, X){
  return( pmax(metalearner_linear(alpha,X),0))
}

#' @import sl3
generate_learners <- function(){
  #TODO integrate timeseries learners + mechanistic models
  
  lrnr_glm <- make_learner(Lrnr_glm)
  lrnr_rf <- make_learner(Lrnr_randomForest)
  
  metalearner_competition <- make_learner(Lrnr_solnp, metalearner_linear_bound, loss_competition)
  
  sl <- make_learner(Lrnr_sl, list(lrnr_glm, lrnr_rf), metalearner_competition)
  
  return(sl)
}

fit_learners <- function(){
  fit <- sl$train(task)
  cv_preds <- fit$predict_fold(task, "validation")
  rmse <- sqrt(mean(loss_competition(cv_preds, task$Y)))
}