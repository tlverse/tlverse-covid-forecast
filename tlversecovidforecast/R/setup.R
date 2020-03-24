#' @import data.table
generate_analytic_data <- function(){
  main_data <- fread("Data/covid19-global-forecasting-week-1/train.csv")
  old_names <- c("Id", "Province/State", "Country/Region", "Lat", "Long", "Date", 
                 "ConfirmedCases", "Fatalities")
  new_names <- c("id","state", "country", "lat", "long", "date", "cases", "deaths")
  
  setnames(main_data, old_names, new_names)
  #TODO: merge in helper datasets
  
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
  
  return(data)
}

generate_task <- function(){
  covariates <- c("case_days","log_case_days")
  
  #TODO: carefully consider fold structure
  task <- make_sl3_Task(main_data, outcome="cases", covariates = covariates, id="region")
  return(task)
}


loss_competition <- function(pred, observed){
  # TODO: put bound in learner, not loss
  pred <- pmax(pred, 0)
  loss <- (log(pred+1)-log(observed + 1))^2
  return(loss)
}

#' @import sl3
generate_learners <- function(){
  lrnr_glm <- make_learner(Lrnr_glm)
  lrnr_rf <- make_learner(Lrnr_randomForest)
  
  metalearner_competition <- make_learner(Lrnr_solnp, metalearner_linear, loss_competition)
  
  sl <- make_learner(Lrnr_sl, list(lrnr_glm, lrnr_rf), metalearner_competition)
  
  return(sl)
}

fit_learners <- function(){
  fit <- sl$train(task)
  cv_preds <- fit$predict_fold(task, "validation")
  rmse <- sqrt(mean(loss_competition(cv_preds, task$Y)))
}