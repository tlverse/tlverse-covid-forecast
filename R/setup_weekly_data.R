#' @import lubridate
setup_weekly_data <- function(data_path = "data") {
  load(file.path(data_path, "state_data.rdata"))
  data <- processed$data
  data[,week:=floor_date(date, unit="week")]
  
  outcomes <- processed$outcomes
  other <- setdiff(names(data),c(outcomes,"State","week"))
  
  weekly_data <- data[,lapply(.SD,function(x)x[1]),by=list(State, week),.SDcols=other]
  weekly_outcomes <- data[,lapply(.SD,mean,na.rm=T),by=list(State, week),.SDcols=outcomes]
  weekly_data <- cbind(weekly_data, weekly_outcomes[,outcomes,with=FALSE])
  
  # kludge to make days indexing work
  all_days <- unique(weekly_data$days)
  weekly_data[,days:=match(days,all_days)]

  processed$data <- weekly_data
  
  processed$lag <- 1
  one_outcome <- unlist(weekly_data[,outcomes[[1]], with=FALSE])
  processed$training_indexes <- which(!is.nan(one_outcome))
  processed$test_indexes <- which(is.nan(one_outcome))
  
  save(processed, file=file.path(data_path, "weekly_state_data.rdata"))
}