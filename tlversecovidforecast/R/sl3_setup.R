#' @import data.table
#' @import tidyverse
#' @import origami

generate_task <- function(data){
  
  #covariates <- colnames(data)[-which(names(data) %in% c("cases","region"))]
  covariates <- c("hospital_bed", "lung_disease")
  folds <- origami::make_folds(data, t = max(data$days), id = data$region, 
                               time = data$days,
                               fold_fun = folds_rolling_origin_pooled, 
                               first_window = 20, 
                               validation_size = 30, 
                               gap = 0, 
                               batch = 1) 
  
  # TODO: consider imputation of covariates, drop_missing_outcome
  task <- make_sl3_Task(data, outcome = "cases", covariates = covariates, 
                        folds = folds)
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

