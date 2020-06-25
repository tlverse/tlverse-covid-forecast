run_analysis <- function(outcome,processed){
  sl <- generate_learners(metalearner_stratified = FALSE)
  # sl_trans <- make_learner(Lrnr_transform_outcome,trans, inv_trans, sl)
  
  data <- processed$data[processed$training_indexes]
  test_data <- processed$data[processed$test_indexes]
  
  #TODO: should state be a covariate?
  covariates=c("State",processed$tv_covariates, processed$baseline_covariates)
  
  # generate case task and training predictions
  batch_size <- processed$lag
  task <- generate_task(data, outcome, covariates = covariates, 
                        ids = processed$ids, batch = batch_size, val_size = batch_size)
  
  sl_fit <- sl$train(task)
  
  # structure data for cv predictions
  get_obsdata <- function(fold, task) {
    list(cv_dt = data.table(
      fold_index = fold_index(),
      index = validation(),
      obs = validation(task$Y),
      id = validation(task$id),
      weights = validation(task$weights)
    ))
  }
  
  data[,index:=.I]
  data$obs <- data[,outcome,with=FALSE]
  
  cv_dt <- origami::cross_validate(get_obsdata, task$folds, task)$cv_dt
  
  # get cv predictions
  cv_dt <- cv_dt[order(index, fold_index)]
  cv_dt$pred_sl <- sl_fit$predict_fold(task, "validation")
  cv_library_pred <- sl_fit$fit_object$cv_fit$predict_fold(task, "validation")
  library_pred_cols <- sprintf("pred_%s",colnames(cv_library_pred))
  colnames(cv_library_pred) <- library_pred_cols
  cv_dt <- cbind(cv_dt, cv_library_pred)
  cv_dt <- merge(cv_dt,data[,list(index,State, date = outcome_date)],by="index")
  cv_dt$pred_type <- "cv"
  ids <- c("State", "date","pred_type","obs")
  pred_cols <- c("pred_sl",library_pred_cols)
  all_cols <- c(ids, pred_cols)
  cv_dt <- cv_dt[,all_cols,with = FALSE]
  
  # get full fit predictions
  pred_dt <- data[,list(State,date = outcome_date,obs)]
  pred_dt$pred_sl <- sl_fit$predict()
  library_pred <- sl_fit$fit_object$cv_fit$predict_fold(task,"full")
  colnames(library_pred) <- sprintf("pred_%s",colnames(library_pred))
  pred_dt <- cbind(pred_dt, library_pred)
  pred_dt$pred_type <- "full"
  pred_dt <- pred_dt[,all_cols, with=FALSE]
  
  # get test set predictions
  test_task <- generate_task(test_data, outcome, covariates = covariates, 
                             ids = processed$ids, batch = batch_size, val_size = batch_size)
  
  test_pred_dt <- test_data[,list(State,date = outcome_date,obs=NA)]
  test_pred_dt$pred_sl <- sl_fit$predict(test_task)
  library_pred <- sl_fit$fit_object$cv_fit$predict_fold(test_task,"full")
  colnames(library_pred) <- sprintf("pred_%s",colnames(library_pred))
  test_pred_dt <- cbind(test_pred_dt, library_pred)
  test_pred_dt$pred_type <- "forecast"
  test_pred_dt <- test_pred_dt[,all_cols, with=FALSE]

  all_preds <- rbindlist(list(cv_dt, pred_dt, test_pred_dt))  
  results <- melt(all_preds,id=ids, measure=pred_cols,
                  variable.name = "learner", value.name = "pred")
  
  results[,learner:=gsub("pred_","",learner)]
  results[,outcome:=outcome]
  
  return(results)
}