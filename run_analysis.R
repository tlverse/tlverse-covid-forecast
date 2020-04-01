library(devtools)
library(here)
document("tlversecovidforecast")
load_all("tlversecovidforecast")
# run this to regenerate processed data
# setup_data()

data <- fread(here("Data/training.csv"))
task <- generate_task(data)
sl <- generate_learners()


fit <- sl$train(task)

# TODO: why doesn't this work
fit$cv_risk(loss_competition)

# manually calculate loss
cv_preds <- fit$predict_fold(task, "validation")
get_val_obs <- function(fold, task){
  list(obs = validation(task$Y))
}

cv_obs <- cross_validate(get_val_obs, task$folds, task)$obs
rmse <- sqrt(mean(loss_competition(cv_preds, cv_obs)))

# compare against null (mean) model
# TODO: why doesn't this work
cv_mean <- make_learner(Lrnr_cv, make_learner(Lrnr_mean))
cv_mean_fit <- cv_mean$train(task)
cv_mean_preds <- cv_mean_fit$predict(task)

# manually do this
cv_mean <- function(fold, task){
  
  list(preds = rep(mean(training(task)$Y),length(validation())))
}

cv_mean_preds <- cross_validate(cv_mean, task$folds, task)$preds
null_rmse <- sqrt(mean(loss_competition(cv_mean_preds, cv_obs)))
