#' @import data.table
#' @import tidyverse
#' @import origami

generate_task <- function(data, outcome, covariates, ids, first_window = 20, time = data$days,
                          t = max(data$days), batch = 1, val_size = 7) {

  id_vec <- unlist(data[,ids,with=FALSE])
  folds <- origami::make_folds(data,
    t = t, 
    id = id_vec,
    time = time,
    fold_fun = folds_rolling_origin_pooled,
    first_window = first_window,
    validation_size = val_size,
    gap = 0,
    batch = batch
  )

  # TODO: consider imputation of covariates, drop_missing_outcome
  nodes <- list(
    outcome = outcome,
    covariates = covariates,
    time = "days",
    id = ids
  )

  task <- make_sl3_Task(data,
    nodes = nodes,
    folds = folds
  )
  return(task)
}

loss_competition <- function(pred, observed) {
  loss <- (log(pred + 1) - log(observed + 1))^2
  return(loss)
}

metalearner_linear_bound <- function(alpha, X) {
  return(pmax(metalearner_linear(alpha, X), 0))
}

#' @import sl3
generate_learners <- function(metalearner_stratified = TRUE, stack = NULL) {
  if(is.null(stack)){
    ### base library
    grid_params <- list(
      max_depth = c(3, 8),
      eta = c(0.05, 0.2),
      nrounds = c(50)
    ) 
    grid <- expand.grid(grid_params, KEEP.OUT.ATTRS = FALSE)
    params_default <- list(nthread = getOption("sl.cores.learners", 1))
    xgb_learners <- apply(grid, MARGIN = 1, function(params_tune) {
      do.call(Lrnr_xgboost$new, c(params_default, as.list(params_tune)))
    })
    
    names(xgb_learners) <- sprintf("xgboost_%d",seq_along(xgb_learners))
    lrnr_mean <- make_learner(Lrnr_mean)
    lrnr_lasso <- Lrnr_glmnet$new(alpha = 1, nfolds = 3)
    lrnr_ridge <- Lrnr_glmnet$new(alpha = 0, nfolds = 3)
    enet_lrnr_reg25 <- Lrnr_glmnet$new(alpha = 0.25, nfolds = 3)
    enet_lrnr_reg50 <- Lrnr_glmnet$new(alpha = 0.50, nfolds = 3)
    enet_lrnr_reg75 <- Lrnr_glmnet$new(alpha = 0.75, nfolds = 3)
    lrnr_glm <- Lrnr_glm_fast$new()
    lrnr_ranger <- make_learner(Lrnr_ranger)

    # time series learners
    lrnr_alan <- make_learner(Lrnr_alan_pois)
    lrnr_gts <- make_learner(Lrnr_gts)
    lrnr_arima <- make_learner(Lrnr_arima)
    lrnr_expSmooth <- make_learner(Lrnr_expSmooth)
    lrnr_lstm10 <- make_learner(Lrnr_lstm, epochs = 500, batch_size = 10,
                                early_stop = TRUE)
    lrnr_lstm1 <- make_learner(Lrnr_lstm, epochs = 500, batch_size = 1,
                               early_stop = TRUE)
    lrnr_expSmooth_alt_aic <- make_learner(Lrnr_expSmooth, nmse = 14, 
                                           opt.crit = "mse", ic = "aic", 
                                           allow.multiplicative.trend = TRUE)
    lrnr_expSmooth_alt_bic <- make_learner(Lrnr_expSmooth, nmse = 14, 
                                           opt.crit = "mse", ic = "bic", 
                                           allow.multiplicative.trend = TRUE)
    # do one by one for now
    lrnr_arima_strat <- Lrnr_multiple_ts$new(learner = lrnr_arima)
    lrnr_expSmooth_strat <- Lrnr_multiple_ts$new(learner = lrnr_expSmooth)
    lrnr_expSmooth_alt_aic_strat <- Lrnr_multiple_ts$new(
      learner = lrnr_expSmooth_alt_aic
      )
    lrnr_expSmooth_alt_bic_strat <- Lrnr_multiple_ts$new(
      learner = lrnr_expSmooth_alt_bic
    )

    
    exp_trans <- function(x){exp(x)-1}
    exp_trans_inv <- function(x){log(x+1)}
    lin_expsmooth_strat_lrnr <- make_learner(Lrnr_transform_outcome, exp_trans, exp_trans_inv, lrnr_expSmooth_strat)
    lin_arima_strat_lrnr <- make_learner(Lrnr_transform_outcome, exp_trans, exp_trans_inv, lrnr_arima_strat)
    #
    lrnr_lstm10_strat <- Lrnr_multiple_ts$new(learner = lrnr_lstm10)
    lrnr_lstm1_strat <- Lrnr_multiple_ts$new(learner = lrnr_lstm1)
    ### stack of base learners
    stack <- make_learner(
      Stack, 
      unlist(list(mean=lrnr_mean,
                  xgb_learners,
                  glm=lrnr_glm,
                  lasso=lrnr_lasso,
                  ridge=lrnr_ridge,
                  # enet_lrnr_reg25,
                  # enet_lrnr_reg50,
                  # enet_lrnr_reg75,
                  ranger=lrnr_ranger,
                  # gts=lrnr_gts,
                  alan_poisson=lrnr_alan
                  # arima=lrnr_arima_strat,
                  #lrnr_lstm10_strat,
                  #lrnr_lstm1_strat,
                  # lrnr_expSmooth_alt_aic_strat,
                  # lrnr_expSmooth_alt_bic_strat,
                  # expSmooth=lrnr_expSmooth_strat
                  # expSmooth_linear=lin_expsmooth_strat_lrnr,
                  # arima_linear=lin_arima_strat_lrnr
                  ),
             recursive = TRUE)
      )

    ### screeners
    screener_lasso <- make_learner(Lrnr_screener_coefs, lrnr_lasso,
                                   threshold = 1e-3)
    #screener_lasso_flex <- make_learner(Lrnr_screener_coefs, lrnr_lasso,
                                        #threshold = 1e-3)
    # pipelines
    screen_lasso_pipe <- make_learner(Pipeline, screener_lasso, stack)
    #screen_lasso_flex_pipe <- make_learner(Pipeline, screener_lasso_flex,
                                           #stack)

    ### final stack
    stack <- make_learner(Stack, screen_lasso_pipe)
  }

  ### metalearner
  metalearner_competition <- make_learner(
    Lrnr_solnp, metalearner_linear_bound, loss_squared_error
  )
  if (metalearner_stratified) {
    stratified_metalearner <- Lrnr_stratified$new(
      learner = metalearner_competition, variable_stratify = "continent"
    )
    ### super learner
    sl <- make_learner(Lrnr_sl, stack, stratified_metalearner)
  } else {
    sl <- make_learner(Lrnr_sl, stack, metalearner_competition)
  }
  return(sl)
}

strat_metalearner_coefs <- function(fit){
  # check metalearner fits
  coefs <- sapply(fit$fit_object$cv_meta_fit$fit_object,coef)
  mean_coefs <- unlist(rowMeans(coefs))
  return(mean_coefs)  
}