#' @import data.table
#' @import tidyverse
#' @import origami

covariate_list <- function(include_tv) {
  covariates <- c(
    "StateName", "POP_LATITUDE", "POP_LONGITUDE", "Rural-UrbanContinuumCode2013", 
    "PopulationEstimate2018", "PopTotalMale2017", "PopTotalFemale2017", 
    "FracMale2017", "PopulationEstimate65+2017", "PopulationDensityperSqMile2010", 
    "CensusPopulation2010", "MedianAge2010", "#EligibleforMedicare2018", 
    "MedicareEnrollment,AgedTot2017", "DiabetesPercentage", "HeartDiseaseMortality", 
    "StrokeMortality", "Smokers_Percentage", "RespMortalityRate2014", 
    "#FTEHospitalTotal2017", "TotalM.D.'s,TotNon-FedandFed2017", 
    "#HospParticipatinginNetwork2017", "#Hospitals", "#ICU_beds", 
    "dem_to_rep_ratio", "PopMale<52010", "PopFmle<52010", "PopMale5-92010", 
    "PopFmle5-92010", "PopMale10-142010", "PopFmle10-142010", "PopMale15-192010", 
    "PopFmle15-192010", "PopMale20-242010", "PopFmle20-242010", "PopMale25-292010", 
    "PopFmle25-292010", "PopMale30-342010", "PopFmle30-342010", "PopMale35-442010", 
    "PopFmle35-442010", "PopMale45-542010", "PopFmle45-542010", "PopMale55-592010", 
    "PopFmle55-592010", "PopMale60-642010", "PopFmle60-642010", "PopMale65-742010", 
    "PopFmle65-742010", "PopMale75-842010", "PopFmle75-842010", "PopMale>842010", 
    "PopFmle>842010", "3-YrMortalityAge45-54Years2015-17", "3-YrMortalityAge55-64Years2015-17", 
    "3-YrMortalityAge65-74Years2015-17", "3-YrMortalityAge75-84Years2015-17", 
    "3-YrMortalityAge85+Years2015-17", "SVIPercentile", "delta_lat", 
    "delta_lon", "delta_Rural-UrbanContinuumCode2013", "delta_PopulationEstimate2018", 
    "delta_PopTotalMale2017", "delta_PopTotalFemale2017", "delta_FracMale2017", 
    "delta_PopulationEstimate65+2017", "delta_PopulationDensityperSqMile2010", 
    "delta_CensusPopulation2010", "delta_MedianAge2010", "delta_#EligibleforMedicare2018", 
    "delta_MedicareEnrollment,AgedTot2017", "delta_DiabetesPercentage", 
    "delta_HeartDiseaseMortality", "delta_StrokeMortality", "delta_Smokers_Percentage", 
    "delta_RespMortalityRate2014", "delta_#FTEHospitalTotal2017", 
    "delta_TotalM.D.'s,TotNon-FedandFed2017", "delta_#HospParticipatinginNetwork2017", 
    "delta_#Hospitals", "delta_#ICU_beds", "delta_dem_to_rep_ratio", 
    "delta_PopMale<52010", "delta_PopFmle<52010", "delta_PopMale5-92010", 
    "delta_PopFmle5-92010", "delta_PopMale10-142010", "delta_PopFmle10-142010", 
    "delta_PopMale15-192010", "delta_PopFmle15-192010", "delta_PopMale20-242010", 
    "delta_PopFmle20-242010", "delta_PopMale25-292010", "delta_PopFmle25-292010", 
    "delta_PopMale30-342010", "delta_PopFmle30-342010", "delta_PopMale35-442010", 
    "delta_PopFmle35-442010", "delta_PopMale45-542010", "delta_PopFmle45-542010", 
    "delta_PopMale55-592010", "delta_PopFmle55-592010", "delta_PopMale60-642010", 
    "delta_PopFmle60-642010", "delta_PopMale65-742010", "delta_PopFmle65-742010", 
    "delta_PopMale75-842010", "delta_PopFmle75-842010", "delta_PopMale>842010", 
    "delta_PopFmle>842010", "delta_3-YrMortalityAge45-54Years2015-17", 
    "delta_3-YrMortalityAge55-64Years2015-17", "delta_3-YrMortalityAge65-74Years2015-17", 
    "delta_3-YrMortalityAge75-84Years2015-17", "delta_3-YrMortalityAge85+Years2015-17", 
    "delta_SVIPercentile", "delta_HPSAShortage", "delta_HPSAServedPop", 
    "delta_HPSAUnderservedPop", "lat", "lon", "HPSAShortage", "HPSAServedPop", 
    "HPSAUnderservedPop", "delta__lat", "delta__lon", "delta__HPSAShortage", 
    "delta__HPSAServedPop", "delta__HPSAUnderservedPop", 
    "log_Rural-UrbanContinuumCode2013", "log_PopulationEstimate2018", 
    "log_PopTotalMale2017", "log_PopTotalFemale2017", "log_FracMale2017", 
    "log_PopulationEstimate65+2017", "log_PopulationDensityperSqMile2010", 
    "log_CensusPopulation2010", "log_MedianAge2010", "log_#EligibleforMedicare2018", 
    "log_MedicareEnrollment,AgedTot2017", "log_DiabetesPercentage", 
    "log_HeartDiseaseMortality", "log_StrokeMortality", "log_Smokers_Percentage", 
    "log_RespMortalityRate2014", "log_#FTEHospitalTotal2017", "log_TotalM.D.'s,TotNon-FedandFed2017", 
    "log_#HospParticipatinginNetwork2017", "log_#Hospitals", "log_#ICU_beds", 
    "log_dem_to_rep_ratio", "log_PopMale<52010", "log_PopFmle<52010", 
    "log_PopMale5-92010", "log_PopFmle5-92010", "log_PopMale10-142010", 
    "log_PopFmle10-142010", "log_PopMale15-192010", "log_PopFmle15-192010", 
    "log_PopMale20-242010", "log_PopFmle20-242010", "log_PopMale25-292010", 
    "log_PopFmle25-292010", "log_PopMale30-342010", "log_PopFmle30-342010", 
    "log_PopMale35-442010", "log_PopFmle35-442010", "log_PopMale45-542010", 
    "log_PopFmle45-542010", "log_PopMale55-592010", "log_PopFmle55-592010", 
    "log_PopMale60-642010", "log_PopFmle60-642010", "log_PopMale65-742010", 
    "log_PopFmle65-742010", "log_PopMale75-842010", "log_PopFmle75-842010", 
    "log_PopMale>842010", "log_PopFmle>842010", "log_3-YrMortalityAge45-54Years2015-17", 
    "log_3-YrMortalityAge55-64Years2015-17", "log_3-YrMortalityAge65-74Years2015-17", 
    "log_3-YrMortalityAge75-84Years2015-17", "log_3-YrMortalityAge85+Years2015-17", 
    "log_HPSAShortage", "log_HPSAServedPop", "log_HPSAUnderservedPop"
  )
    
  
  tv_covariates <- c(
    "weekday", "deaths_lag14", "cases_lag14", "deaths_lag15", "cases_lag15", 
    "deaths_lag16", "cases_lag16","deaths_lag17total", "cases_lag17total",  
    "restrict_stayhome_lag14", "restrict_gather50_lag14", "restrict_gather500_lag14", 
    "restrict_school_lag14", "restrict_dining_lag14", "restrict_entertain_lag14", 
    "restrict_federal_lag14", "restrict_travel_lag14", "restrict_stayhome_days_lag14", 
    "restrict_gather50_days_lag14", "restrict_gather500_days_lag14", "restrict_school_days_lag14", 
    "restrict_dining_days_lag14", "restrict_entertain_days_lag14", "restrict_federal_days_lag14", 
    "restrict_travel_days_lag14", "log_deaths_lag14", "log_cases_lag14", "log_deaths_lag15", 
    "log_cases_lag15", "log_deaths_lag16", "log_cases_lag16", "log_deaths_lag17total", 
    "log_cases_lag17total", "case_days_lag14", "case10_days_lag14", "case100_days_lag14", 
    "death_days_lag14", "death10_days_lag14", "death100_days_lag14")
  
  if(include_tv){
    covariates <- c(covariates, tv_covariates)
  }
  
  return(covariates)
}

generate_task <- function(data, outcome, first_window = 20, time = data$days,
                          t = max(data$days), batch = 1, include_tv=TRUE) {

  folds <- origami::make_folds(data,
    t = t, 
    id = data$countyFIPS,
    time = time,
    fold_fun = folds_rolling_origin_pooled,
    first_window = first_window,
    validation_size = 14,
    gap = 0,
    batch = batch
  )

  # TODO: consider imputation of covariates, drop_missing_outcome
  nodes <- list(
    outcome = outcome,
    covariates = covariate_list(include_tv),
    strata_vars = c("continent"),
    time = "days",
    id = "region",
    population_size = "population"
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
                  gts=lrnr_gts,
                  alan_poisson=lrnr_alan,
                  arima=lrnr_arima_strat,
                  #lrnr_lstm10_strat,
                  #lrnr_lstm1_strat,
                  # lrnr_expSmooth_alt_aic_strat,
                  # lrnr_expSmooth_alt_bic_strat,
                  expSmooth=lrnr_expSmooth_strat,
                  expSmooth_linear=lin_expsmooth_strat_lrnr,
                  arima_linear=lin_arima_strat_lrnr),
             recursive = TRUE)
      )

    ### screeners
    screener_lasso <- make_learner(Lrnr_screener_coefs, lrnr_lasso,
                                   threshold = 1e-2)
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