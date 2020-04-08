#' @import data.table
#' @import tidyverse
#' @import origami

covariate_list <- function() {
  c("recoveries", "days_quarantine", "quarantine", "days_restrictions", 
    "restrictions", "days_schools_national", "schools_national", 
    "days_schools_localized", "schools_localized", "lat", "lon", "continent", 
    "population", "area", "sars_cases", "sars_deaths", "sars_recovered", 
    "delta_recoveries", "subregion", "tests", "density", "median_age", 
    "urbanpop", "hospital_bed", "smokers", "sex0", "sex14", "sex25", "sex54", 
    "sex64", "sex65plus", "sex_ratio", "lung_disease", "femalelung", "malelung", 
    "obese", "male_obese", "fem_obese", "overweight", "male_overweight", 
    "fem_overweight", "air_year", "air_passengers", "econ_worldrank", 
    "econ_regionrank", "econ_2019score", "econ_propertyrights", "econ_judical", 
    "econ_gov_integrity", "econ_taxburden", "econ_govspending", 
    "econ_fiscalhealth", "econ_businessfreedom", "econ_laborfreedom", 
    "econ_monetaryfreedom", "econ_tradefreedom", "econ_investmentfreedom", 
    "econ_financialfreedom", "econ_tariffrate", "econ_incometaxrate", 
    "econ_corporatetaxrate", "econ_taxburdengdp", "econ_govexpendgdp", 
    "econ_popmills", "econ_gdpbills", "econ_gdpgrowthrate", 
    "econ_yeargdpgrowthrate", "econ_gdppercapita", "econ_unemployment", 
    "econ_inflation", "econ_fdiflowmills", "econ_publicdeptofgdp", "gdp_2018", 
    "crime_index", "population2020", "smoking2016", "females2018", 
    "hospbeds_year", "hospbeds_per1k", "docs_year", "docs_per1k", 
    "pollution_2010", "pollution_2011", "pollution_2012", "pollution_2013", 
    "pollution_2014", "pollution_2015", "pollution_2016", "pollution_2017", 
    "pop65above_year", "pop65above_percent", "prisoncount_year", "prison_count", 
    "prisonrate_year", "prison_rate", "rail_year", "rail_millionpassengerkm", 
    "delta_tests", "delta_density", "delta_median_age", "delta_urbanpop", 
    "delta_hospital_bed", "delta_smokers", "delta_sex0", "delta_sex14", 
    "delta_sex25", "delta_sex54", "delta_sex64", "delta_sex65plus", 
    "delta_sex_ratio", "delta_lung_disease", "delta_femalelung", 
    "delta_malelung", "delta_obese", "delta_male_obese", "delta_fem_obese", 
    "delta_overweight", "delta_male_overweight", "delta_fem_overweight", 
    "delta_air_year", "delta_air_passengers", "delta_econ_worldrank", 
    "delta_econ_regionrank", "delta_econ_2019score", 
    "delta_econ_propertyrights", "delta_econ_judical", 
    "delta_econ_gov_integrity", "delta_econ_taxburden", 
    "delta_econ_govspending", "delta_econ_fiscalhealth", 
    "delta_econ_businessfreedom", "delta_econ_laborfreedom", 
    "delta_econ_monetaryfreedom", "delta_econ_tradefreedom", 
    "delta_econ_investmentfreedom", "delta_econ_financialfreedom", 
    "delta_econ_tariffrate", "delta_econ_incometaxrate", 
    "delta_econ_corporatetaxrate", "delta_econ_taxburdengdp", 
    "delta_econ_govexpendgdp", "delta_econ_popmills", "delta_econ_gdpbills", 
    "delta_econ_gdpgrowthrate", "delta_econ_yeargdpgrowthrate", 
    "delta_econ_gdppercapita", "delta_econ_unemployment", 
    "delta_econ_inflation", "delta_econ_fdiflowmills", 
    "delta_econ_publicdeptofgdp", "delta_gdp_2018", "delta_crime_index", 
    "delta_population2020", "delta_smoking2016", "delta_females2018", 
    "delta_hospbeds_year", "delta_hospbeds_per1k", "delta_docs_year", 
    "delta_docs_per1k", "delta_pollution_2010", "delta_pollution_2011", 
    "delta_pollution_2012", "delta_pollution_2013", "delta_pollution_2014", 
    "delta_pollution_2015", "delta_pollution_2016", "delta_pollution_2017", 
    "delta_pop65above_year", "delta_pop65above_percent", 
    "delta_prisoncount_year", "delta_prison_count", "delta_prisonrate_year", 
    "delta_prison_rate", "delta_rail_year", "delta_rail_millionpassengerkm",
    "days", "case_days", "case10_days", "case100_days", "max_cases", 
    "log_recoveries", "log_population", "log_area", "log_sars_cases", 
    "log_sars_deaths", "log_sars_recovered", "log_tests", "log_density", 
    "log_median_age", "log_urbanpop", "log_hospital_bed", "log_smokers", 
    "log_sex0", "log_sex14", "log_sex25", "log_sex54", "log_sex64", 
    "log_sex65plus", "log_sex_ratio", "log_lung_disease", "log_femalelung", 
    "log_malelung", "log_obese", "log_male_obese", "log_fem_obese", 
    "log_overweight", "log_male_overweight", "log_fem_overweight", 
    "log_air_passengers", "log_econ_2019score", "log_econ_propertyrights", 
    "log_econ_judical", "log_econ_gov_integrity", "log_econ_taxburden", 
    "log_econ_govspending", "log_econ_fiscalhealth", "log_econ_businessfreedom", 
    "log_econ_laborfreedom", "log_econ_monetaryfreedom", 
    "log_econ_tradefreedom", "log_econ_investmentfreedom", 
    "log_econ_financialfreedom", "log_econ_tariffrate", 
    "log_econ_incometaxrate", "log_econ_corporatetaxrate", 
    "log_econ_taxburdengdp", "log_econ_govexpendgdp", "log_econ_popmills", 
    "log_econ_gdpbills", "log_econ_gdppercapita", "log_econ_unemployment", 
    "log_econ_publicdeptofgdp", "log_gdp_2018", "log_crime_index", 
    "log_population2020", "log_smoking2016", "log_females2018", 
    "log_hospbeds_per1k", "log_docs_per1k", "log_pollution_2010", 
    "log_pollution_2011", "log_pollution_2012", "log_pollution_2013",
    "log_pollution_2014", "log_pollution_2015", "log_pollution_2016", 
    "log_pollution_2017", "log_pop65above_percent", "log_prison_count", 
    "log_prison_rate", "log_rail_millionpassengerkm", "log_max_cases", 
    "weekday")
}

generate_task <- function(data, outcome, first_window = 20, time = data$days,
                          t = max(data$days), batch = 1) {

  folds <- origami::make_folds(data,
    t = t, 
    id = data$region,
    time = time,
    fold_fun = folds_rolling_origin_pooled,
    first_window = first_window,
    validation_size = 30,
    gap = 0,
    batch = batch
  )

  # TODO: consider imputation of covariates, drop_missing_outcome
  nodes <- list(
    outcome = outcome,
    covariates = covariate_list(),
    time = "days",
    id = "region"
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
      max_depth = c(2, 4, 6, 8),
      eta = c(0.001, 0.01, 0.1, 0.2, 0.3)
    )
    grid <- expand.grid(grid_params, KEEP.OUT.ATTRS = FALSE)
    params_default <- list(nthread = getOption("sl.cores.learners", 1))
    xgb_learners <- apply(grid, MARGIN = 1, function(params_tune) {
      do.call(Lrnr_xgboost$new, c(params_default, as.list(params_tune)))
    })
    lrnr_lasso <- make_learner(Lrnr_glmnet)
    lrnr_glm <- make_learner(Lrnr_glm)
    lrnr_ranger <- make_learner(Lrnr_ranger)
    lrnr_earth <- make_learner(Lrnr_earth)
    
    # time series learners
    lrnr_gts <- make_learner(Lrnr_gts)
    lrnr_arima <- make_learner(Lrnr_arima)
    lrnr_expSmooth <- make_learner(Lrnr_expSmooth)
    lrnr_lstm <- make_learner(Lrnr_lstm, epochs = 500)
    # do one by one for now
    lrnr_arima_strat <- Lrnr_multiple_ts$new(learner = lrnr_arima)
    lrnr_expSmooth_strat <- Lrnr_multiple_ts$new(learner = lrnr_expSmooth)
    lrnr_lstm_strat <- Lrnr_multiple_ts$new(learner = lrnr_lstm)
    
    ### stack of base learners
    stack <- make_learner(
      Stack, 
      unlist(list(xgb_learners, lrnr_glm, lrnr_lasso, lrnr_ranger, lrnr_earth,
                  lrnr_gts, lrnr_arima_strat, lrnr_lstm_strat, 
                  lrnr_expSmooth_strat), recursive = TRUE)
      )
    
    ### screeners
    screener_lasso <- make_learner(Lrnr_screener_coefs, lrnr_lasso, 
                                   threshold = 1e-1)
    screener_lasso_flex <- make_learner(Lrnr_screener_coefs, lrnr_lasso, 
                                        threshold = 1e-3)
    screener_rf <- make_learner(Lrnr_screener_randomForest, ntree = 500, 
                                nVar = 15)
    # pipelines
    screen_lasso_pipe <- make_learner(Pipeline, screener_lasso, stack)
    screen_lasso_flex_pipe <- make_learner(Pipeline, screener_lasso_flex, stack)
    screen_rf_pipe <- make_learner(Pipeline, screener_rf, stack)
    
    ### final stack
    stack <- make_learner(Stack, screen_lasso_pipe, screen_lasso_flex_pipe,
                          screen_rf_pipe)
  }
  
  ### metalearner
  metalearner_competition <- make_learner(
    Lrnr_solnp, metalearner_linear_bound,loss_squared_error
  )
  if(metalearner_stratified){
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
