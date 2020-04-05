#' @import data.table
#' @import tidyverse
#' @import origami

covariate_list <- function() {
  # TODO: fix continent and add back in
  c(
    "delta_rail_millionpassengerkm",
    "days", "case_days",
    "case10_days", "case100_days", "recoveries",
    "days_quarantine", "quarantine", "days_restrictions", "restrictions",
    "days_schools_national", "schools_national", "days_schools_localized",
    "schools_localized", "lat", "lon", "population", "area", "sars_cases",
    "sars_deaths", "sars_recovered", "delta_recoveries",
    "tests", "density", "median_age", "urbanpop", "hospital_bed",
    "smokers", "sex0", "sex14", "sex25", "sex54", "sex64", "sex65plus",
    "sex_ratio", "lung_disease", "femalelung", "malelung", "obese",
    "male_obese", "fem_obese", "overweight", "male_overweight", "fem_overweight",
    "air_passengers", "econ_worldrank", "econ_regionrank",
    "econ_2019score", "econ_propertyrights", "econ_judical", "econ_gov_integrity",
    "econ_taxburden", "econ_govspending", "econ_fiscalhealth", "econ_businessfreedom",
    "econ_laborfreedom", "econ_monetaryfreedom", "econ_tradefreedom",
    "econ_investmentfreedom", "econ_financialfreedom", "econ_tariffrate",
    "econ_incometaxrate", "econ_corporatetaxrate", "econ_taxburdengdp",
    "econ_govexpendgdp", "econ_popmills", "econ_gdpbills", "econ_gdpgrowthrate",
    "econ_yeargdpgrowthrate", "econ_gdppercapita", "econ_unemployment",
    "econ_inflation", "econ_fdiflowmills", "econ_publicdeptofgdp",
    "gdp_2018", "crime_index", "population2020", "smoking2016", "females2018",
    "hospbeds_per1k", "docs_per1k",
    "pollution_2017",
    "pop65above_percent",
    "prison_count", "prison_rate",
    "rail_millionpassengerkm", "delta_tests",
    "delta_density", "delta_median_age", "delta_urbanpop", "delta_hospital_bed",
    "delta_smokers", "delta_sex0", "delta_sex14", "delta_sex25",
    "delta_sex54", "delta_sex64", "delta_sex65plus", "delta_sex_ratio",
    "delta_lung_disease", "delta_femalelung", "delta_malelung", "delta_obese",
    "delta_male_obese", "delta_fem_obese", "delta_overweight", "delta_male_overweight",
    "delta_fem_overweight", "delta_air_year", "delta_air_passengers",
    "delta_econ_worldrank", "delta_econ_regionrank", "delta_econ_2019score",
    "delta_econ_propertyrights", "delta_econ_judical", "delta_econ_gov_integrity",
    "delta_econ_taxburden", "delta_econ_govspending", "delta_econ_fiscalhealth",
    "delta_econ_businessfreedom", "delta_econ_laborfreedom", "delta_econ_monetaryfreedom",
    "delta_econ_tradefreedom", "delta_econ_investmentfreedom", "delta_econ_financialfreedom",
    "delta_econ_tariffrate", "delta_econ_incometaxrate", "delta_econ_corporatetaxrate",
    "delta_econ_taxburdengdp", "delta_econ_govexpendgdp", "delta_econ_popmills",
    "delta_econ_gdpbills", "delta_econ_gdpgrowthrate", "delta_econ_yeargdpgrowthrate",
    "delta_econ_gdppercapita", "delta_econ_unemployment", "delta_econ_inflation",
    "delta_econ_fdiflowmills", "delta_econ_publicdeptofgdp", "delta_gdp_2018",
    "delta_crime_index", "delta_population2020", "delta_smoking2016",
    "delta_females2018", "delta_hospbeds_year", "delta_hospbeds_per1k",
    "delta_docs_year", "delta_docs_per1k", "delta_pollution_2010",
    "delta_pollution_2011", "delta_pollution_2012", "delta_pollution_2013",
    "delta_pollution_2014", "delta_pollution_2015", "delta_pollution_2016",
    "delta_pollution_2017", "delta_pop65above_year", "delta_pop65above_percent",
    "delta_prisoncount_year", "delta_prison_count", "delta_prisonrate_year",
    "delta_prison_rate", "delta_rail_year"
  )
}
generate_task <- function(data, outcome) {

  # covariates <- colnames(data)[-which(names(data) %in% c("cases","region"))]
  folds <- origami::make_folds(data,
    t = max(data$days), id = data$region,
    time = data$days,
    fold_fun = folds_rolling_origin_pooled,
    first_window = 20,
    validation_size = 30,
    gap = 0,
    batch = 1
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
generate_learners <- function() {
  # TODO integrate timeseries learners + mechanistic models
  grid_params <- list(
    max_depth = c(2, 5, 8),
    eta = c(0.005, 0.1, 0.25)
  )
  grid <- expand.grid(grid_params, KEEP.OUT.ATTRS = FALSE)
  params_default <- list(nthread = getOption("sl.cores.learners", 1))
  xgb_learners <- apply(grid, MARGIN = 1, function(params_tune) {
    do.call(Lrnr_xgboost$new, c(params_default, as.list(params_tune)))
  })
  lrnr_lasso <- make_learner(Lrnr_glmnet, alpha = 1)
  lrnr_glm <- make_learner(Lrnr_glm)
  lrnr_ranger <- make_learner(Lrnr_ranger)
  lrnr_earth <- make_learner(Lrnr_earth)
  stack <- make_learner(Stack, unlist(list(
    xgb_learners, lrnr_glm, lrnr_lasso,
    lrnr_ranger, lrnr_earth
  ),
  recursive = TRUE
  ))
  lrnr_glmnet <- make_learner(Lrnr_glmnet)
  screener <- make_learner(sl3:::Lrnr_screener_coefs, lrnr_glmnet, 1e-2)
  pipe <- make_learner(Pipeline, screener, stack)
  metalearner_competition <- make_learner(
    Lrnr_solnp, metalearner_linear_bound,
    loss_squared_error
  )
  sl <- make_learner(Lrnr_sl, pipe, metalearner_competition)
  return(sl)
}
