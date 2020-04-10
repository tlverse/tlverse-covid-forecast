data <- read_csv(here("Data", "training_processed.csv"))
test_data <- read_csv(here("Data", "test_processed.csv"))

# generate case task and training predictions
log_cases_task <- generate_task(data, "log_cases")
nodes <- log_cases_task$nodes
nodes$covariates <- nodes$covariates[1:10]
log_cases_task <- log_cases_task$next_in_chain(new_nodes = nodes)
lrnr_lasso <- make_learner(Lrnr_glmnet)
lrnr_ranger <- make_learner(Lrnr_ranger)

### stack of base learners
stack <- make_learner(
  Stack, 
  unlist(list(lrnr_lasso, lrnr_ranger), recursive = TRUE)
)

### metalearner
metalearner_competition <- make_learner(
  Lrnr_solnp, metalearner_linear_bound,loss_squared_error
)
stratified_metalearner <- Lrnr_stratified$new(
  learner = metalearner_competition, variable_stratify = "continent"
)
sl <- make_learner(Lrnr_sl, stack, stratified_metalearner)

fit <- sl$train(log_cases_task)
preds <- fit$predict

# check metalearner fits
coefs <- sapply(fit$fit_object$cv_meta_fit$fit_object,coef)
coefs
