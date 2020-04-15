library(here)
library(tidyverse)
library(data.table)
remotes::install_github("tlverse/origami@devel")
# remotes::install_github("tlverse/sl3@timeseries-overhaul")
library(future)
library(sl3)
library(knitr)
library(kableExtra)
source(here("R", "utils.R"))

sl3_debug_mode()
options(sl3.verbose=TRUE)

# load helper package
devtools::document(here("tlversecovidforecast"))
devtools::load_all(here("tlversecovidforecast"))

# get/set miniconda python for kerasR
reticulate::use_python(system("which python3"))

# data.table threading vs. future (probably unnecessary)
# dt_cores <- as.integer(unname(round(0.2 * availableCores())))
# future_cores <- as.integer(unname(availableCores()) - dt_cores)
# setDTthreads(dt_cores)
# plan(multiprocess, workers = future_cores)

# load helper package
devtools::document(here("tlversecovidforecast"))
devtools::load_all(here("tlversecovidforecast"))

data <- read_csv(here("Data", "training_processed.csv"))
test_data <- read_csv(here("Data", "test_processed.csv"))
setDT(data)
setDT(test_data)

# generate case task and subset to Italy data
log_cases_task <- generate_task(data, "log_cases")
italy_task <- log_cases_task[data$region == "Italy"]
italy_data <- data[data$region == "Italy", ]

# base library
grid_params <- list(
  max_depth = c(2, 5, 8),
  eta = c(0.001, 0.05, 0.2),
  nrounds = c(50, 100)
) %>% expand.grid(KEEP.OUT.ATTRS = FALSE)
params_default <- list(nthread = getOption("sl.cores.learners", 1))
xgb_lrnrs <- apply(grid_params, MARGIN = 1, function(params_tune) {
  do.call(Lrnr_xgboost$new, c(params_default, as.list(params_tune)))
})

xgb_lrnrs <- xgb_lrnrs[14:18]

mean_lrnr <- Lrnr_mean$new()
glm_lrnr <- Lrnr_glm_fast$new()
ridge_lrnr <- Lrnr_glmnet$new(alpha = 0, nfolds = 3)
lasso_lrnr <- Lrnr_glmnet$new(alpha = 1, nfolds = 3)
enet_lrnr_reg25 <- Lrnr_glmnet$new(alpha = 0.25, nfolds = 3)
enet_lrnr_reg50 <- Lrnr_glmnet$new(alpha = 0.50, nfolds = 3)
enet_lrnr_reg75 <- Lrnr_glmnet$new(alpha = 0.75, nfolds = 3)
ranger_lrnr_nt50 <- Lrnr_ranger$new(num.trees = 50)
ranger_lrnr_nt100 <- Lrnr_ranger$new(num.trees = 100)
ranger_lrnr_nt500 <- Lrnr_ranger$new(num.trees = 500)
earth_lrnr <- make_learner(Lrnr_earth)

# time series learners
alan_lrnr <- make_learner(Lrnr_alan_pois)
gts_lrnr <- make_learner(Lrnr_gts)

# TODO identify rank deficient covariates
arima_lrnr <- make_learner(Lrnr_arima, covariates=c("case_days"))
expsmooth_lrnr <- make_learner(Lrnr_expSmooth)
arima_strat_lrnr <- Lrnr_multiple_ts$new(learner = arima_lrnr)
expsmooth_strat_lrnr <- Lrnr_multiple_ts$new(learner = expsmooth_lrnr)


exp_trans <- function(x){exp(x)-1}
exp_trans_inv <- function(x){log(x+1)}
lin_expsmooth_strat_lrnr <- make_learner(Lrnr_transform_outcome, exp_trans, exp_trans_inv, expsmooth_strat_lrnr)
lin_arima_strat_lrnr <- make_learner(Lrnr_transform_outcome, exp_trans, exp_trans_inv, arima_strat_lrnr)
# 
# lrnr_lstm10 <- make_learner(Lrnr_lstm, epochs = 500, batch_size = 10, 
#                             early_stop = TRUE)
# lrnr_lstm1 <- make_learner(Lrnr_lstm, epochs = 500, batch_size = 1, 
#                            early_stop = TRUE)
# 
# lrnr_lstm10_strat <- Lrnr_multiple_ts$new(learner = lrnr_lstm10)
# lrnr_lstm1_strat <- Lrnr_multiple_ts$new(learner = lrnr_lstm1)

# library for Stack
stack_lib <- unlist(list(mean_lrnr, glm_lrnr, xgb_lrnrs, alan_lrnr, 
                         expsmooth_strat_lrnr, lin_expsmooth_strat_lrnr,
                         arima_strat_lrnr, lin_arima_strat_lrnr),
                    recursive = TRUE)

long_lrnr_names <- lapply(stack_lib,`[[`,"name")

short_lrnr_names <- c("mean","glm",
                      "xgboost 1","xgboost 2", 
                      "xgboost 3", "xgboost 4",
                      "xgboost 5", "poisson regression",
                      "exp smoothing","exp smoothing (linear)",
                      "arima", "arima (linear)")
# make stack for SL
stack_lrnrs <- make_learner(Stack, stack_lib)

# LASSO screener
#screener_lasso <- make_learner(Lrnr_screener_coefs, lrnr_lasso,
                               #threshold = 1e-1)
#screener_lasso_flex <- make_learner(Lrnr_screener_coefs, lrnr_lasso,
                                    #threshold = 1e-3)
screener_lasso <- make_learner(Lrnr_screener_coefs, lasso_lrnr,
                               threshold = 1e-4)

# pipelines
#screen_lasso_pipe <- make_learner(Pipeline, screener_lasso, stack)
#screen_lasso_flex_pipe <- make_learner(Pipeline, screener_lasso_flex, stack)
screen_lasso_pipe <- make_learner(Pipeline, screener_lasso, stack_lrnrs)

# final stack for SL
#stack_screeners <- make_learner(Stack, screen_lasso_pipe,
                                #screen_lasso_flex_pipe)
sl_stack_run_forrest_run <- make_learner(Stack, screen_lasso_pipe)

cv_ss <- make_learner(Lrnr_cv, sl_stack_run_forrest_run)
# cv_ss_fit <- cv_ss$train(italy_task)
# debug_predict(cv_ss_fit)
# preds <- cv_ss_fit$predict()
# simple covariate screening

sl <- generate_learners(stack = screen_lasso_pipe, metalearner_stratified = FALSE)

# fit on Italy and check re-substitution performance
italy_fit <- sl$train(italy_task)
# use first val set prediction for each obs

val_preds <- italy_fit$predict_fold(italy_task, "validation")

lrnr_preds <- italy_fit$fit_object$cv_fit$base_predict()
long_lrnr_names <- sprintf("Pipeline(Lrnr_screener_coefs_1e-04->Stack)_%s",long_lrnr_names)
colnames(lrnr_preds)<-short_lrnr_names[match(colnames(lrnr_preds), long_lrnr_names)]

get_obsdata <- function(fold, task) {
  list(loss_dt = data.table(
    fold_index = fold_index(),
    index = validation(),
    obs = validation(task$Y),
    id = validation(task$id),
    weights = validation(task$weights)
  ))
}

loss_dt <- origami::cross_validate(get_obsdata, italy_task$folds, italy_task)$loss_dt
loss_dt <- loss_dt[order(index, fold_index)]
loss_dt[,horizon:=(index-min(index))+1,by=list(fold_index)]
ids <- names(loss_dt)


loss_dt <- cbind(loss_dt, lrnr_preds)
loss_dt <- cbind(loss_dt, SuperLearner=val_preds)
long <- melt(loss_dt,id=ids)
long[,rse:=sqrt((obs-value)^2)]
ggplot(long,aes(x=horizon,y=log(rse),color=variable))+
  geom_smooth()+theme_bw()

ggplot(long[variable=="SuperLearner"&horizon%in%c(1,7,14,30)],aes(x=index,y=value))+
  geom_line()+
  geom_point(data=italy_data, aes(y=log_cases))+
  facet_wrap(~horizon)
ggplot(long[variable=="exp smoothing (linear)"&horizon%in%c(1,7,14,30)],aes(x=index,y=value))+
  geom_line()+
  geom_point(data=italy_data, aes(y=log_cases))+
  facet_wrap(~horizon)

loss_dt <- loss_dt[,list(preds=preds[which.max(fold_index)]),by=list(index)]
italy_data[,index:=.I]
italy_data$preds <- NULL
italy_data <- merge(italy_data, loss_dt, by="index")

saveRDS(italy_data, file = here("sandbox", "italy_results.rds"))

# visualize
italy_data <- readRDS(here("sandbox", "italy_results.rds"))
p_italy_preds <- italy_data[days<=50] %>%
  ggplot(aes(x = days, y = log_cases)) +
    geom_point() +
    geom_line(aes(y = preds)) +
    labs(
      x = "",
      y = "",
      title = ""
    ) +
    theme_bw() +
    theme(legend.background = element_rect(fill = "gray90", size = 0.25,
                                           linetype = "dotted"),
          legend.position = "none",
          legend.title = element_blank(),
          text = element_text(size = 30),
          axis.text.x = element_text(angle = 20, colour = "black", size = 26),
          axis.text.y = element_text(colour = "black", size = 26)
         )

print(p_italy_preds)
ggsave(here("graphs", "italy_preds.pdf"), plot = p_italy_preds,
       width = 20, height = 16)
