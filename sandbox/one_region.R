library(here)
library(tidyverse)
library(data.table)
remotes::install_github("tlverse/origami@devel")
remotes::install_github("tlverse/sl3@timeseries-overhaul")
library(future)
library(sl3)
library(knitr)
library(kableExtra)
source(here("R", "utils.R"))

# get/set miniconda python for kerasR
reticulate::use_python(system("which python3"))

# data.table threading vs. future (probably unnecessary)
dt_cores <- as.integer(unname(round(0.2 * availableCores())))
future_cores <- as.integer(unname(availableCores()) - dt_cores)
setDTthreads(dt_cores)
plan(multiprocess, workers = future_cores)

# load helper package
devtools::document(here("tlversecovidforecast"))
devtools::load_all(here("tlversecovidforecast"))

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

mean_lrnr <- Lrnr_mean$new()
glm_lrnr <- Lrnr_glm$new()
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
arima_lrnr <- make_learner(Lrnr_arima)
expsmooth_lrnr <- make_learner(Lrnr_expSmooth)
arima_strat_lrnr <- Lrnr_multiple_ts$new(learner = arima_lrnr)
expsmooth_strat_lrnr <- Lrnr_multiple_ts$new(learner = expsmooth_lrnr)

# library for Stack
stack_lib <- unlist(list(mean_lrnr, glm_lrnr, ridge_lrnr),
                    recursive = TRUE)

# make stack for SL
stack_lrnrs <- make_learner(Stack, stack_lib)

# LASSO screener
#screener_lasso <- make_learner(Lrnr_screener_coefs, lrnr_lasso,
                               #threshold = 1e-1)
#screener_lasso_flex <- make_learner(Lrnr_screener_coefs, lrnr_lasso,
                                    #threshold = 1e-3)
screener_lasso <- make_learner(Lrnr_screener_coefs, lasso_lrnr,
                               threshold = 1e-2)

# pipelines
#screen_lasso_pipe <- make_learner(Pipeline, screener_lasso, stack)
#screen_lasso_flex_pipe <- make_learner(Pipeline, screener_lasso_flex, stack)
screen_lasso_pipe <- make_learner(Pipeline, screener_lasso, stack_lrnrs)

# final stack for SL
#stack_screeners <- make_learner(Stack, screen_lasso_pipe,
                                #screen_lasso_flex_pipe)
sl_stack_run_forrest_run <- make_learner(Stack, screen_lasso_pipe)



# simple covariate screening
sl3_debug_mode()
sl <- generate_learners(stack = screen_lasso_pipe, metalearner_stratified = FALSE)
data <- read_csv(here("Data", "training_processed.csv"))
test_data <- read_csv(here("Data", "test_processed.csv"))
setDT(data)
setDT(test_data)

# generate case task and subset to Italy data
log_cases_task <- generate_task(data, "log_cases")
italy_task <- log_cases_task[data$region == "Italy"]
italy_data <- data[data$region == "Italy", ]

# fit on Italy and check re-substitution performance
italy_fit <- sl$train(italy_task)
italy_data$preds <- italy_fit$predict()
saveRDS(italy_data, file = here("sandbox", "italy_results.rds"))

# visualize
italy_data <- readRDS(here("sandbox", "italy_results.rds"))
p_italy_preds <- italy_data %>%
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
ggsave(here("graphs", "italy_preds.pdf"), plot = p_italy_preds,
       width = 20, height = 16)
