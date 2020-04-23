library(here)
library(tidyverse)
library(data.table)
remotes::install_github("tlverse/origami@devel")
remotes::install_github("tlverse/sl3@timeseries-overhaul")
library(future)
library(sl3)

# TODO: why aren't these being loaded automatically?
library(hts)
library(earth)

batch_size <- 3
# get/set miniconda python for kerasR
reticulate::use_python(system("which python3"))

# data.table threading vs. future (probably unnecessary)
# setDTthreads(2)
# plan(multiprocess, workers = availableCores() - getDTthreads())

# load helper package
devtools::document(here("tlversecovidforecast"))
devtools::load_all(here("tlversecovidforecast"))

# make SL learners and read in data, 
# both fread and read_csv switch of facs to chars, read.csv avoids this
options(sl3.verbose = TRUE)
sl3_debug_mode()
sl <- generate_learners()
data <- read.csv(here("Data", "training_processed.csv"))
setDT(data)

# generate case task and training predictions
log_cases_task <- generate_task(data, "log_cases", batch = batch_size, include_tv=FALSE)
cases_fit <- sl$train(log_cases_task)

cv_risk_table <- cases_fit$cv_risk(loss_squared_error)
cv_risk_table$coefficients <- c(strat_metalearner_coefs(cases_fit), NA)
screener_name <- gsub("mean","",cv_risk_table$learner[[1]])
cv_risk_table$learner <- gsub(screener_name,"",
                              cv_risk_table$learner,
                              fixed=TRUE)
print(cv_risk_table)

# manually implement horizon stratified metalearning for now
ml_task <- cases_fit$fit_object$cv_fit$chain()

# generate forecast horizons
fold <- log_cases_task$folds[[1]]
task <- log_cases_task


get_obsdata <- function(fold, task) {
  val_times <- validation(task)$time
  max_train_time <- max(training(task)$time)
  horizons <- val_times - max_train_time
  
  list(loss_dt = data.table(
    fold_index = fold_index(),
    horizon=horizons,
    index = validation(),
    obs = validation(task$Y),
    id = validation(task$id),
    time = validation(task$time),
    weights = validation(task$weights)
  ))
}

loss_dt <- origami::cross_validate(get_obsdata, log_cases_task$folds, log_cases_task)$loss_dt
loss_dt <- loss_dt[order(index, fold_index)]

new_columns <- ml_task$add_columns(data.table(horizon=loss_dt$horizon))
new_nodes <- ml_task$nodes
new_nodes$horizon <- "horizon"
chained <- ml_task$next_in_chain(column_names = new_columns, new_nodes=new_nodes)

chained$get_data(,"horizon")

chained_h1 <- chained[forecast_horizons==1]

metalearner_competition <- make_learner(
  Lrnr_solnp, metalearner_linear_bound, loss_squared_error
)

stratified_metalearner <- Lrnr_stratified$new(
  learner = metalearner_competition, variable_stratify = "horizon"
)


learner_list <- cv_risk_table$learner

sml_fit <- stratified_metalearner$train(chained)  
horizon_coefs <- sapply(sml_fit$fit_object,coef)
hc_dt <- melt(horizon_coefs)
setDT(hc_dt)
setnames(hc_dt, names(hc_dt),c("learner","horizon","coef"))
hc_dt$learner <- gsub(screener_name,"",
                      hc_dt$learner,
                      fixed=TRUE)

hc_dt$learner <- factor(hc_dt$learner, levels=learner_list)
ggplot(hc_dt, aes(x=horizon,y=coef, color=learner))+
  geom_smooth(se=FALSE)+theme_bw()+
  xlab("Horizon (days)")+ylab("Weight")
# View(horizon_coefs)


val_preds <- sml_fit$predict()

lrnr_preds <- chained$X
colnames(lrnr_preds)<-gsub(screener_name,"",
                           colnames(lrnr_preds),
                           fixed=TRUE)

ids <- names(loss_dt)


pred_dt <- cbind(loss_dt, lrnr_preds)
pred_dt <- cbind(pred_dt, SuperLearner=val_preds)
long <- melt(pred_dt,id=ids, variable.name="learner")
perf <- long[,list(rmse=sqrt(mean((obs-value)^2))),by=list(learner,horizon)]
perf$learner <- factor(perf$learner, levels=learner_list)
perf_plot <- ggplot(perf,aes(x=horizon,y=rmse,color=learner))+
  geom_smooth(size=1, se =FALSE)+theme_bw()+xlab("Forecast Horizon (days)")+
  ylab("rMSE (Log Cases)")+
  scale_color_discrete("Learner")

print(perf_plot)

perf_plot+scale_y_continuous(limits=c(0,1))

# california_data <- fread(here("Data", "training_processed.csv"))
california_data <- data[region=="California"]
california_data[,date:=as.POSIXct(date)]
california_data[,time:=days]
california_preds <- long[learner=="SuperLearner"&
                         id=="California"]
california_preds <- merge(california_preds, california_data[,list(date, time)],by="time")
california_preds[,fold_date:=min(date),by=list(fold_index)]

ggplot(california_preds,
       aes(x=date,y=value))+
  geom_line(color="red",size=1)+
  geom_line(data=california_data, aes(y=log_cases),size=1)+
  xlab("Date")+ylab("log(Cases)")+theme_bw()+
  geom_vline(linetype="dashed",aes(xintercept=fold_date))+
  scale_color_discrete("Model Fit Date")+facet_wrap(~fold_date)

stack_fit <- cases_fit$fit_object$full_fit$fit_object$learner_fits[[1]]$fit_object$learner_fits[[1]]$fit_object$learner_fits[[2]]
glm_fit <- stack_fit$fit_object$learner_fits[["glm"]]
glm_coefs <- coef(glm_fit)
glm_coefs[order(abs(glm_coefs), decreasing = TRUE)]
