library(tidyverse)
library(data.table)
library(future)
library(sl3)
library(here)
library(deSolve)

# load helper package
devtools::document(here("tlversecovidforecast"))
devtools::load_all(here("tlversecovidforecast"))

batch_size <- 5
data <- read.csv(here("Data", "training_processed.csv"))
test_data <- read.csv(here("Data", "test_processed.csv"))
setDT(data)
setDT(test_data)
data <- data[!(days %in% unique(test_data$days))]

task_all <- generate_task(data, "log_cases", batch = batch_size)
task <- task_all[data$region=="Italy"]

lrnr_sir <- Lrnr_SIR$new()
fit <- lrnr_sir$train(task)
pred <- fit$predict()

lrnr_sir <- Lrnr_SIR$new()
fit_regions <- lrnr_sir$train(task_all)
preds<-fit_regions$predict(task_all)
