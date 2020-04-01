load_all("tlversecovidforecast")
# run this to regenerate processed data
# setup_data()

data <- fread(here("Data/training_processed.csv"))
task <- generate_task(data)


# double_check fold structure
test_times <- function(fold, task){
  last_train_time <- max(training(task)$get_node("time"))
  first_val_time <- min(validation(task)$get_node("time"))
  expect_lt(last_train_time, first_val_time)
}

cross_validate(test_times, task$folds, task)
