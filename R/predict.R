## predict.R
## Code to handle reading in trained models and generate predictions using them.

airpred.predict <- function() {

  ## Place Holder, need to figure out best way to get the data here
  info <- data.frame()


  training_output_dir <- get_training_output()
  initial_models <- readRDS(file.path(training_output_dir, "initial_trained.RDS"))

  preensemble <- data.table(start = rep_len(0, nrow(info)))
  for (model in names(initial_models)) {
    preensemble[[model]] <- predict(initial_models[[model]], newdata = info)
  }
  preensemble$start <- NULL

  initial_ensemble <- readRDS(file.path(training_output_dir, "initial_ensemble.RDS"))

  initial_prediction <- predict(initial_ensemble, newdata = preensemble)


  ## Should be functionalized longterm, currently done this way to avoid duplicating the dataset in memory
  nearby <- gen_nearby_terms(new_vals, max(info$site))

  ## Assign values to current dataframe
  for (name in names(nearby)) {
    info[[name]] <- nearby[[name]]
  }



  nearby_models <- readRDS(file.path(training_output_dir, "nearby_trained.RDS"))

  for (model in names(nearby_models)) {
    preensemble[[model]] <- predict(nearby_models[[model]], newdata = info)
  }
  nearby_ensemble <- readRDS(file.path(training_output_dir, "nearby_ensemble.RDS"))
  predictions <- predict(nearby_ensemble, newdata = preensemble)

  ## TODO
  ## Figure out best way to assign location to predictions
  ## Figure out best format to save predictions
  ## Further functionalize this algorithm
  ## Condense objects to minimize memory use

}

load_predict_data <- function() {

}


