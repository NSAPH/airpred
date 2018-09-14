## predict.R
## Code to handle reading in trained models and generate predictions using them.


#' Generate predictions from a previously trained model
#'
#'
#'
#'
#' @importFrom h2o h2o.loadModel
#' @export
airpred.predict <- function(prepped = T) {

  h2o.init()

  if (prepped) {
    info <- load_data(file.path(get_predict_mid_process(), "predict_prepped.rds"))
  } else {
    info <- load_predict_data()
  }
  message("Data Loaded")
  message(class(info))
  info <- as.h2o(info)
  message("here")

  training_output_dir <- get_training_output()
  selected_models <- get_training_models()
  initial_models <- list()
  for (model in names(selected_models)) {
    message(model)
    model_dir <- file.path(training_output_dir, paste0("initial_", model))
    print(model_dir)
    initial_models[[model]] <- h2o.loadModel(file.path(model_dir, list.files(model_dir)))
  }
  message("Models Loaded")

  preensemble <- data.table(start = rep_len(0, nrow(info)))
  for (model in names(initial_models)) {
    preensemble[[model]] <- as.vector(h2o.predict(initial_models[[model]], info)$predict)
  }

  message("Predictions Generated")

  preensemble$start <- NULL

  initial_ensemble <- readRDS(file.path(training_output_dir, "initial_ensemble.RDS"))

  initial_prediction <- predict(initial_ensemble, newdata = preensemble)

  message("Ensemble Completed")

  if(get_two_stage()) {
  ## Should be functionalized longterm, currently done this way to avoid duplicating the dataset in memory
  nearby <- gen_nearby_terms(initial_prediction, max(info$site))

  nearby <- as.h2o(nearby)
  ## Assign values to current dataframe
  info <- h2o.cbind(info, nearby)



  nearby_models <- list()
  for (model in names(selected_models)) {
    model_dir <- file.path(training_output_dir, paste0("nearby_", model))
    nearby_models[[model]] <- h2o.loadModel(file.path(model_dir, list.files(model_dir)))
  }

  for (model in names(nearby_models)) {
    preensemble[[model]] <- as.vector(h2o.predict(nearby_models[[model]], newdata = info)$predict)
  }
  nearby_ensemble <- readRDS(file.path(training_output_dir, "nearby_ensemble.RDS"))
  predictions <- predict(nearby_ensemble, newdata = preensemble)
  } else {
    predictions <- initial_prediction
  }
  message(class(predictions))

  message("Writing Predictions!")
  predictions <- data.frame(as.vector(info[[get_site_var()]]), as.vector(info[[get_date_var()]]), predictions)

  names(predictions) <- c("site", "date", "MonitorData")
  if (get_normalize()) {
  predictions <- denormalize_all(predictions)
  }
  if (get_transform()) {
  predictions <- detransform_all(predictions)
  }
  saveRDS(predictions, file = file.path(get_predict_output(), "predictions.RDS"))



  ## TODO
  ## Figure out best way to assign location to predictions
  ## Figure out best format to save predictions
  ## Further functionalize this algorithm
  ## Condense objects to minimize memory use

}


#' Load and prepare data for prediction
#'
#' Uses \code{predict_mid_process} and \code{predict_data} from the
#' configuration file.
#'
#' @export
load_predict_data <- function() {

  mid_process_path <- get_predict_mid_process()
  data_path <- get_predict_data()
  info <- load_data(data_path)

  if (get_transform()) {
  message("Transforming Data")
  info <- transform_all(info, store = F, load = T)
  saveRDS(info, file = file.path(mid_process_path, "predict_post_transform.rds"))
  }

  if (get_normalize()) {

    message("Normalizing Data")
  info <- normalize_all(info, store = F, load = T)
  saveRDS(info, file = file.path(mid_process_path, "predict_post_normal.rds"))
  }

  if (get_impute()) {
  message("Imputing Data")
  info <- predict_impute_all(info)
  }
  saveRDS(info, file = file.path(mid_process_path, "predict_prepped.rds"))

  return(info)
}



