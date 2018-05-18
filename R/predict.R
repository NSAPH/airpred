## predict.R
## Code to handle reading in trained models and generate predictions using them.

airpred.predict <- function(prepped = T) {

  if (prepped) {
    info <- load_data(file.path(get_predict_mid_process(), "predict_prepped.rds"))
  } else {
    info <- load_predict_data()
  }

  info <- as.h2o(info)

  training_output_dir <- get_training_output()
  initial_models <- readRDS(file.path(training_output_dir, "initial_trained.RDS"))

  preensemble <- data.table(start = rep_len(0, nrow(info)))
  for (model in names(initial_models)) {
    preensemble[[model]] <- as.vector(h2o.predict(trained[[model]], info)$predict)
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

  message("Transforming Data")
  info <- transform_all(info, store = F, load = T)
  saveRDS(info, file = file.path(mid_process_path, "predict_post_transform.rds"))

  message("Normalizing Data")
  info <- normalize_all(info, store = F, load = T)
  saveRDS(info, file = file.path(mid_process_path, "predict_post_normal.rds"))

  message("Imputing Data")
  info <- predict_impute_all(info)
  saveRDS(info, file = file.path(mid_process_path, "predict_prepped.rds"))

  return(info)
}



