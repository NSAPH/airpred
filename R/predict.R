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
    info <-
      load_data(file.path(get_predict_mid_process(), "predict_prepped.rds"))
  } else {
    info <- load_predict_data(shutdown = F)
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
    model_dir <-
      file.path(training_output_dir, paste0("initial_", model))
    print(model_dir)
    initial_models[[model]] <-
      h2o.loadModel(file.path(model_dir, list.files(model_dir)))
  }
  message("Models Loaded")

  if (length(names(initial_models)) > 1) {
    ensemble_model <- h2o.loadModel(file.path(training_output_dir, "initial_ensemble",
                                              list.files(file.path(training_output_dir,
                                                                   "initial_ensemble"))))
    initial_prediction <- as.vector(h2o.predict(ensemble_model, info)$predict)
  } else {
    message("Single model, no ensemble")
    initial_prediction <-
      as.vector(h2o.predict(initial_models[[1]], info)$predict)
    message("Predictions Generated")
  }

  if (get_two_stage()) {
    ## Should be functionalized longterm, currently done this way to avoid duplicating the dataset in memory
    nearby <- gen_nearby_terms(initial_prediction, max(info$site))

    nearby <- as.h2o(nearby)
    ## Assign values to current dataframe
    info <- h2o.cbind(info, nearby)



    nearby_models <- list()
    for (model in names(selected_models)) {
      model_dir <-
        file.path(training_output_dir, paste0("nearby_", model))
      nearby_models[[model]] <-
        h2o.loadModel(file.path(model_dir, list.files(model_dir)))
    }

    if (length(names(nearby_models)) > 1) {
      ensemble_model <- h2o.loadModel(file.path(training_output_dir, "nearby_ensemble",
                                                list.files(file.path(training_output_dir,
                                                                     "nearby_ensemble"))))
      predictions <- as.vector(h2o.predict(ensemble_model, info)$predict)
    } else {
      predictions <-
        as.vector(h2o.predict(nearby_models[[1]], info)$predict)
    }
  } else {
    predictions <- initial_prediction
  }
  message(class(predictions))

  message("Writing Predictions!")
  predictions <-
    data.frame(as.vector(info[[get_site_var()]]), as.vector(info[[get_date_var()]]), predictions)

  names(predictions) <- c("site", "date", "MonitorData")

  message(class(predictions[["MonitorData"]]))
  saveRDS(predictions, file = file.path(get_predict_output(), "debug.rds"))

  if (get_standardize()) {
    predictions <- destandardize_all(predictions)
  }

  saveRDS(predictions, file = file.path(get_predict_output(), "predictions.rds"))
  if (class(predictions[["MonitorData"]]) == "list") {
    stop("Error in predictions, output as list")
  }



  ## TODO
  ## Figure out best way to assign location to predictions
  ## Figure out best format to save predictions
  ## Further functionalize this algorithm
  ## Condense objects to minimize memory use

}


#' Load and prepare data for prediction
#'
#' @param init should an h2o cluster be initialized during
#'     the imputation process (default = TRUE)
#' @param shutdown should the h2o cluster used during imputation
#'     be shutdown after imputation (default = TRUE)
#' Uses \code{predict_mid_process} and \code{predict_data} from the
#' configuration file.
#'
#' @export
load_predict_data <- function(init = T, shutdown = T) {
  mid_process_path <- get_predict_mid_process()
  data_path <- get_predict_data()
  info <- load_data(data_path)

  if (get_standardize()) {
    message("Standardizing Data")
    info <- standardize_all(info, store = F, load = T)
    saveRDS(info,
            file = file.path(mid_process_path, "predict_standardized.rds"))
  }
  if (get_impute()) {
    message("Imputing Data")
    info <- h2o_predict_impute_all(info, init = init, shutdown = shutdown)
  }
  saveRDS(info, file = file.path(mid_process_path, "predict_prepped.rds"))

  return(info)
}
