## train.R
## Train Code

#' Train Neural Net
#'
#' @param info data frame with model data
#' @param train_ind vector with randomly selected indicies for use as the training set
#'
#' @return h2o model
#'
#' @importFrom h2o h2o.deeplearning
train_nn <- function(info, train_ind) {
 model <- h2o.deeplearning(y = "MonitorData",
                           x = setdiff(names(info), c("MonitorData", "site", "date", "year")),
                           training_frame = info[train_ind,],
                           nfolds=get_model_param("nn", "nfolds"),
                           fold_assignment=get_model_param("nn", "fold_assignment"),
                           seed=get_model_param("nn", "seed"),
                           keep_cross_validation_predictions = get_model_param("nn", "keep_cross_validation_predictions"),
                           activation=get_model_param("nn", "activation"),hidden=get_model_param("nn", "hidden"),
                           epochs=get_model_param("nn", "epochs"),
                           epsilon = get_model_param("nn", "epsilon"),
                           l1=get_model_param("nn", "l1"),
                           distribution=get_model_param("nn", "distribution"))
 return(model)
}

#' Train Random Forest
#'
#' @param info data frame with model data
#' @param train_ind vector with randomly selected indicies for use as the training set
#'
#' @return h2o model
#'
#' @importFrom h2o h2o.randomForest
train_forest <- function(info, train_ind) {
  model <- h2o.randomForest(y = "MonitorData",
                            x = setdiff(names(info), c("MonitorData", "site", "date", "year")),
                            training_frame = info[train_ind,],
                            nfolds=get_model_param("forest","nfolds"),
                            fold_assignment=get_model_param("forest","fold_assignment"),
                            seed=get_model_param("forest","seed"),
                            keep_cross_validation_predictions = get_model_param("forest","keep_cross_validation_predictions"),
                            ntrees=get_model_param("forest","ntrees"),
                            max_depth = get_model_param("forest","max_depth"),
                            nbins = get_model_param("forest","bins"),
                            nbins_cats = get_model_param("forest","nbins_cats"),
                            mtries = get_model_param("forest","mtries"),
                            sample_rate = get_model_param("forest","sample_rate"))

  return(model)
}

#' Train Gradient Boost
#'
#' @param info data frame with model data
#' @param train_ind vector with randomly selected indicies for use as the training set
#'
#' @return h2o model
#'
#' @importFrom h2o h2o.gbm
train_gradboost <- function(info, train_ind) {
  model <- h2o.gbm(y = "MonitorData",
                   x = setdiff(names(info), c("MonitorData", "site", "date", "year")),
                   training_frame = info[train_ind,],
                   nfolds=get_model_param("gradboost","nfolds"),
                   fold_assignment=get_model_param("gradboost","fold_assignment"),
                   seed=get_model_param("gradboost","seed"),
                   keep_cross_validation_predictions = get_model_param("gradboost","keep_cross_validation_predictions"),
                   ntrees=get_model_param("gradboost","ntrees"),
                   learn_rate = get_model_param("gradboost","learn_rate"),
                   max_depth = get_model_param("gradboost","max_depth"),
                   sample_rate = get_model_param("gradboost","sample_rate"),
                   col_sample_rate = get_model_param("gradboost","col_sample_rate"))

  return(model)
}

#' Train an h2o model using the generic architecture
#'
#' @param model the name of the function to run
#' @param info the data for use with the model
#' @param train_ind vector with randomly selected indicies for use as the training set
#'
train_generic <- function(model, info, train_ind) {

}

#' Train Air Pollution Model
#' @param init Boolean for whether or not an h2o cluster should be initiated on call of
#'        this function.
#' @param shutdown Boolean for whether or not the h2o cluster should shutdown on termination
#' @return NULL, but saves the models required to predict.
#' @export
#'
#' @importFrom h2o h2o.init as.h2o h2o.shutdown h2o.predict h2o.cbind h2o.saveModel
#' @importFrom mgcv bam s
#' @importFrom parallel detectCores
#'
#' @seealso \code{\link{train_generic}} \code{\link{train_gradboost}}
#' @seealso \code{\link{train_forest}} \code{\link{train_nn}}
train <- function(init = T, shutdown = F) {
  models <- get_training_models()
  trained <- list()
  if (init) {h2o.init()}
  ## Load data
  info <- readRDS(get_training_data())
  train_out_path <- get_training_output()
  train_ind <- sample(seq(nrow(info)), size = round(nrow(info)*0.9))
  train_ind <- sort(train_ind, decreasing = FALSE)
  ## Convert to h2o
  info <- as.h2o(info)
  ## run + save models

  if (!param_config_check()) {
    edit_params()
  }

  if (!is.null(models$nn)) {
    trained$nn <- train_nn(info, train_ind)
  }
  if (!is.null(models$forest)) {
    trained$forest <- train_forest(info, train_ind)
  }
  if (!is.null(models$gradboost)) {
    trained$gradboost <- train_gradboost(info, train_ind)
  }

  for (model in names(trained)) {
    h2o.saveModel(trained[[model]], path = file.path(train_out_path, paste0("initial_", model)))
  }

  ## Initial ensemble
    ## Assemble ensemble data frame
  ensemble_data <- data.frame(as.vector(info$MonitorData))
  names(ensemble_data)[1] <- "MonitorData"
  for (model_name in names(trained)) {
    ensemble_data[[model_name]] <- as.vector(h2o.predict(trained[[model_name]], info)$predict)
  }
  saveRDS(ensemble_data, file.path(train_out_path, "ensemble1_data.RDS"))

    ## Run Model
  ensemble <- bam(as.formula(ensemble_formula(trained)), data = ensemble_data[train_ind,],
                  nthreads = detectCores())
  saveRDS(ensemble, file.path(train_out_path,"initial_ensemble.RDS"))
  new_vals <- predict(ensemble, ensemble_data)

  ## use weights to generate nearby terms
  nearby <- gen_nearby_terms(new_vals, max(info$site))
  nearby <- as.h2o(nearby)
    ## Assign values to current dataframe
  info <- h2o.cbind(info, nearby)

  ## Store data with nearby terms

  saveRDS(info, file.path(train_out_path,"nearby_data.RDS"))

  ## re run models


  if (!is.null(models$nn)) {
    trained$nn <- train_nn(info, train_ind)
  }
  if (!is.null(models$forest)) {
    trained$forest <- train_forest(info, train_ind)
  }
  if (!is.null(models$gradboost)) {
    trained$gradboost <- train_gradboost(info, train_ind)
  }
  for (model in names(trained)) {
    h2o.saveModel(trained[[model]], path = file.path(train_out_path, paste0("nearby_", model)))
  }

  ensemble_data <- data.frame(as.vector(info$MonitorData))
  names(ensemble_data)[1] <- "MonitorData"
  for (model_name in names(trained)) {
    ensemble_data[[model_name]] <- as.vector(h2o.predict(trained[[model_name]], info)$predict)
  }

  saveRDS(ensemble_data, file.path(train_out_path, "ensemble2_data.RDS"))

  ensemble <- bam(as.formula(ensemble_formula(trained)), data = ensemble_data[train_ind,],
                 nthreads = detectCores())
  saveRDS(ensemble, file.path(train_out_path,"nearby_ensemble.RDS"))
  new_vals <- predict(ensemble, ensemble_data)

  if (shutdown) {h2o.shutdown(prompt = F)}
}

#' Build formula for ensmeble model
#'
#' returns a string with the form "MonitorData ~ s(model1) + s(model2) ...
#'
#' @param models a list of trained models
#'
#' @return A string with the formula used in the ensemble model
ensemble_formula <- function(models) {
  out <- "MonitorData ~ "
  terms <- character(0)
  for (model in names(models)) {
    terms <- c(terms, paste0("s(", model, ")"))
  }
  out <- paste0(out, Vector2FormulaString(terms))
  return(out)
}


gen_nearby_terms <- function(new_vals, num_sites) {
  ## assumes observations are ordered as following
  ## year > day > monitor
  out <- data.frame(new_vals)
  new_val_mat <- matrix(new_vals, nrow = num_sites)

  ## Spatial Terms
  for (i in 1:3) {
    site_weights <- gen_weights(term = i)
    out_vec <- site_weights %*% new_val_mat
    out[[paste0("Spatial_Lagged_",i)]] <- c(out_vec)
  }

  ## Temporal Terms
  for (i in 1:3) {
    temp_mat <- matrix(nrow = nrow(new_val_mat), ncol = ncol(new_val_mat))
    for (j in num_sites) {
      temp_mat[j,] <- as.numeric(stats::filter(new_val_mat[j,], filter_term(i), sides = 2))
    }
    out[[paste0("Temporal_Lagged_",i)]] <- c(temp_mat)
  }
  out$new_vals <- NULL
  return(out)
}


## Here to keep the nearby term code cleaner
## Currently hardcoded, would like to come up with a non-hardcoded weighting function in the future
filter_term <- function(val) {
  out <- matrix(nrow = 3, ncol = 5)
  out[1,] <- rep(1/5, 5)
  out[2,] <- c(1/9, 2/9, 1/3, 2/9, 1/3)
  out[3,] <- c(1/16, 3/16, 1/2, 3/16, 1/16)
  return(out[val,])
}
