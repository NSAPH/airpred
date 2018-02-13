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
                           nfolds=10, fold_assignment="Modulo",seed=271828,
                           keep_cross_validation_predictions = TRUE,
                           activation="Rectifier",hidden=c(200,200),epochs=50,
                           epsilon = 1e-08,l1=1e-05,distribution="AUTO")
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
                            nfolds=10,
                            fold_assignment="Modulo",seed=271828,
                            keep_cross_validation_predictions = TRUE,
                            ntrees=5,max_depth = 9,nbins = 20,nbins_cats = 449,
                            mtries = 4,sample_rate = 0.41536)

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
                   nfolds=10,
                   fold_assignment="Modulo", seed=271828,
                   keep_cross_validation_predictions = TRUE,
                   ntrees=100,learn_rate = 0.1,max_depth = 5,
                   sample_rate = 1,col_sample_rate = 0.5)

  return(model)
}


#' Train Air Pollution Model
#'
#' @return
#' @export
#'
#' @importFrom h2o h2o.init as.h2o h2o.shutdown h2o.predict
#' @importFrom gam gam s
train <- function() {
  models <- get_training_models()
  trained <- list()
  h2o.init()
  ## Load data
  info <- readRDS("../test_data/test_prepped.RDS") ## Change later, config value
  train_ind <- sample(seq(nrow(info)), size = round(nrow(info*9)))
  train_ind <- sort(train_ind, decreasing = FALSE)
  ## Convert to h2o
  info <- as.h2o(info)
  ## run + save models
  if (!is.null(models$nn)) {
    trained$nn <- train_nn(info, train_ind)
  }
  if (!is.null(models$forest)) {
    trained$forest <- train_forest(info, train_ind)
  }
  if (!is.null(models$gradboost)) {
    trained$gradboost <- train_gradboost(info, train_ind)
  }
  saveRDS(trained, "initial_trained.RDS")

  ## Initial ensemble
    ## Assemble ensemble data frame
  ensemble_data <- data.frame(as.vector(info$MonitorData))
  names(ensemble_data)[1] <- "MonitorData"
  for (model_name in names(trained)) {
    ensemble_data[[model_name]] <- as.vector(h2o.predict(trained[[model_name]], info)$predict)
  }
  saveRDS(ensemble_data, "ensemble1_data.RDS")

    ## Run Model
  ensemble <- gam(as.formula(ensemble_formula(trained)), data = ensemble_data[train_ind,])
  saveRDS(ensemble, "initial_ensemble.RDS")
  new_vals <- predict(ensemble, ensemble_data)

  ## use weights to generate nearby terms
  nearby <- gen_nearby_terms(new_vals, max(info$site))
    ## Assign values to current dataframe

  ## Store data with nearby terms

  saveRDS(info, "nearby_data.RDS")

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
  saveRDS(trained, "nearby_trained.RDS")

  ensemble_data <- data.frame(as.vector(info$MonitorData))
  names(ensemble_data)[1] <- "MonitorData"
  for (model_name in names(trained)) {
    ensemble_data[[model_name]] <- as.vector(h2o.predict(trained[[model_name]], info)$predict)
  }

  ensemble <- gam(as.formula(ensemble_formula(trained)), data = ensemble_data[train_ind,])
  saveRDS(ensemble, "initial_ensemble.RDS")
  new_vals <- predict(ensemble, ensemble_data)

  h2o.shutdown()
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
      temp_mat[j,] <- as.numeric(filter(new_val_mat[j,], filter_term(i), sides = 2))
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
  return(out[i,])
}
