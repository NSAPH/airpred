# impute.R
# Imputation Code

#' Generate the weights for use in the mixed imputation model
#'
#' @param info a data.table or tibble with variables corresponding to those contained in the
#'             yaml files in the package.
#' @param var a string containing the name of the variable to be imputed
#'
#'
#' @import dplyr
#' @importFrom stats as.formula binomial glm predict.glm na.exclude
#' @include utils.R
#' @export
get_logit_weights <- function(info, var) {


  info <- mutate(info, logit_var = is.na(info[[var]]))
  ## assemble formula
  covars <- get_formula(paste0(path.package("airpred"),"/yaml_files/logit_formula.yml"), var)

  logit_model <- glm(as.formula(paste0("logit_var~",covars)),family = binomial(link = "logit"),
                     data = info, na.action = na.exclude)

  mle_weight <- 1/(1 - predict.glm(logit_model, type = "response"))
  ##message("Logit NAs:", sum(is.na(mle_weight)))
  mle_weight[is.na(mle_weight)] <- 0

  info$logit_var <- NULL

  return(mle_weight)
}


#' Impute missing values using a H2O Random Forest model
#'
#' @param info h2o data frame
#' @param var String containing the name of the variable to be imputed
#'
#' @details This function assumes that the h2o cluster has already been initialized
#' and that the data has already been loaded to the h2o cluster. To protect against
#' errors it is not exported.
#'
#' H2O's Random Forests interpret missingness in the input as containing information
#' and therefore the output will have values for all inputs, regardless of the missingness
#' of the input.
#'
#'
h2o_impute <- function(info, var) {

  covars <- get_impute_formula()
  impute_model <- h2o.randomForest(y = var,
                                   x = covars,
                                   training_frame = info,
                                   model_id = paste0(var, "_impute"))
  h2o.saveModel(impute_model,path = file.path(get_impute_location(), var), force = T)
  return(as.vector(h2o.predict(impute_model, info)))
}

#' Impute all specified variables using h2o
#'
#' @param info dataframe containing all variables for training
#' @param init should the h2o instance be initialized?
#' Only set as F if the h2o instance has already been initialized.
#' @param shutdown Should the h2o instance be shutdown after imputation, defaults to T.
#'
#' @return a data frame
#' @export
#'
#' @examples
#' \dontrun{
#' h2o.init()
#' info <- h2o_impute_all(info, init = F)
#' }
h2o_impute_all <- function(info, init = T, shutdown = T) {
  if (init) {
    h2o.init()
  }
  info_h2o <- as.h2o(info)
  impute_vars <- get_impute_vars()
  for (variable in impute_vars){
    message(paste("Imputing", variable))
    info[[variable]] <- h2o_impute(info_h2o, variable)
    if (all(!is.na(info[[variable]]))) message("Impute Success")
  }

  if (shutdown) {
    h2o.shutdown(prompt = F)
  }

  return(info)
}

#' Impute varibles using mixed linear models
#'
#' @param info data set
#' @param var the variable to be imputed
#'
#' @return a vector of the imputed values
#' @export
#'
#' @importFrom lme4 lmer
#' @importFrom stats predict na.omit
#' @importFrom gam na.gam.replace
#'
#' @examples
MLE_impute <- function(info, var) {
  info$weights <- get_logit_weights(info, var)
  covars <- get_formula(paste0(path.package("airpred"),"/yaml_files/lme_formula.yml"), var)
  ## Generate Model
  m1.lme <- lmer(as.formula(paste0(var,"~",covars)),info, weights = info$weights, na.action = na.omit)
  saveRDS(m1.lme, file = file.path(get_impute_location(), paste0(var, "ImputeModel.RDS")))

  ## Replace Values
  new_vals <- predict(m1.lme, newdata = info, na.action = na.gam.replace, allow.new.levels=T)
  ##message("MLE NAs:", sum(is.na(new_vals)))
  ##print(all(!is.na(new_vals)))
  info[[var]][is.na(info[[var]])] <- new_vals[is.na(info[[var]])]
  ##print(all(!is.na(info[[var]])))
  if (sum(is.na(info[[var]])) != 0) {
    warning(paste("Error in imputation, missing values still present in", var))
  }

  info$weights <- NULL
  return(info)

}

#' Impute full dataset
#'
#' @param info the dataset being imputed
#'
#' @return data.table with replaced missing values
#' @export
#'
#' @details
#' As a side effect of this function, all of the models used to impute the values
#' are saved in the directory specified in the configuration file. Ensure that there
#' is enough space available in that folder to store the generated models. These models
#' will later be used when data is being prepared for prediction.
impute_all <- function(info) {
  impute_vars <- load_yaml(paste0(path.package("airpred"),"/yaml_files/impute_vars.yml"))
  for (variable in impute_vars){
    message(paste("Imputing", variable))
    info <- MLE_impute(info, variable)
    if (all(!is.na(info[[variable]]))) message("Impute Success")
  }

  return(info)
}

#' Impute missing values using a previously trained H2O Random Forest model
#'
#' @param info h2o data frame
#' @param var String containing the name of the variable to be imputed
#'
#' @details This function assumes that the h2o cluster has already been initialized
#' and that the data has already been loaded to the h2o cluster. To protect against
#' errors it is not exported.
#'
#' H2O's Random Forests interpret missingness in the input as containing information
#' and therefore the output will have values for all inputs, regardless of the missingness
#' of the input.
#'
#'
h2o_predict_impute <- function(info, var) {
  impute_model <- h2o.loadModel(file.path(get_impute_location(),
                                          var,
                                          paste0(var, "_impute")))
  return(as.vector(h2o.predict(impute_model, info)))
}

#' Impute all specified variables using h2o with previously trained models
#'
#' @param info dataframe containing all variables for training
#' @param init should the h2o instance be initialized?
#' Only set as F if the h2o instance has already been initialized.
#' @param shutdown Should the h2o instance be shutdown after imputation, defaults to T.
#'
#' @return a data frame
#' @export
#'
#' @examples
#' \dontrun{
#' h2o.init()
#' info <- h2o_predict_impute_all(info, init = F)
#' }
h2o_predict_impute_all <- function(info, init = T, shutdown = T) {
  if (init) {
    h2o.init()
  }

  info_h2o <- as.h2o(info)
  impute_vars <- get_impute_vars()
  for (variable in impute_vars){
    message(paste("Imputing", variable))
    info[[variable]] <- h2o_predict_impute(info_h2o, variable)
    if (all(!is.na(info[[variable]]))) message("Impute Success")
  }

  if (shutdown) {
    h2o.shutdown(prompt = F)
  }
}

predict_impute <- function(info, var) {

  impute_model <- readRDS(file.path(get_impute_location(), paste0(var, "ImputeModel.RDS")))
  new_vals <- predict(impute_model, newdata = info, na.action = na.gam.replace, allow.new.levels=T)
  info[[var]][is.na(info[[var]])] <- new_vals[is.na(info[[var]])]

  if (sum(is.na(info[[var]])) != 0) {
    warning(paste("Error in imputation, missing values still present in", var))
  }

  return(info)
}

#' Impute full dataset
#'
#' @param info the dataset being imputed
#'
#' @return data.table with replaced missing values
#' @export
#'
#' @details
#' This function relies on all variables having previously been imputed. If these models do not exist
#' or the directory in the configuration file is misspecified an error reporting that the file cannor be found
#' will be generated.
predict_impute_all <- function(info) {
  impute_vars <- load_yaml(paste0(path.package("airpred"),"/yaml_files/impute_vars.yml"))
  for (variable in impute_vars){
    message(paste("Imputing", variable))
    info <- predict_impute(info, variable)
    if (all(!is.na(info[[variable]]))) message("Impute Success")
  }

  return(info)
}

MLE_mcimpute <- function(var, info) {
  weights <- get_logit_weights(info, var)
  covars <- get_formula(paste0(path.package("airpred"),"/yaml_files/lme_formula.yml"), var)

  ## Generate Model
  m1.lme <- lmer(as.formula(paste0(var,"~",covars)),info, weights = info$weights)

  ## Replace Values
  new_vals <- predict(m1.lme)

  return(new_vals)

}

#' Parallel implementation of Imputation algorithm
#'
#' @param info the dataset being imputed
#'
#' @return
#' @export
#'
#' @import parallel
#' @seealso impute_all
#' @examples
impute_all_parallel <- function(info) {
  impute_vars <- load_yaml(paste0(path.package("airpred"),"/yaml_files/impute_vars.yml"))

  predictions <- mclapply(impute_vars, MLE_mcimpute, info = info, mc.cores = detectCores())


}


#' List all varibles imputed in the process
#' @export
list_imputed_variables <- function() {
  impute_vars <- load_yaml(paste0(path.package("airpred"),"/yaml_files/impute_vars.yml"))
  print(impute_vars)
}

#' List inputs to logit used in imputation
#' @export
print_logit_inputs <- function() {
  logit_vars <- load_yaml(paste0(path.package("airpred"),"/yaml_files/logit_formula.yml"))
  print(logit_vars)
}

#' List inputs to mle used in imputation
#' @export
print_MLE_inputs <- function() {
  lme_vars <- load_yaml(paste0(path.package("airpred"),"/yaml_files/lme_formula.yml"))
  print(lme_vars)
}

get_impute_vars <- function() {
  if (get_impute_var_path() == "default") {
    impute_vars <- load_yaml(paste0(path.package("airpred"),"/yaml_files/impute_vars.yml"))
  } else {
    impute_vars <- load_yaml(get_impute_var_path())
  }
  return(impute_vars)
}

get_impute_formula <- function() {
  if (get_impute_formula_path() == "default") {
    impute_vars <- load_yaml(paste0(path.package("airpred"),"/yaml_files/lme_formula.yml"))
  } else {
    impute_vars <- load_yaml(get_impute_formula_path())
  }
}


