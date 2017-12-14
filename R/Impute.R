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
#' @examples
impute_all <- function(info) {
  impute_vars <- load_yaml(paste0(path.package("airpred"),"/yaml_files/impute_vars.yml"))
  for (variable in impute_vars){
    message(paste("Imputing", variable))
    info <- MLE_impute(info, variable)
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
