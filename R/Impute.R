# Imputation Code

#' Generate the weights for use in the mixed imputation model
#'
#' @param info a data.table or tibble with variables corresponding to those contained in the
#'             yaml files in the package.
#' @param var a string containing the name of the variable to be imputed
#'
#'
#' @import dplyr
#' @import tidyr
#' @importFrom stats as.formula binomial glm predict.glm
#' @include utils.R
#' @export
get_logit_weights <- function(info, var) {


  info <- mutate(info, logit_var = is.na(info[[var]]))
  ## assemble formula
  covars <- get_formula(paste0(path.package("airpred"),"/yaml_files/logit_formula.yml"), var)

  logit_model <- glm(as.formula(paste0("logit_var~",covars)),family = binomial(link = "logit"),
                     data = info)

  mle_weight <- 1/(1 - predict.glm(logit_model, type = "response"))

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
#' @examples
MLE_impute <- function(info, var) {
  weights <- get_logit_weights(info, var)
  covars <- get_formula(paste0(path.package("airpred"),"/yaml_files/lme_formula.yml"), var)

  ## Generate Model
  m1.lme <- lmer(as.formula(paste0(var,"~",covars)),info, weights = info$weights)

  ## Replace Values
  new_vals <- predict(m1.lme)
  info[[var]][is.na(info[[var]])] <- new_vals[is.na(info[[var]])]

  return(info)

}

impute_all <- function(info) {
  impute_vars <- load_yaml(paste0(path.package("airpred"),"/yaml_files/impute_vars.yml"))
  for (variable in impute_vars){
    info <- MLE_impute(info, variable)
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
#' @param info
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
