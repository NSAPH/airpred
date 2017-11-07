# Imputation Code

#' Generate the weights for use in the mixed imputation model
#'
#' @param info a data.table or tibble with variables corresponding to those contained in the
#'             yaml files in the package.
#' @param var a string containing the name of the variable to be imputed
#'
#'
#' @import dplyr
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
#' @importFrom nlme lme
#' @examples
MLE_impute <- function(info, var) {
  info$weights <- get_logit_weights(info, var)
  covars <- get_formula(paste0(path.package("airpred"),"/yaml_files/lme_formula.yml"), var)
  randoms <- get_formula(paste0(path.package("airpred"),"/yaml_files/lme_formula.yml"), "random")



  m1.lme <- lme(as.formula(paste0(var,"~",covars)),info,
                random = as.formula(paste0("~",randoms)), weights = ~weights)


}
