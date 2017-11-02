# Imputation Code
#' @include utils.R
#'

get_logit_weights <- function(data, var) {

  ## assemble formula
  covars <- get_formula(paste0(path.package("airpred"),"/yaml_files/logit_formula.yml"), var)


}
