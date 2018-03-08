# utils.R
#' @import yaml

load_yaml <- function(path, var="base") {
  file_list <- yaml.load_file(path)

  if (!(var %in% names(file_list))) {
    var <- "base"
  }

  return(strsplit(file_list[[var]], " ")[[1]])
}

Vector2FormulaString <- function(strvec) {
  if (length(strvec)==1) {
    return(strvec)
  }
  else {
      out <- strvec[1]
      for (string in strvec[2:length(strvec)]) {
        out <- paste(out, string, sep = "+")
      }
  }
  return(out)
}

get_formula<- function(path, var="base") {
  out <- load_yaml(path, var)
  out <- Vector2FormulaString(out)
  return(out)
}


#' List Implemented training models
#'
#' @return vector of implemented models
#' @export
#'
#' @details
#' This function returns the acceptable strings to be included in the Training_Models
#' field in the config file. A string included in that section not currently in this list
#' will result in an error.
#'
#' The currently implemented models are as follows
#' \itemize{
#'   \item{\code{nn}} {A neural network algorithm}
#'   \item{\code{forest}} {A random forest algorithm}
#'   \item{\code{gradboost}} {A gradient boost algorithm}
#' }
#'
implemented_models <- function() {
  out <- yaml.load_file(file.path(path.package("airpred"),"yaml_files","implemented_models.yml"))
  return(out$base)
}
