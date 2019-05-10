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
  ## Not used with h2o imputation
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
  return(out)
}

#' Load csv or RDS data
#'
#' @param path the path of the data beint loaded
#'
#' Load a data.frame like object stored as a csv or rds file. Passing any other
#' data type will return an error.
#'
#' @importFrom tools file_ext
#' @export
load_data <- function(path) {
  if (file_ext(path) == "csv") {
    return(fread(path))
  } else if (file_ext(path) == "rds" | file_ext(path) == "RDS") {
    out <- readRDS(path)
    if (!(any(class(out) %in% "data.frame"))) {
      stop("error, ",path," is not a data.frame like object")
    }
    return(out)
  } else {
    stop(path, " is not a supported file type")
  }
}


#' get a boolean value from the user
#'
#' @param prompt the question to be asked
#'
#' @return
#' @export
#'
#' @importFrom stringr str_wrap
yes <- function(prompt) {
  done <- F
  while (!done) {
    x <- readline(str_wrap(paste(prompt, "(y/n):  ")))
    if (!(x %in% c("y", "n"))) {
      message("invalid input")
    } else {
      done <- T
    }
  }

  return(x == "y")
}

get_input <- function(prompt) {
  x <- readline(str_wrap(paste(prompt, "Enter a value or 'x' to skip:  ")))
  if (x == "x") {
    return(NULL)
  } else {
    return(x)
  }
}

multi_input <- function(prompt) {
  out <- NULL
  while (TRUE) {
    x <- readline(str_wrap(paste(prompt, "Enter a value or 'x' to finish:  ")))
    if (x == "x") {
      return(out)
    } else {
      out <- c(out, x)
    }
  }
}
