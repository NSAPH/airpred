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


#' Generate Config File Skeleton
#'
#' @param path the directory that the config file should be saved in
#'
#' @return Null, but saves a yml file with the headers needed to run the prediction model saved
#' @export
#'
#' @details
#' \itemize{
#'   \item{\code{Monitor}} {The pollution type the data will be trained on}
#'   \item{\code{Data_Location}} {The directory holding the required data files}
#'   \item{\code{train}} {A boolean. If TRUE, the model run is a training run. If false,
#'                        the run is going to be used to create predictions}
#'  }
gen_config <- function(path = ".") {
  out <- list()
  out$Monitor <- ""
  out$Data_Location <- ""
  out$train <- FALSE

  out.file <- file(file.path(path, "config.yml"))
  write(as.yaml(out), file=out.file)
  close(out.file)

}
