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

get_final_date <- function() {
  return(ymd(yaml.load_file("config.yml")$finalday))
}

get_save_location <- function() {
  return(yaml.load_file("config.yml")$Data_Save_Location)
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
#'   \item{\code{Data_Save_Location}} {The directory processed data files
#'                                     should be saved in}
#'   \item{\code{train}} {A boolean. If TRUE, the model run is a training run. If false,
#'                        the run is going to be used to create predictions}
#'   \item{\code{finalday}} {The date of the last day covered by the data set}
#'  }
gen_config <- function(default = TRUE, path = ".", in_list = NULL) {
  if (default) {
    out <- yaml.load_file(file.path(path.package("airpred"),"yaml_files",
                               "Config_Default.yml"))
  } else if (!is.null(in_list)) {
    ## Need to validate input here
    out <- in_list
  } else {
    out <- list()
    out$Monitor <- ""
    out$Data_Location <- ""
    out$Data_Save_Location <- ""
    out$train <- TRUE
    out$finalday <- 20180101
  }

  out.file <- file(file.path(path, "config.yml"))
  write(as.yaml(out), file=out.file)
  close(out.file)

}

#' Remove current config file
#'
#' @return none
#' @export
#'
clean_up_config <- function() {
  file.remove("config.yml")
}
