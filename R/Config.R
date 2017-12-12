## Config.R
## Functions related to handling the config file

get_final_date <- function() {
  if (!file.exists("config.yml")) {
    stop("No config file found, try running gen_config()")
  }
  return(ymd(yaml.load_file("config.yml")$finalday))
}

get_save_location <- function() {
  if (!file.exists("config.yml")) {
    stop("No config file found, try running gen_config()")
  }
  return(yaml.load_file("config.yml")$Data_Save_Location)
}

get_csv_location <- function() {
  if (!file.exists("config.yml")) {
    stop("No config file found, try running gen_config()")
  }
  return(yaml.load_file("config.yml")$csv_path)
}

get_impute_location <- function() {
  if (!file.exists("config.yml")) {
    stop("No config file found, try running gen_config()")
  }
  return(yaml.load_file("config.yml")$Imputation_Models)
}

get_mid_process_location <- function() {
  if (!file.exists("config.yml")) {
    stop("No config file found, try running gen_config()")
  }
  return(yaml.load_file("config.yml")$Mid_Process_Data)
}

#' Generate Config File Skeleton
#'
#' @param default A boolean determining whether or not
#'        default values should be loaded into the generated config file
#' @param path the directory that the config file should be saved in
#' @param in_list An optional list that can be passed in to generate values for a config file.
#'        Currently, the list is not validated, so the config file is not guaranteed to work with
#'        the structure.
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
#'   \item{\code{csv_path}} {The path where the assembled data is stored as a csv}
#'   \item{\code{Imputation_Models}} {The path where the imputation models should be saved.}
#'   \item{\code{Mid_Process_Data}} {The path where data should be saved between imputation, normalization
#'                                   and transformation steps}
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
    out$csv_path <- ""
    out$Imputation_Models <- ""
    out$Mid_Process_Data <- ""
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
