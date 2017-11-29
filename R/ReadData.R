## Data Reading file

#' Convert matlab file to RDS
#'
#' @param path path to a file
#'
#' @return none
#'
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom R.matlab readMat
Matlab2RDS <- function(path) {
  if (file_ext(path) != "mat") {
    return()
  } else {
    x <- readMat(path)
    saveRDS(x$Result, file = paste0(file_path_sans_ext(path),".rds"))
  }

}



process_annual <- function(){}

process_daily <- function(){}

process_location <- function(){}

process_windspeed <- function(){}

process_monitor <- function(){}

process_data <- function(path = "../predictions/EPANO2") {
  files <- gen_data_paths(path)

  for (var in names(files)) {
    if (var == "MonitorData") {
      process_monitor()
    } else if (substr(var,1,20) == "REANALYSIS_windspeed") {
      process_windspeed()
    } else if (length(files[[var]] == 1)) {
      process_location()
    } else if (readMat(files[[var]][1])$Result == 1) {
      process_annual()
    } else {
      process_daily()
    }
  }
}

#' Generate Data File path
#'
#' @param path the directory storing the data to be used in the model
#'
#' @return
#' A list where each object is a variable containing a vector of all existing files
#' in the checked directory that match the file name pattern for that variable specified in
#' the Data_Location.yml file.
#'
#'
#'
#'
gen_data_paths <- function(path = "../predictions/EPANO2") {

  # Initialize List
  file.yaml <- list()

  # get names of variables in dataset
  varlist <- yaml.load_file(file.path(path.package("airpred"),"yaml_files",
                                      "Data_Location.yml"))


  for (variable in names(varlist)) {
    if (length(varlist[[variable]]) == 2) {
      directory <- varlist[[variable]][1]
      files <- file.path(directory,list.files(file.path(path, directory),
                                              pattern = varlist[[variable]][2]))
    } else {
      directory <- varlist[[variable]][2]
      files <- file.path(directory,list.files(file.path(path, directory),
                                              pattern = varlist[[variable]][3]))
    }
    if (length(files) > 0) {
      listname <- variable
      # while (!is.null(file.yaml[[listname]])){
      #   listname <- paste0(listname, "bad")
      # }
      file.yaml[[listname]] <- files
      #} else {
      #  file.yaml[[variable]] <- c("bad", directory)
    }
  }

  return(file.yaml)
}
