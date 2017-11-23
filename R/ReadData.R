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


