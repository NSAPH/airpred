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

#' Process all data for a given data set
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
process_data <- function(path = "../predictions/EPANO2") {
  files <- gen_data_paths(path)
  save_path <- get_save_location()
  for (var in names(files)) {
    t <- Sys.time()
    if (var == "MonitorData") {
      print("Monitor")
      ##process_monitor()
    } else if (substr(var,1,20) == "REANALYSIS_windspeed") {
      print("wind")
      df <- process_windspeed(files[[var]])
      print(var)
      saveRDS(df, file = file.path(save_path, paste0(var,".RDS")))
    } else if (length(files[[var]]) == 1) {
      print("loc")
      df <- process_location(files[[var]])
      print(var)
      saveRDS(df, file = file.path(save_path, paste0(var,".RDS")))
    } else if (length(readMat(files[[var]][1])$Result[,1]) == 1) {
      print("year")
      df <- process_annual(files[[var]])
      print(var)
      saveRDS(df, file = file.path(save_path, paste0(var,".RDS")))
    } else {
      print("day")
      df <- process_daily(files[[var]])
      print(var)
      saveRDS(df, file = file.path(save_path, paste0(var,".RDS")))

    }
    print(Sys.time() - t)
  }
}

#' process annual data
#'
#' @param files vector of file paths
#'
#' @return a Data Frame containing the value, dates, and site IDs for the given variable
#'
#' @importFrom lubridate ymd years year
#'
process_annual <- function(files){
  final <- get_final_date()
  out <- data.frame(value=numeric(), site = numeric(), year = numeric())
  date_counter <- ymd(20000101)
  file_index <- 1
  while ((file_index <= length(files))&&(date_counter < final)) {
    path <- files[file_index]
    if (file_ext(path) == "mat") {
      ## Extract start date from file name and insure that it matches the date counter
      ## Done to account for the fact that not all data exists for all years
      if (date_counter == ymd(substr(path, nchar(path) - 20, nchar(path) - 13))) {
        mat_result <- readMat(path)$Result
        temp.frame <- data.frame(value = mat_result[1,],
                                 site = 1:length(mat_result[1,]),
                                 year = year(date_counter))
        out <- rbind(out, temp.frame)
        date_counter <- date_counter + years()
        file_index <- file_index + 1
      } else {
        date_counter <- date_counter + years()
      }
    } else {
      file_index <- file_index + 1
    }
  }
  return(out)
}

process_daily <- function(files){
  final <- get_final_date()
  out <- data.frame(value=numeric(), site = numeric(), year = numeric(), date = numeric())
  date_counter <- ymd(20000101)
  file_index <- 1
  while ((file_index <= length(files))&&(date_counter < final)) {
    path <- files[file_index]
    if (file_ext(path) == "mat") {
      ## Extract start date from file name and insure that it matches the date counter
      ## Done to account for the fact that not all data exists for all years
      if (date_counter == ymd(substr(path, nchar(path) - 20, nchar(path) - 13))) {
        mat_result <- readMat(path)$Result
        data_day <- date_counter
        year.frame <- data.frame(value=numeric(), site = numeric(), year = numeric(), date = numeric())
        for (i in 1:length(mat_result[,1])) {
          ##print(i)
          temp.frame <- data.frame(value = mat_result[i,],
                                   site = 1:length(mat_result[1,]),
                                   date = data_day,
                                   year = year(data_day))
          year.frame <- rbind(year.frame, temp.frame)
          data_day <- data_day + days()
        }
        out <- rbind(out, year.frame)
        date_counter <- date_counter + years()
        file_index <- file_index + 1
      } else {
        date_counter <- date_counter + years()
      }
    } else {
      file_index <- file_index + 1
    }
  }
  return(out)
}

process_location <- function(path){

  mat_result <- readMat(path)$Result
  out <- data.frame(value = mat_result[1,],
                           site = 1:length(mat_result[1,]))

  return(out)
}

process_windspeed <- function(files){
  final <- get_final_date()
  out <- data.frame(value=numeric(), site = numeric(),
                    year = numeric(), date = numeric())
  date_counter <- ymd(20000101)
  file_index <- 1
  while ((file_index <= length(files$vwnd))&&(date_counter < final)) {
    upath <- files$uwnd[file_index]
    vpath <- files$vwnd[file_index]
    if (file_ext(upath) == "mat") {
      ## Extract start date from file name and insure that it matches the date counter
      ## Done to account for the fact that not all data exists for all years
      if (date_counter == ymd(substr(upath, nchar(upath) - 20, nchar(upath) - 13))) {
        umat_result <- readMat(upath)$Result
        vmat_result <- readMat(vpath)$Result
        data_day <- date_counter
        year.frame <- data.frame(value=numeric(), site = numeric(),
                                 year = numeric(), date = numeric())
        for (i in 1:length(umat_result[,1])) {
          ##print(i)
          temp.frame <- data.frame(value = sqrt(umat_result[i,]^2 + umat_result[i,]^2),
                                   site = 1:length(umat_result[1,]),
                                   date = data_day,
                                   year = year(data_day))
          year.frame <- rbind(year.frame, temp.frame)
          data_day <- data_day + days()
        }
        out <- rbind(out, year.frame)
        date_counter <- date_counter + years()
        file_index <- file_index + 1
      } else {
        date_counter <- date_counter + years()
      }
    } else {
      file_index <- file_index + 1
    }
  }
  return(out)
}


process_monitor <- function(files){
  final <- get_final_date()
  out <- list()
  date_counter <- ymd(20000101)
  file_index <- 1
  while ((file_index <= length(files))&&(date_counter < final)) {
    if (file_ext(path) == "mat") {
      ## Extract start date from file name and insure that it matches the date counter
      ## Done to account for the fact that not all data exists for all years
      if (date_counter == ymd(substr(path, nchar(path) - 20, nchar(path) - 13))) {
        mat_result <- readMat(path)$Result
        date_counter <- date_counter + years()
        file_index <- file_index + 1
      } else {
        date_counter <- date_counter + years()
      }
    } else {
      file_index <- file_index + 1
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
#' @export
#'
gen_data_paths <- function(path = "../predictions/EPANO2") {

  # Initialize List
  file.yaml <- list()

  # get names of variables in dataset
  varlist <- yaml.load_file(file.path(path.package("airpred"),"yaml_files",
                                      "Data_Location.yml"))


  for (variable in names(varlist)) {
    directory <- file.path(path,varlist[[variable]][1])
    if (length(varlist[[variable]]) == 2) {
      files <- file.path(directory,list.files(directory,
                                              pattern = varlist[[variable]][2]))
    } else {
      ## windspeed process - ASSUMES THAT ONLY WINDSPEED VARS HAVE >2 LINES
      files <- list()
      files$uwnd <- file.path(directory,list.files(directory,
                                              pattern = varlist[[variable]][2]))
      files$vwnd <- file.path(directory,list.files(directory,
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

get_monitor_ids <- function() {
    config <- yaml.load_file("config.yml")
    monitors <- read.csv(file.path(config$Data_Location,"Location",
                                paste0(config$Monitor,"Site_North_America_Equidistant_Conic.csv")))
    return(monitors)
}
