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
#'
#' @return none
#'
#' @details
#' saves RDS files into the folder specified in the config file
#' @export
#'
#'
process_data <- function() {
  start <- Sys.time()
  path <- get_data_location()
  files <- gen_data_paths(path)
  save_path <- get_save_location()
  for (var in names(files)) {
    ##t <- Sys.time()
    ##print(file.path(save_path, paste0(var, ".RDS")))
    if (var == "MonitorData") {
     ## print("Monitor")
      df <- process_monitor(files[[var]])
     ## print(var)
      names(df)[1] <- var
      saveRDS(df, file = file.path(save_path, paste0(var,".RDS")))
    } else if (substr(var,1,20) == "REANALYSIS_windspeed") {
      ##print("wind")
      df <- process_windspeed(files[[var]])
      ##print(var)
      names(df)[1] <- var
      saveRDS(df, file = file.path(save_path, paste0(var,".RDS")))
    } else if (length(files[[var]]) == 1) {
      ##print("loc")
      df <- process_location(files[[var]])
     ## print(var)
      names(df)[1] <- var
      saveRDS(df, file = file.path(save_path, paste0(var,".RDS")))
    } else if (length(readMat(files[[var]][1])$Result[,1]) == 1) {
     ## print("year")
      df <- process_annual(files[[var]])
    ##  print(var)
      names(df)[1] <- var
      saveRDS(df, file = file.path(save_path, paste0(var,".RDS")))
    } else {
     ## print("day")
      df <- process_daily(files[[var]])
     ## print(var)
      names(df)[1] <- var
      saveRDS(df, file = file.path(save_path, paste0(var,".RDS")))
    }
   ## print(Sys.time() - t)
  }
  print("Done!")
  print(Sys.time() - start)
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

#' Process Daily Data
#'
#' @param files vector of paths to files
#'
#' @return data frame
#' @importFrom lubridate days
#'
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
  out <- data.frame(value=numeric(), site = numeric(), year = numeric(), date = numeric())
  date_counter <- ymd(20000101)
  file_index <- 1
  while ((file_index <= length(files))&&(date_counter < final)) {
    path <- files[file_index]
    ## Long condition, insures only year by year data files used
    if ((file_ext(path) == "mat") &&
        (substr(path, nchar(path) - 12, nchar(path) - 9) == substr(path, nchar(path) - 7, nchar(path) - 4))) {
      ## Extract start date from file name and insure that it matches the date counter
      ## Done to account for the fact that not all data exists for all years
      if (year(date_counter) == substr(path, nchar(path) - 12, nchar(path) - 9)) {
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

#' Join processed variables into a single data.table
#'
#' @param files used when being called from process data, should not be used when called
#' from the console. Passes in the list of variables
#'
#' @return none, but saves the processed data as an RDS file
#' @export
#'
#' @importFrom data.table data.table setkey setorder
#' @importFrom utils write.csv
join_data <- function(files = NULL) {
  save_path <- get_save_location()
  if (is.null(files)) {
    files <- gen_data_paths(get_data_location())
  }
  ## Load Monitor Data as initial data frame
  out <- readRDS(file.path(save_path, "MonitorData.RDS"))
  ## Convert to Data.Table
  out <- data.table(out)
  ## For each variable
  for (var in names(files)) {
    setkey(out, site, year, date)
    if (var != "MonitorData") {
      ## Read in DataFrame
      if (file.exists(file.path(save_path, paste0(var, ".RDS")))) {
        new_data <-
          data.table(readRDS(file.path(save_path, paste0(var, ".RDS"))))
        ## Check if annual, constant, or daily data
        if (length(names(new_data)) == 2) {
          ## Monitor Constant, join on monitor only
          print("SITE!")
          setkey(new_data, site)
          out <- merge(out, new_data, all.x = T)
        } else if (length(names(new_data)) == 3) {
          ## Annual data, join on monitor and year
          print("YEAR!")
          setkey(new_data, site, year)
          out <- merge(out, new_data, all.x = T)
        } else if (length(names(new_data)) == 4) {
          ## Daily data, join on monitor and date
          print("DAY!")
          setkey(new_data, site, year, date)
          out <- merge(out, new_data, all.x = T)
        }
      }
    }
  }
  setorder(out, date)
  saveRDS(out, file = file.path(save_path, "assembled_data.RDS"))

  write.csv(out, file = file.path(save_path, "assembled_data.csv"), row.names = F)
}
