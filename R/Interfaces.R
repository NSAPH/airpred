## Interface functions between currently built systems

#' Generate training data from CSV
#'
#' Given a csv file in the config file, generates a dataframe from the file ready to use for training
#'
#' @return data frame
#' @export
#'
#' @importFrom data.table fread
#'
get_csv_data <- function() {
  csv_path <- get_csv_location()
  save_path <- get_mid_process_location()
  data <- fread(csv_path)
  message("Transforming Data")
  data <- transform_all(data)
  saveRDS(data, file.path(save_path,"post_transform.RDS"))
  message("Normalizing Data")
  data <- normalize_all(data)
  saveRDS(data, file.path(save_path,"post_normal.RDS"))
  message("Imputing Data")
  data <- impute_all(data)
  saveRDS(data, file.path(save_path,"prepped.RDS"))
  return(data)
}

get_rds_data <- function() {
  rds_path <- get_rsd_location()
  save_path <- get_mid_process_location()
  data <- readRDS()
  message("Transforming Data")
  data <- transform_all(data)
  saveRDS(data, file.path(save_path,"post_transform.RDS"))
  message("Normalizing Data")
  data <- normalize_all(data)
  saveRDS(data, file.path(save_path,"post_normal.RDS"))
  message("Imputing Data")
  data <- impute_all(data)
  saveRDS(data, file.path(save_path,"prepped.RDS"))
  return(data)
}

## Transform + Normalize Data
