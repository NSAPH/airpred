## interfaces.R
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
  if (get_transform()) {
  message("Transforming Data")
  data <- transform_all(data)
  saveRDS(data, file.path(save_path,"post_transform.RDS"))
  }

  if (get_normalize()) {
  message("Normalizing Data")
  data <- normalize_all(data)
  saveRDS(data, file.path(save_path,"post_normal.RDS"))
  }

  if (get_impute()) {
  message("Imputing Data")
  data <- h2o_impute_all(data)
  }
  saveRDS(data, file.path(save_path,"prepped.RDS"))
  return(data)
}

#' Generate training data from RDS
#'
#' Given a rds file in the config file, generates a dataframe from the file ready to use for training
#'
#' @return data frame
#' @export
get_rds_data <- function() {
  rds_path <- get_rds_location()
  save_path <- get_mid_process_location()
  data <- readRDS(rds_path)
  if (get_transform()) {
  print("Transforming Data")
  data <- transform_all(data)
  saveRDS(data, file.path(save_path,"post_transform.RDS"))
  }
  if (get_normalize()) {
  print("Normalizing Data")
  data <- normalize_all(data)
  saveRDS(data, file.path(save_path,"post_normal.RDS"))
  }
  if (get_impute()) {
  print("Imputing Data")
  data <- h2o_impute_all(data)
  }
  saveRDS(data, file.path(save_path,"prepped.RDS"))
  return(data)
}

airpred <- function() {

}
