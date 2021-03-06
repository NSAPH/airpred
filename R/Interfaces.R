## interfaces.R
## Interface functions between currently built systems

#' Generate training data from CSV
#'
#' Given a csv file in the config file, generates a dataframe from the file ready to use for training
#' @param init should a h2o cluster be initialized during the imputation step
#' @param shutdown should the imputation h2o cluster be shut down after imputation
#'
#' @return data frame
#' @export
#'
#' @importFrom data.table fread
#'
get_csv_data <- function(init = T, shutdown = T) {
  csv_path <- get_csv_location()
  save_path <- get_mid_process_location()
  data <- fread(csv_path)
  if (get_standardize()) {
    message("Standardizing Data")
    data <- standardize_all(data)
    saveRDS(data, file.path(save_path,"standardized.RDS"))
  }

  if (get_impute()) {
  message("Imputing Data")
  data <- h2o_impute_all(data, init = init, shutdown = shutdown)
  }
  saveRDS(data, file.path(save_path,"prepped.RDS"))
  return(data)
}

#' Generate training data from RDS
#'
#' Given a rds file in the config file, generates a dataframe from the file ready to use for training
#'
#' @param init should a h2o cluster be initialized during the imputation step
#' @param shutdown should the imputation h2o cluster be shut down after imputation
#'
#' @return data frame
#' @export
get_rds_data <- function(init = T, shutdown = T) {
  rds_path <- get_rds_location()
  save_path <- get_mid_process_location()
  data <- readRDS(rds_path)
  if (get_standardize()) {
    print("Standardizing Data")
    data <- standardize_all(data)
    saveRDS(data, file.path(save_path,"standardized.RDS"))
  }
  if (get_impute()) {
    print("Imputing Data")
    data <- h2o_impute_all(data, init = init, shutdown = shutdown)
  }
  saveRDS(data, file.path(save_path,"prepped.RDS"))
  return(data)
}

