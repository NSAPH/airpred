## config.R
## Functions related to handling the config file

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
#'
#' The following are the items contained in the config file. All of them must be present in order for
#' the model to run successfully.
#'
#' \itemize{
#'   \item{\code{impute}} {A boolean. If TRUE, airpred will generate imputations for
#'                         specified variables}
#'   \item{\code{impute_vars}} {Either "default" or a path to a yaml file listing the variables to be
#'                              imputed. If a custom file is being used, the top level of the
#'                              yaml file should be "base" with the elements of "base" being the
#'                              names of the variables being imputed.}
#'   \item{\code{impute_formula}} {Either "default" or a path to a yaml file listing the variables to be
#'                              used as inputs to the imputation model. If a custom file is being used, the top level of the
#'                              yaml file should be "base" or the name of a variable being imputed with the elements being the
#'                              names of the variables being used for imputation.}
#'   \item{\code{transform}} {A boolean. If TRUE, airpred will perform transformations on
#'                         specified variables}
#'   \item{\code{normalize}} {{A boolean. If TRUE, airpred will perform normalizations
#'                         on all variables}}
#'   \item{\code{finalday}} {The date of the last day covered by the data set}
#'   \item{\code{csv_path}} {The path where the assembled data is stored as a csv}
#'   \item{\code{rds_path}} {The path where the assembled data is stored as an rds file}
#'   \item{\code{date_var}} {The name of the variable containing date identification}
#'   \item{\code{site_var}} {the name of the variable containing site identification}
#'   \item{\code{output_var}} {The name of the variable that the model is trying to predict}
#'   \item{\code{imputation_models}} {The path where the imputation models should be saved.}
#'   \item{\code{mid_process_data}} {The path where data should be saved between imputation, normalization
#'                                   and transformation steps}
#'   \item{\code{training_models}} {A list of the models to be used in training and used for the
#'                                  ensemble model.}
#'   \item{\code{two_stage}} {Should the two stage modeling process be implemented?}
#'   \item{\code{monitor_list}} {The location of the file containing the coordinates of the monitors}
#'   \item{\code{training_data}} {The file containing transformed and imputed code to be used for training.
#'                                 Currently must be an RDS file.}
#'   \item{\code{training_output}} {The directory to be used for storing the output of the training models}
#'   \item{\code{predict_data}} {The input data for a given round of prediction}
#'   \item{\code{predict_mid_process}} {The directory that holds all saved files
#'                                      generated in the prediction process.}
#'   \item{\code{predict_output}} {The directory that holds the generated predictions}
#'   \item{\code{pre_generated_weights}} {A boolean determining whether or not the spatial weights
#'                                       are stored on disk or need to be generated on the fly from
#'                                       the list of monitors}
#'    \item{\code{weight_matrix_path}} {The path to where pregenerated weights are stored}
#'  }
gen_config <- function(default = TRUE, path = ".", in_list = NULL) {
  if (default) {
    out <- yaml.load_file(file.path(path.package("airpred"),"yaml_files",
                                    "Config_Default.yml"))
  } else {
    out <- list()
    out$impute <- TRUE
    out$transform <- TRUE
    out$normalize <- TRUE
    out$csv_path <- ""
    out$monitor_list <- ""
    out$pre_generated_weights <- FALSE
    out$weight_matrix_path <- ""
    out$imputation_models <- ""
    out$mid_process_data <- ""
    out$training_data <- ""
    out$training_output <- ""
    out$training_models <- c("nn", "forest", "gradboost")
    out$two_stage <- TRUE
    out$predict_data <- ""
    out$predict_mid_process <- ""
    out$predict_output <- ""
    out$date_var <- ""
    out$site_var <- ""
    out$output_var <- "MonitorData"
    out$impute_vars <- "default"
  }

  if (!is.null(in_list)) {
    ## Need to validate input here
    for (item in names(in_list)) {
      out[[item]] <- in_list[[item]]
    }
  }

  out.file <- file(file.path(path, "config.yml"))
  write(as.yaml(out), file=out.file)
  close(out.file)

}


get_monitor_list <- function() {
  if (!file.exists("config.yml")) {
    stop("No config file found, try running gen_config()")
  }
  return(yaml.load_file("config.yml")$monitor_list)
}


get_csv_location <- function() {
  if (!file.exists("config.yml")) {
    stop("No config file found, try running gen_config()")
  }
  return(yaml.load_file("config.yml")$csv_path)
}

get_rds_location <- function() {
  if (!file.exists("config.yml")) {
    stop("No config file found, try running gen_config()")
  }
  return(yaml.load_file("config.yml")$rds_path)
}

get_impute_location <- function() {
  if (!file.exists("config.yml")) {
    stop("No config file found, try running gen_config()")
  }
  return(yaml.load_file("config.yml")$imputation_models)
}

get_mid_process_location <- function() {
  if (!file.exists("config.yml")) {
    stop("No config file found, try running gen_config()")
  }
  return(yaml.load_file("config.yml")$mid_process_data)
}

get_training_data <- function() {
  if (!file.exists("config.yml")) {
    stop("No config file found, try running gen_config()")
  }
  return(yaml.load_file("config.yml")$training_data)
}

get_training_output <- function() {
  if (!file.exists("config.yml")) {
    stop("No config file found, try running gen_config()")
  }
  return(yaml.load_file("config.yml")$training_output)
}

get_training_models <- function() {
  if (!file.exists("config.yml")) {
    stop("No config file found, try running gen_config()")
  }
  out <- list()
  possible <- implemented_models()
  models <- yaml.load_file("config.yml")$training_models
  for (mod in models) {
    if (!(mod %in% names(possible))) {
      message(paste0(mod, " is not currently implemented as a training model"))
      message("A custom parameter file will be generated and the generic function
              will be attempted. No promises though.")
      possible[[mod]] <- mod
    }
    out[[mod]] <- possible[[mod]]
  }

  return(out)
}

get_predict_data <- function() {
  if (!file.exists("config.yml")) {
    stop("No config file found, try running gen_config()")
  }

  return(yaml.load_file("config.yml")$predict_data)
}

get_predict_mid_process <- function() {
  if (!file.exists("config.yml")) {
    stop("No config file found, try running gen_config()")
  }

  return(yaml.load_file("config.yml")$predict_mid_process)
}

get_predict_output <- function() {
  if (!file.exists("config.yml")) {
    stop("No config file found, try running gen_config()")
  }

  return(yaml.load_file("config.yml")$predict_output)
}

get_two_stage <- function() {
  if (!file.exists("config.yml")) {
    stop("No config file found, try running gen_config()")
  }

  return(yaml.load_file("config.yml")$two_stage)
}

get_impute <- function() {
  if (!file.exists("config.yml")) {
    stop("No config file found, try running gen_config()")
  }

  return(yaml.load_file("config.yml")$impute)
}

get_transform <- function() {
  if (!file.exists("config.yml")) {
    stop("No config file found, try running gen_config()")
  }

  return(yaml.load_file("config.yml")$transform)
}

get_normalize <- function() {
  if (!file.exists("config.yml")) {
    stop("No config file found, try running gen_config()")
  }

  return(yaml.load_file("config.yml")$normalize)
}

get_date_var <- function() {
  if (!file.exists("config.yml")) {
    stop("No config file found, try running gen_config()")
  }

  return(yaml.load_file("config.yml")$date_var)
}

get_site_var <- function() {
  if (!file.exists("config.yml")) {
    stop("No config file found, try running gen_config()")
  }

  return(yaml.load_file("config.yml")$site_var)
}

get_output_var <- function() {
  if (!file.exists("config.yml")) {
    stop("No config file found, try running gen_config()")
  }

  return(yaml.load_file("config.yml")$output_var)
}

get_impute_var_path <- function() {
  if (!file.exists("config.yml")) {
    stop("No config file found, try running gen_config()")
  }

  return(yaml.load_file("config.yml")$impute_vars)
}

get_impute_formula_path <- function() {
  if (!file.exists("config.yml")) {
    stop("No config file found, try running gen_config()")
  }

  return(yaml.load_file("config.yml")$impute_formula)
}

#' Remove current config file
#'
#' @return none
#' @export
#'
clean_up_config <- function(path = "config.yml") {
  file.remove(path)
}


#' Print a config file's contents to the console
#'
#' @param path the path to the config file you want to print.
#'
#' @export
display_config <- function(path = "config.yml") {
  print(cat(readLines(path), sep = "\n"))
}

