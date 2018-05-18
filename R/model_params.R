# model_params.R
# code to handle passing tuning parameters
#

#' Generate a config file for a given function
#'
#' @param func the function for which a config file is being generated
#' @param path the path in which to store the config file
#'
#' @details
#' This is used to generate config files for functions not specially implemented
#' within the airpred. A yaml file with the name "[func]_params.yml" will be
#' created in your current directory. The file will have its upper level be all of the
#' tuning arguments for a given function while the lower level will contain the default
#' values.
#'
#' NOTE: The function MUST be the first argument passed, otherwise the yaml file will not
#' be appropriately named, and may end up in an unintended location.
#'
#' @export
gen_model_config <- function(func, path = ".") {
  ## handle functions passed as strings
  if (is.character(func)) {
    func_name <- func
    func <- eval(as.name(func))
  } else {
    func_name <- as.character(sys.call())[2] ## Assumes that the function is always the first argument
  }
  model_args <- formals(func)
  out <- list()
  for (val in names(model_args)) {
    if (class(model_args[[val]]) == "name") {
      ## Do Nothing
    } else if (class(model_args[[val]]) == "call") {
      out[[val]] <- eval(model_args[[val]])
    } else if (is.null(model_args[[val]])) {
      out[[val]] <- ""
    } else {
      out[[val]] <- model_args[[val]]
    }
  }

  out.file <- file(file.path(path, paste0(func_name,"_params.yml")))
  write(as.yaml(out), file=out.file)
  close(out.file)

}

#' Generate config files for all training models
#'
#' @param path the directory that will store the generated config files
#'
#' @export
#' @seealso gen_model_config
edit_params <- function(path = ".") {
  models <- get_training_models()
  for (model in names(models)) {
    if (model %in% implemented_models()) {
      default_model_config(model, path)
    } else {
      gen_model_config(model, path)
    }
  }
}

#' @describeIn gen_model_config generates config file for specifically implemented models
#' @export
default_model_config <- function(model, path = ".") {
  out <- yaml.load_file(file.path(path.package("airpred"),"yaml_files",
                                  paste0(model,"_default_params.yml")))

  out.file <- file(file.path(path, paste0(model,"_params.yml")))
  write(as.yaml(out), file=out.file)
  close(out.file)

}

#'@export
get_model_param <- function(model, param, path = ".") {
  return(yaml.load_file(file.path(path, paste0(model,"_params.yml")))[[param]])
}

#' Check to see if model config files are in use
#'
#' @return boolean
param_config_check <- function(path = ".") {
  models <- get_training_models()
  for (name in models) {
    if (!file.exists(file.path(path, paste0(name,"_params.yml")))) {
      return(FALSE)
    }
  }

  return(TRUE)
}

