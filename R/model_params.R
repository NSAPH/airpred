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
  func_name <- as.character(sys.call())[2] ## Assumes that the function is always the first argument
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

