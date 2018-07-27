## grid_search.R
##
## Code implementing functionality to automate selection of
## model hyper parameters.

#' run grid search for specified models
#'
#' @export
#' @importFrom h2o h2o.grid
grid_search <- function() {
  info <- readRDS(get_training_data())
  info <- as.h2o(info)

  models <- get_training_models()
  grids <- list()

  for (model in names(models)) {
    grids[[model]] <- do_grid(model, info)
  }

  return(grids)

}

do_grid <- function(model, info) {
  hypers <- load_grid_config(model)
  return(h2o.grid(model, y = "MonitorData",
                  x = setdiff(names(info), c("MonitorData", "site", "date", "year")),
                  training_frame = info,
                  hyper_params = hypers))
}


#' Generate configuration file for use with the grid search function
#'
#' @export
gen_grid_config <- function(default = T, path = ".") {
  if (default) {
    out <- yaml.load_file(file.path(path.package("airpred"), "yaml_files",
                                    "default_grid_config.yml"))

  }

  out.file <- file(file.path(path, "grid_config.yml"))
  write(as.yaml(out), file = out.file)
  close(out.file)
}

load_grid_config <- function(model) {
  return(yaml.load_file("grid_config.yml")$model)
}
