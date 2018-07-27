## grid_search.R
##
## Code implementing functionality to automate selection of
## model hyper parameters.

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

gen_grid_config <- function() {}

load_grid_config <- function(model) {
  return(yaml.load_file("grid_config.yml")$model)
}
