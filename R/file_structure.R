### Code to handle creating default file structures

#' Create airpred project directory structure
#'
#' @param path directory serving as the airpred project. By default, the current working directory.
#'
#' @return
#' @export
#'
#' @examples
make_default_structure <- function(path = ".") {

  dir.create(file.path(path, "training_input"))
  dir.create(file.path(path, "mid_process_data"))
  dir.create(file.path(path, "mid_process_data", "imputation_models"))
  dir.create(file.path(path, "training_output"))
  dir.create(file.path(path, "predictions"))
  dir.create(file.path(path, "predictions", "input"))
  dir.create(file.path(path, "predictions", "mid_process"))
  dir.create(file.path(path, "predictions","output"))
}
