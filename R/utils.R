# utils.R
#' @import yaml

load_yaml <- function(path, var="base") {
  file_list <- yaml.load_file(path)

  if (!(var %in% names(file_list))) {
    var <- "base"
  }

  return(strsplit(file_list[[var]], " ")[[1]])
}
