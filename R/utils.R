# utils.R
#' @import yaml

load_yaml <- function(path, var="base") {
  file_list <- yaml.load_file(path)

  if (!(var %in% names(file_list))) {
    var <- "base"
  }

  return(strsplit(file_list[[var]], " ")[[1]])
}

Vector2FormulaString <- function(strvec) {
  if (length(strvec)==1) {
    return(strvec)
  }
  else {
      out <- strvec[1]
      for (string in strvec[2:length(strvec)]) {
        out <- paste(out, string, sep = "+")
      }
  }
  return(out)
}

get_formula<- function(path, var="base") {
  out <- load_yaml(path, var)
  out <- Vector2FormulaString(out)
  return(out)
}



