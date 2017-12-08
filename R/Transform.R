## Transformation code

detransform <- function(val, xmin, xmax, xmean, x20, x80) {
  k <- (x80 - x20)/(2*atanh(0.8))
  output <- (xmax - xmin)/(2*tanh((val - xmean)/k)) + (xmax + xmin)/2
  return(output)
}

transform <- function(val, xmin, xmax, xmean, x20, x80) {
  k <- (x80 - x20)/(2*atanh(0.8))
  output <- xmean + k*atanh(2*(val - (xmin+xmax)/2)/(xmax-xmin))
  return(output)
}

transform_all <- function(info, store = TRUE, load = FALSE) {
  transform_terms <- gen_norm_vals(info, store = store, load = load)
  transform_vars <- load_yaml(file.path(path.package("airpred"), "yaml_files", "transform_vars.yml"))
  for (var in names(info)) {
    for (term in transform_vars) {
      if (grepl(term, var)) {
        info[[var]] <- sapply(info[[var]], transform, xmin = transform_terms[[var]][2],
                              xmax = transform_terms[[var]][1], xmean = transform_terms[[var]][3],
                              x20 = transform_terms[[var]][4], x80 = transform_terms[[var]][5])
        message(var)
        message(class(info[[var]]))
        break
      }
    }
  }

  return(info)
}

detransform_all <- function(info, store = TRUE, load = FALSE) {
  transform_terms <- load_norm_vals()
  transform_vars <- load_yaml(file.path(path.package("airpred"), "yaml_files", "transform_vars.yml"))
  for (var in names(info)) {
    for (term in transform_vars) {
      if (grepl(term, var)) {
        info[[var]] <- sapply(info[[var]], detransform, xmin = transform_terms[[var]][2],
                              xmax = transform_terms[[var]][1], xmean = transform_terms[[var]][3],
                              x20 = transform_terms[[var]][4], x80 = transform_terms[[var]][5])
        break
      }
    }
  }

  return(info)
}
