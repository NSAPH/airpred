# Normalize Data Functions
#
#


gen_norm_vals <- function(info, store = TRUE, load = FALSE) {
  # set up normalization term data.fram

  if (load) {
    ## load previously created normalization terms
    norm.terms <- readRDS("norm_terms.rds")
  } else {
    ## generate and store normalization terms
    norm.terms <- matrix(nrow = 2, ncol = ncol(info))
    norm.terms <- data.frame(norm.terms, row.names = c("max", "min"))
    names(norm.terms) <- names(info)

    for (var in names(info)) {
      norm.terms[[var]] <- c(max(info[[var]]), min(info[[var]]))
    }

    if (store) {
      saveRDS(norm.terms, file = "norm_terms.rds")
    }
  }

  return(norm.terms)
}

load_norm_vals <- function() {
 x <- readRDS('norm_terms.rds')
 return(x)

}

normalize <- function(val, max, min) {
  return((val - min)/(max - min))
}

normalize_all <- function(info, store = TRUE) {
  norm.terms <- gen_norm_vals(info, store = store)
 for (var in names(info)) {
   info[[var]] <- lapply(info[[var]], normalize, max = norm.terms[[var]][1],
                         min = norm.terms[[var]][2])
 }
  return(info)
}

denormalize <- function(val, max, min) {
  return((val*(max-min)) + min)
}

denormalize_all <- function(info, store = TRUE) {
  norm.terms <- load_norm_vals()
  for (var in names(info)) {
    info[[var]] <- lapply(info[[var]], denormalize, max = norm.terms[[var]][1],
                          min = norm.terms[[var]][2])
  }
  return(info)
}

clean_up <- function() {
  file.remove("norm_terms.rds")
}
