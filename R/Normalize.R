# normalize.R
# Normalize Data Functions
#
#


#' Generate Data Frame with stored metadata
#'
#' @param info A dataframe
#' @param store should the genreated metadata be stored
#' @param load SHould the metadata be loaded instead
#'
#' @return metadata dataframe
#' @export
#'
#' @importFrom stats quantile
#'
gen_norm_vals <- function(info, store = TRUE, load = FALSE) {
  # set up normalization term data.frame
  # Also generates values for transformation

  if (load) {
    ## load previously created normalization terms
    norm.terms <- readRDS("norm_terms.rds")
  } else {
    ## generate and store normalization terms
    norm.terms <- matrix(nrow = 7, ncol = ncol(info))
    norm.terms <- data.frame(norm.terms, row.names = c("max", "min", "mean", "20%","80%", "1%","99%"))
    names(norm.terms) <- names(info)

    for (var in names(info)) {
      if (!(var %in% c("site","year","date")) && is.numeric(info[[var]][1])) {
      norm.terms[[var]] <- c(max(info[[var]], na.rm = T),
                             min(info[[var]], na.rm = T),
                             mean(info[[var]], na.rm = T),
                             quantile(info[[var]],0.2, na.rm = T),
                             quantile(info[[var]],0.8, na.rm = T),
                             quantile(info[[var]],0.01, na.rm = T),
                             quantile(info[[var]],0.99, na.rm = T))
    }}

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

#' Normalize Data
#'
#' @param info the data set to be normalized
#' @param store should the normalization values be stored
#' @param load should the normalization values be loaded
#'
#' @return data frame
#' @export
#'
normalize_all <- function(info, store = TRUE, load = FALSE) {
  norm.terms <- gen_norm_vals(info, store = store, load = load)
 for (var in names(info)) {
   if (!(var %in% c("site","year","date")) && is.numeric(info[[var]][1])) {
     info[[var]] <- sapply(info[[var]], normalize, max = norm.terms[[var]][1],
                           min = norm.terms[[var]][2])
     if (is.null(norm.terms[[var]])) {
       info[[var]] <- NULL
     }
   }
  }
  return(info)
}

denormalize <- function(val, max, min) {
  return((val*(max-min)) + min)
}

denormalize_all <- function(info, store = TRUE) {
  norm.terms <- load_norm_vals()
  for (var in names(info)) {
    if (!(var %in% c("site","year","date")) && is.numeric(info[[var]][1])) {
    info[[var]] <- lapply(info[[var]], denormalize, max = norm.terms[[var]][1],
                          min = norm.terms[[var]][2])
  }}
  return(info)
}

clean_up_norm <- function() {
  file.remove("norm_terms.rds")
}
