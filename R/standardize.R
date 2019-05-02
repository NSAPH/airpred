## code to handle standardization

#' Generate Data Frame with stored mean and sd
#'
#' @param info A dataframe
#' @param store should the genreated metadata be stored
#' @param load SHould the metadata be loaded instead
#'
#' @return metadata dataframe. The first row is the mean of a
#' variable and the second is the standard deviation
#' @export
#'
#' @importFrom stats sd
#'
gen_stats <- function(info, store = TRUE, load = FALSE) {
  if (load) {
    out <-  read.csv("standardize_terms.csv")
  } else {
    out <- NULL
    for (var in setdiff(names(info), c(get_site_var(), get_date_var()))) {
      if (!is.numeric(info[[var]]))  {
        next
      }
      out[[var]] <-
        c(mean(info[[var]], na.rm = T), sd(info[[var]], na.rm = T))
    }
    out <- as.data.frame(out)
    row.names(out) <- c("mean", "sd")
  }

  if (store) {
    write.csv(out, "standardize_terms.csv", row.names = F)
  }

  return(out)
}

standardize  <- function(val, the_mean, stddev) {
  out <- (val - the_mean) / stddev
  return(out)
}

destandardize <- function(val, the_mean, stddev) {
  out <- (val * stddev) + the_mean
  return(out)
}

#' Standardize Data
#'
#' standardize all variables to be z-scores
#'
#' @param info the data set to be normalized
#' @param store should the normalization values be stored
#' @param load should the normalization values be loaded
#'
#' @return data frame
#' @export
#'
standardize_all <- function(info, store = TRUE, load = FALSE) {
  stats <- gen_stats(info, store = store, load = load)
  for (var in names(info)) {
    if (!is.numeric(info[[var]]))  {
      next
    }
    info[[var]] <-
      sapply(info[[var]], standardize, the_mean = stats[[var]][1],
             stddev = stats[[var]][2])
    if (is.null(norm.terms[[var]])) {
      info[[var]] <- NULL
    }
  }
  return(info)
}

#' Destandardize Data
#'
#' convert variables from z-scores to original values
#'
#' @param info the data set to be normalized
#' @param store should the normalization values be stored
#' @param load should the normalization values be loaded
#'
#' @return data frame
#' @export
#'
destandardize_all <- function(info, store = FALSE, load = TRUE) {
  stats <- gen_stats(info, store = store, load = load)
  for (var in names(info)) {
    if (!is.numeric(info[[var]]))  {
      next
    }
    info[[var]] <-
      sapply(info[[var]], destandardize, the_mean = stats[[var]][1],
             stddev = stats[[var]][2])
    if (is.null(norm.terms[[var]])) {
      info[[var]] <- NULL
    }
  }
  return(info)
}

clean_up_stats <- function(path=".") {
  file.remove(file.path(path, "standardize_terms.csv"))
}
