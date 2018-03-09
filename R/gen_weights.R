# gen_weights.R

#' Generate geographic weighting matrix
#'
#' Given two sets of points, generate a matrix of weights for the k nearest neighbors
#' for use in distance weighted averaging of values.
#'
#' @param orig the data in which the neighbors are being found
#' @param query the data for which the neighbors need to be found
#' @param k the number of neighbors being searched for
#' @param threshold the maximum distance for a neighbor to be given a weight
#' @param term the power that the distance is raised to when the weight matrix
#'     is generated.
#'
#' @importFrom RANN nn2
#' @export
gen_weights <- function(orig = load_site_list(), query = load_site_list(), k=4, threshold = 100000, term = 1) {
    neighbors <- nn2(data = orig, query = query, k = k)

    weight_matrix <- process_weights(neighbors, nrow(orig), nrow(query), threshold, term)

    return(weight_matrix)
}


process_weights <- function(neighbors, orig_len, query_len, threshold, term = 1) {
  out <- matrix( data = 0, nrow = query_len, ncol = orig_len)

  for (i in 1:query_len) {
    for (j in 1:ncol(neighbors$nn.idx)) {
      if (neighbors$nn.dists[i,j] <= threshold && neighbors$nn.dists[i,j] != 0) {
        out[i,neighbors$nn.idx[i,j]] <- 1/(neighbors$nn.dists[i,j]^term)
      }
    }
  }

  return(out)
}

load_site_list <- function() {
  site_list <- get_monitor_list()
  if (file_ext(site_list) == "mat") {
    out <- read_mat_sitelist(site_list)
  } else if (file_ext(site_list) == "csv") {
    out <- read_csv_sitelist(site_list)
  } else {
    stop('The site list provided is not of a supported format.')
  }

  return(out)
}

read_mat_sitelist <- function(filename) {
   sites <- readMat(filename)$Result
   return(sites)
}

read_csv_sitelist <- function(filename) {
  sites <- fread(filename)
  sites$SiteCode <- NULL
  return(sites)
}
