% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Impute.R
\name{get_logit_weights}
\alias{get_logit_weights}
\title{Generate the weights for use in the mixed imputation model}
\usage{
get_logit_weights(info, var)
}
\arguments{
\item{info}{a data.table or tibble with variables corresponding to those contained in the
yaml files in the package.}

\item{var}{a string containing the name of the variable to be imputed}
}
\description{
Generate the weights for use in the mixed imputation model
}
