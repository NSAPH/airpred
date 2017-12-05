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
