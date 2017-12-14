## Train Code

train_nn <- function() {}


train_forest <- function() {}


train_gradboost <- function() {}

train <- function() {
  h2o.init()
  ## Do Whatever
  h2o.shutdown()
}


implemented_models <- function() {
  return(c("nn", "forest", "gradboost"))
}
