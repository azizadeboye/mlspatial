#' Train Random Forest
#' @export
train_rf <- function(data, formula, ntree = 500, seed = 123) {
  set.seed(seed)
  randomForest::randomForest(formula, data = data, ntree = ntree, importance = TRUE)
}

#' Train XGBoost
#' @export
train_xgb <- function(data, formula, nrounds = 100, max_depth = 4, eta = 0.1) {
  model.matrix(formula, data = data)[,-1] -> x
  label <- eval(formula[[2]], data)
  dtrain <- xgb.DMatrix(x, label = label)
  xgb::xgboost(data = dtrain, objective = "reg:squarederror",
               nrounds = nrounds, max_depth = max_depth, eta = eta, verbose = 0)
}

#' Train SVR
#' @export
train_svr <- function(data, formula) {
  e1071::svm(formula, data = data, type = "eps-regression", kernel = "radial")
}


