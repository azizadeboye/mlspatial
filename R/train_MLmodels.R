#' Train Random Forest
#' Train a Random Forest model
#'
#' @param data A data frame containing the training data.
#' @param formula A formula describing the model structure.
#' @param ntree Number of trees to grow.
#' @param seed Random seed for reproducibility.

#' @export
train_rf <- function(data, formula, ntree = 500, seed = 123) {
  set.seed(seed)
  randomForest::randomForest(formula, data = data, ntree = ntree, importance = TRUE)
}

#' Train XGBoost
#' Train an XGBoost model
#'
#' @param data A data frame with the training data.
#' @param formula A formula defining the model structure.
#' @param nrounds Number of boosting iterations.
#' @param max_depth Maximum tree depth.
#' @param eta Learning rate.
#' @importFrom xgboost xgb.DMatrix
#' @importFrom xgboost xgboost

#' @export
train_xgb <- function(data, formula, nrounds = 100, max_depth = 4, eta = 0.1) {
  model.matrix(formula, data = data)[,-1] -> x
  label <- eval(formula[[2]], data)
  dtrain <- xgb.DMatrix(x, label = label)
  xgboost::xgboost(data = dtrain, objective = "reg:squarederror",
               nrounds = nrounds, max_depth = max_depth, eta = eta, verbose = 0)
}

#' Train SVR
#' Train a Support Vector Regression model
#'
#' @param data A data frame containing the training data.
#' @param formula A formula specifying the model.

#' @export
train_svr <- function(data, formula) {
  e1071::svm(formula, data = data, type = "eps-regression", kernel = "radial")
}


