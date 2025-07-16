#' Train Random Forest model
#'
#' Trains a Random Forest regression model.
#'
#' @param data A data frame containing the training data.
#' @param formula A formula describing the model structure.
#' @param ntree Number of trees to grow (default 500).
#' @param seed Random seed for reproducibility (default 123).
#'
#' @return A trained randomForest model object.
#' @export
train_rf <- function(data, formula, ntree = 500, seed = 123) {
  set.seed(seed)
  randomForest::randomForest(formula, data = data, ntree = ntree, importance = TRUE)
}

#' @examples
#' rf_model <- train_rf(
#'   mapdata,
#'   incidence ~ female + male + agea + ageb + agec + fagea + fageb + fagec +
#'               magea + mageb + magec + yrb + yrc + yrd + yre,
#'   ntree = 500
#' )


#' Train XGBoost model
#'
#' Trains an XGBoost regression model.
#'
#' @param data A data frame with the training data.
#' @param formula A formula defining the model structure.
#' @param nrounds Number of boosting iterations (default 100).
#' @param max_depth Maximum tree depth (default 4).
#' @param eta Learning rate (default 0.1).
#'
#' @return A trained xgboost model object.
#' @importFrom xgboost xgb.DMatrix xgboost
#' @export
train_xgb <- function(data, formula, nrounds = 100, max_depth = 4, eta = 0.1) {
  x <- model.matrix(formula, data = data)[, -1]
  label <- eval(formula[[2]], data)
  dtrain <- xgboost::xgb.DMatrix(x, label = label)
  xgboost::xgboost(
    data = dtrain,
    objective = "reg:squarederror",
    nrounds = nrounds,
    max_depth = max_depth,
    eta = eta,
    verbose = 0
  )
}
#' XGBoost training examples
#'
#' Examples of training an XGBoost model with `train_xgb`.
#'
#' @name train_xgb_examples
#' @examples
#' xgb_model <- train_xgb(
#'   mapdata,
#'   incidence ~ female + male + agea + ageb + agec + fagea + fageb + fagec +
#'               magea + mageb + magec + yrb + yrc + yrd + yre,
#'   nrounds = 100
#' )
NULL



#' Train Support Vector Regression (SVR) model
#'
#' Trains an SVR model with radial kernel.
#'
#' @param data A data frame containing the training data.
#' @param formula A formula specifying the model.
#'
#' @return A trained svm model object.
#' @importFrom e1071 svm
#' @export
train_svr <- function(data, formula) {
  e1071::svm(formula, data = data, type = "eps-regression", kernel = "radial")
}

#' SVR training examples
#' @name train_svr_examples
#' @examples
#' svr_model <- train_svr(
#'   mapdata,
#'   incidence ~ female + male + agea + ageb + agec + fagea + fageb + fagec +
#'               magea + mageb + magec + yrb + yrc + yrd + yre
#' )
#' summary(svr_model)
NULL

