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
#' \dontrun{
#' rf_model <- train_rf(
#'   mapdata,
#'   incidence ~ female + male + agea + ageb + agec + fagea + fageb + fagec +
#'               magea + mageb + magec + yrb + yrc + yrd + yre,
#'   ntree = 500
#' )
#' }


#' Train XGBoost model
#'
#' Trains an XGBoost regression model.
#'
#' @name train_xgb
#' @title Train XGBoost model
#'
#' @param data A data frame with the training data.
#' @param formula A formula defining the model structure.
#' @param nrounds Number of boosting iterations.
#' @param max_depth Maximum tree depth.
#' @param eta Learning rate.
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
    verbose = 0)
}

#' @examples
#' \dontrun{
#' xgb_model <- train_xgb(
#'   mapdata,
#'   incidence ~ female + male + agea + ageb + agec + fagea + fageb + fagec +
#'               magea + mageb + magec + yrb + yrc + yrd + yre,
#'   nrounds = 100)
#'
#' summary(xgb_model)
#' }


#' Train Support Vector Regression (SVR) model
#'
#' Trains an SVR model using the radial kernel.
#'
#' @name train_svr
#' @title Train Support Vector Regression (SVR) model
#'
#' @param data A data frame containing the training data.
#' @param formula A formula specifying the model.
#'
#' @return A trained \code{svm} model object from the \pkg{e1071} package.
#'
#' @importFrom e1071 svm
#' @export
train_svr <- function(data, formula) {
  e1071::svm(formula, data = data, type = "eps-regression", kernel = "radial")
}

#' @examples
#' \dontrun{
#' svr_model <- train_svr(
#'   mapdata,
#'   incidence ~ female + male + agea + ageb + agec + fagea + fageb + fagec +
#'               magea + mageb + magec + yrb + yrc + yrd + yre
#' )
#' summary(svr_model)
#' }
