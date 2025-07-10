#' Get RMSE/MAE/R² metrics on training data
#' #' Evaluate Model Performance
#'
#' @param model A trained model
#' @param data A data frame
#' @param formula A formula object
#' @param model_type Character: "rf", or "xgb", "svr"
#'
#' @importFrom ggplot2 aes geom_point geom_abline labs theme_minimal
#' @importFrom dplyr %>%
#' @importFrom xgboost xgb.DMatrix
#' @importFrom stats model.matrix predict
#' @export
eval_model <- function(model, data, formula, model_type = c("rf", "xgb", "svr")) {
  model_type <- match.arg(model_type)
  if (model_type == "xgb") {
    mm <- model.matrix(formula, data)
    preds <- predict(model, newdata = mm)
  } else {
    preds <- predict(model, newdata = data)
  }
  obs <- eval(formula[[2]], data)
  caret::postResample(preds, obs)
}


# Declare known global variables to suppress R CMD check NOTE
utils::globalVariables(c("obs", "pred"))

#' Plot observed vs predicted
#'
#' @param observed A numeric vector of observed values
#' @param predicted A numeric vector of predicted values
#' @param title A string for the plot title
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_abline labs theme_minimal
#' @importFrom ggpubr stat_cor
#' @export
plot_obs_vs_pred <- function(observed, predicted, title = "") {
  ggplot2::ggplot(data.frame(obs = observed, pred = predicted),
                  aes(x = obs, y = pred)) +
    geom_point(alpha = 0.6) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    ggpubr::stat_cor(method = "pearson", aes(label = paste0("R^2 = "))) +
    labs(title = title, x = "Observed", y = "Predicted") +
    theme_minimal()
}
