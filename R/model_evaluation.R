#' Get RMSE/MAE/R² metrics on training data
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

#' Plot observed vs predicted
#' @export
plot_obs_vs_pred <- function(observed, predicted, title = "") {
  ggplot2::ggplot(data.frame(obs = observed, pred = predicted),
                  aes(x = obs, y = pred)) +
    geom_point(alpha = 0.6) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    ggpubr::stat_cor(method = "pearson", aes(label = paste0("R² = "))) +
    labs(title = title, x = "Observed", y = "Predicted") +
    theme_minimal()
}
