#' Compute Moran's I & LISA, classify clusters
#' Compute spatial autocorrelation
#'
#' @param sf_data An `sf` object containing spatial features.
#' @param values A numeric vector or column name with the variable to test.
#' @param signif Logical. If TRUE, show statistically significant areas.

#' @export
compute_spatial_autocorr <- function(sf_data, values, signif = 0.05) {
  nb <- spdep::poly2nb(sf_data) %>% spdep::nb2listw(style = "W", zero.policy = TRUE)
  std <- scale(values)[,1]
  lag_val <- spdep::lag.listw(nb, std, zero.policy = TRUE)
  local <- spdep::localmoran(values, nb, zero.policy = TRUE)
  df <- sf_data %>%
    dplyr::mutate(
      val_st = std,
      lag_val = lag_val,
      Ii = local[,1],
      Z_Ii = local[,4],
      Pr_z = local[,5],
      cluster = dplyr::case_when(
        val_st > 0 & lag_val > 0 & Pr_z <= signif ~ "High-High",
        val_st < 0 & lag_val < 0 & Pr_z <= signif ~ "Low-Low",
        val_st < 0 & lag_val > 0 & Pr_z <= signif ~ "Low-High",
        val_st > 0 & lag_val < 0 & Pr_z <= signif ~ "High-Low",
        TRUE ~ "Not Significant"
      )
    )
  list(data = df, moran = spdep::moran.test(values, nb, zero.policy = TRUE))
}
