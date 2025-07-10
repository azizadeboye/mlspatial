#' Build a tmap for a single variable
#' @param sf_data sf object with variable
#' @param var variable name as string
#' @param title legend title
#' @param palette Color palette for map
#' @return tmap object
#' @importFrom tmap tm_shape tm_fill tm_scale_intervals tm_legend tm_borders tm_compass tm_layout

#' @export
plot_single_map <- function(sf_data, var, title, palette = "reds") {
  tm_shape(sf_data) +
    tm_fill(var, fill.scale = tm_scale_intervals(
      values = paste0("brewer.", palette),
      style = "quantile"
    ), fill.legend = tm_legend(title = title)) +
    tm_borders(fill_alpha = .3) +
    tm_compass() +
    tm_layout(
      legend.text.size = 0.5, legend.position = c("left", "bottom"),
      frame = TRUE, component.autoscale = FALSE
    )
}

#' Arrange multiple tmap plots in a grid
#' @param maps List of tmap objects
#' @param ncol Number of columns
#' @export
plot_map_grid <- function(maps, ncol = 2) {
  old_mode <- tmap::tmap_mode("plot")
  res <- do.call(tmap::tmap_arrange, c(maps, list(ncol = ncol)))
  tmap::tmap_mode(old_mode)
  invisible(res)
}
