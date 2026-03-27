# Example: Load shapefile and plot
library(mlspatial)
data("africa_shp")
data("panc_incidence")
mapdata <- inner_join(africa_shp, panc_incidence, by = "NAME")
tm_shape(mapdata) + tm_fill("incidence") + tm_borders()