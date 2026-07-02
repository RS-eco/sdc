#' Additional package to get shapefile of country outlines
library(dplyr); library(sf)

# Load shapefile of Switzerland
che <- raster::getData('GADM', country='CHE', level=0, path="extdata")
che <- sf::st_as_sf(che)

#' Save to file
save(che, file="data/che.rda", compress="xz")

# Load outline of Switzerland
che1 <- getData('GADM', country='CHE', level=1, path="extdata")
glarus <- subset(che1, NAME_1 == "Glarus")
glarus <- sf::st_as_sf(glarus)
plot(sf::st_geometry(glarus))

#' Save to file
save(glarus, file="data/glarus.rda", compress="xz")
