#' Get shapefile of country outline
build_che <- function(filedir="extdata"){
  # Load shapefile of Switzerland
  che <- raster::getData('GADM', country='CHE', level=0, path=filedir)
  sf::st_as_sf(che)
}
