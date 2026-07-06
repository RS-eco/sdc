#' Get shapefile of country outline
build_che <- function(filedir="inst/extdata"){
  # Load shapefile of Switzerland
  che <- geodata::gadm(country='CHE', level=0, path=filedir)
  sf::st_as_sf(che)
}
