#' Get shapefile of district outline
build_glarus <- function(filedir="inst/extdata"){
  # Load outline of Switzerland
  che1 <- geodata::gadm(country='CHE', level=1, path=filedir)
  che1 <- sf::st_as_sf(che1)
  glarus <- che1 |> filter(NAME_1 == "Glarus")
  glarus
}

