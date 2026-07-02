#' Get shapefile of district outline
build_glarus <- function(filedir="extdata"){
  # Load outline of Switzerland
  che1 <- raster::getData('GADM', country='CHE', level=1, path=filedir)
  glarus <- subset(che1, NAME_1 == "Glarus")
  glarus <- sf::st_as_sf(glarus)
  #plot(sf::st_geometry(glarus))
  
  #' Save to file
  #save(glarus, file="data/glarus.rda", compress="xz")
  glarus
}

