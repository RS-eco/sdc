#' ## Extract GBIF data for Switzerland

build_gbif_che <- function(filedir){
  # Load libraries
  library(dplyr); library(raster); library(ggplot2)
  
  # Get GADM boundaries
  load("data/che.rda")
  gadm <- che
  
  # Connect to GBIF database
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname =paste0(filedir,"/gbif_database.sqlite"))
  gbif <- tbl(con, "gbif")
  
  # Collect data from GBIF database
  minlat <- floor(min(gadm$lat))
  maxlat <- ceiling(max(gadm$lat))
  minlong <- floor(min(gadm$long))
  maxlong <- ceiling(max(gadm$long))
  
  gbif_che <- gbif %>% filter(decimallatitude >= minlat & decimallatitude <= maxlat) %>% 
    filter(decimallongitude >= minlong & decimallongitude <= maxlong) |> collect() |>
    data.frame()
  
  
  # Disconnect database
  DBI::dbDisconnect(con); rm(gbif, con); gc()
  
  # Drop some variables
  gbif_che <- dplyr::select(gbif_che, -c(datasetkey, occurrenceid, eventdate, depth, 
                                 depthaccuracy, typestatus, issue))
  gbif_che
}
