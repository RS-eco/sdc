# Altitude data for Switzerland

####################

## Process low resolution (90m) data

build_srtm3_glarus_1arc <- function(filedir){
  # SRTM 90m Data was downloaded from: http://srtm.csi.cgiar.org/srtmdata/
  
  # Load packages
  library(terra); library(dplyr); library(sf)
  
  # List files
  filedir <- "extdata/SRTM"
  files <- list.files(filedir, pattern="cut", full.names=T)
  
  # Load files
  alt_eur_sub <- terra::rast(files)
  
  # Define CRS (WGS84)
  terra::crs(alt_eur_sub) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  # Load Switzerland outline
  load("data/che.rda")
  
  # Crop data by extent of Switzerland
  alt_che <- terra::mask(terra::crop(alt_eur_sub, che), che)
  raster::plot(alt_che)
  plot(st_geometry(che), add=T)
  
  # Turn into data.frame
  srtm_csi_che_3arc <- as.data.frame(alt_che, xy=T)
  colnames(srtm_csi_che_3arc) <- c("x", "y", "altitude")
  srtm3_csi_che_3arc$altitude <- round(srtm3_che_3arc$altitude, digits=0)
  
  # Save to file
  save(srtm_csi_che_3arc, file="data/srtm_csi_che_3arc.rda", compress="xz")
  
  ####################
  
  ## Process high resolution (30m) data
  
  # SRTM 1ArcSec Data was downloaded from: https://dwtkns.com/srtm30m/
  
  files <- list.files("extdata/SRTMGL1/", pattern="*.hgt", full.names=T)
  
  # Load SRTM files
  alt_suisse <- lapply(files, terra::rast)
  alt_suisse <- sprc(alt_suisse)
  
  # Load Switzerland outline
  load("data/glarus.rda")
  
  # Crop data by extent of Switzerland
  srtm3_1arc_che <- terra::mask(terra::crop(alt_suisse, che), che); gc()
  
  # Merge data into one layer
  srtm3_1arc_che <-terra::merge(srtm3_1arc_che); gc()
  raster::plot(srtm3_1arc_che)
  plot(st_geometry(che), add=T)
  
  # Turn into data.frame
  srtm3_1arc_che <- as.data.frame(srtm3_1arc_che, xy=T); gc()
  colnames(srtm3_1arc_che) <- c("x", "y", "altitude")
  
  # Save to file
  #save(srtm3_che_1arc, file="inst/extdata/srtm3_che_1arc.rda", compress="xz")
  #rm(srtm3_1arc_che); gc()
  
  # Crop data by extent of Switzerland
  alt_glarus <- terra::mask(terra::crop(alt_suisse, glarus), glarus); gc()
  alt_glarus <- terra::merge(alt_glarus); rm(alt_suisse); gc()
  
  plot(alt_glarus)
  plot(sf::st_geometry(glarus), add=T)

  # Turn into data.frame
  srtm3_glarus_1arc <- as.data.frame(alt_glarus, xy=T); gc()
  colnames(srtm3_glarus_1arc) <- c("x", "y", "altitude")
  srtm3_glarus_1arc$altitude <- round(srtm3_glarus_1arc$altitude, digits=0)
  head(srtm3_glarus_1arc)
  
  # Save to file
  #save(srtm3_glarus_1arc, file="data/srtm3_glarus_1arc.rda", compress="xz")
  srtm3_glarus_1arc
}