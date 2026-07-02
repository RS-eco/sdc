# Altitude data for Switzerland

####################

## Process low resolution (90m) data
build_srtm3_che_3arc <- function(filedir){
  # SRTM 90m Data was downloaded from: http://srtm.csi.cgiar.org/srtmdata/
  
  # Load packages
  library(terra); library(dplyr); library(raster)
  
  # List files
  filedir <- "extdata/SRTM"
  files <- list.files(filedir, pattern="cut", full.names=T)
  
  # Load files
  alt_eur_sub <- terra::rast(files[1])
  
  # Define CRS (WGS84)
  terra::crs(alt_eur_sub) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  # Load Switzerland outline
  load("data/che.rda")
  
  # Crop data by extent of Switzerland
  alt_che <- terra::mask(terra::crop(alt_eur_sub, che), che)
  terra::plot(alt_che)
  plot(che, add=T)
  
  # Turn into data.frame
  srtm_csi_che_3arc <- as.data.frame(rasterToPoints(alt_che)); rm(alt_che)
  colnames(srtm_csi_che_3arc) <- c("x", "y", "altitude")
  srtm3_csi_che_3arc$altitude <- round(srtm3_che_3arc$altitude, digits=0)
  
  # Save to file
  #save(srtm_csi_che_3arc, file="data/srtm_csi_che_3arc.rda", compress="xz")
  srtm_csi_che_3arc
}