#' ---
#' title: "Create Corine land cover data for Switzerland"
#' author: "RS-eco"
#' ---

build_corine_cha_che <- function(filedir){
  # Downloaded from https://land.copernicus.eu/pan-european/corine-land-cover
  
  # Specify file directory
  filedir <- "/home/matt/Documents/Corine/DATA/"
  
  # Load packages
  library(raster); library(dplyr)
  
  # Load outline of Switzerland
  #che <- getData('GADM', country='CHE', level=0)
  load("data/che.rda")
  
  # Load files
  cha_files <- list.files(filedir, pattern=".*_CHA.*\\.tif$", full.names=T)
  cha_dat <- lapply(cha_files, raster::raster)
  cha_dat
  
  # Re-project outline data
  che <- sp::spTransform(che, crs(cha_dat[[1]]))
  
  cha_dat <- lapply(cha_dat, function(x) mask(crop(x, che), che))
  cha_dat <- stack(cha_dat)
  
  cha_layers <- c("9000_00", "9000_90", "0006_00", "0006_06", "0612_06", "0612_12", "1218_12", "1218_18")
  corine_cha_che <- as.data.frame(raster::rasterToPoints(cha_dat))
  colnames(corine_cha_che) <- c("x", "y", cha_layers)
  
  head(corine_cha_che)
  tail(corine_cha_che)
  corine_cha_che <- corine_cha_che %>% mutate_at(vars(-c(x,y)), function(x) factor(x, levels = c(1:44, 48, 128), 
                                                                                   labels=c("Continuous urban fabric","Discontinuous urban fabric","Industrial or commercial units",
                                                                                            "Road and rail networks and associated land","Port areas","Airports","Mineral extraction sites","Dump sites",
                                                                                            "Construction sites","Green urban areas","Sport and leisure facilities","Non-irrigated arable land",
                                                                                            "Permanently irrigated land", "Rice fields", "Vineyards", "Fruit trees and berry plantations", "Olive groves",
                                                                                            "Pastures", "Annual crops associated with permanent crops", "Complex cultivation patterns",
                                                                                            "Land principally occupied by agriculture with significant areas of natural vegetation",
                                                                                            "Agro-forestry areas","Broad-leaved forest", "Coniferous forest", "Mixed forest", "Natural grasslands",
                                                                                            "Moors and heathland", "Sclerophyllous vegetation", "Transitional woodland-shrub", "Beaches dunes sands",
                                                                                            "Bare rocks", "Sparsely vegetated areas", "Burnt areas", "Glaciers and perpetual snow", "Inland marshes",
                                                                                            "Peat bogs", "Salt marshes", "Salines", "Intertidal flats", "Water courses", "Water bodies",
                                                                                            "Coastal lagoons", "Estuaries", "Sea and ocean", "NODATA", "NA")))
  corine_cha_che <- corine_cha_che[rowSums(corine_cha_che=="NA")<8,]
  head(corine_cha_che)
  save(corine_cha_che, file="data/corine_cha_che.rda", compress="xz")
}
