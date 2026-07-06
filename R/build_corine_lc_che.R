#' ---
#' title: "Create Corine land cover data for Switzerland"
#' author: "RS-eco"
#' ---

build_corine_lc_che <- function(filedir){
  # Downloaded from https://land.copernicus.eu/pan-european/corine-land-cover
  
  # Load packages
  library(terra); library(dplyr)
  
  # Load outline of Switzerland
  #che <- geodata::gadm(country='CHE', level=0)
  load("data/che.rda")
  
  # Load files
  clc_files <- list.files(filedir, pattern=".*_CLC.*\\.tif$", full.names=T)
  clc_dat <- terra::rast(clc_files)
  clc_dat
  
  # Re-project outline data
  che <- sf::st_transform(che, sf::st_crs(clc_dat))
  
  # Crop data
  clc_dat <- terra::mask(terra::crop(clc_dat, che), che)
  
  # Plot data
  plot(clc_dat[[1]])
  plot(sf::st_geometry(che), add=T)
  
  # Turn into data.frame
  corine_lc_che <- as.data.frame(clc_dat, xy=T)
  colnames(corine_lc_che) <- c("x", "y", 1990, 2000, 2006, 2012, 2018)

  head(corine_lc_che)
  unique(corine_lc_che$`1990`)
  unique(corine_lc_che$`2000`)
  unique(corine_lc_che$`2006`)
  unique(corine_lc_che$`2012`)
  unique(corine_lc_che$`2018`)
  
  # See labels and color code in clc_legend_qgis_raster.qml
  
  # Define categories
  #corine_lc_che <- corine_lc_che %>%
  #  mutate_at(vars(-c(x,y)), factor, levels = c(1:44, 48), 
  #            labels=c("Continuous urban fabric","Discontinuous urban fabric","Industrial or commercial units",
  #                     "Road and rail networks and associated land","Port areas","Airports","Mineral extraction sites","Dump sites",
  #                     "Construction sites","Green urban areas","Sport and leisure facilities","Non-irrigated arable land",
  #                     "Permanently irrigated land", "Rice fields", "Vineyards", "Fruit trees and berry plantations", "Olive groves",
  #                     "Pastures", "Annual crops associated with permanent crops", "Complex cultivation patterns",
  #                     "Land principally occupied by agriculture with significant areas of natural vegetation",
  #                     "Agro-forestry areas","Broad-leaved forest", "Coniferous forest", "Mixed forest", "Natural grasslands",
  #                     "Moors and heathland", "Sclerophyllous vegetation", "Transitional woodland-shrub", "Beaches dunes sands",
  #                     "Bare rocks", "Sparsely vegetated areas", "Burnt areas", "Glaciers and perpetual snow", "Inland marshes",
  #                     "Peat bogs", "Salt marshes", "Salines", "Intertidal flats", "Water courses", "Water bodies",
  #                     "Coastal lagoons", "Estuaries", "Sea and ocean", "NODATA"))
  
  library(ggplot2)
  corine_lc_che %>% ggplot() + geom_tile(aes(x=x, y=y, fill=`2000`)) + coord_sf() + 
    theme(legend.position="bottom")
  #ggsave("corine_lc_che.png", width=10, height=10, dpi=600)
  
  # Save to file
  #save(corine_lc_che, file="data/corine_lc_che.rda", compress="xz")
  corine_lc_che
}
