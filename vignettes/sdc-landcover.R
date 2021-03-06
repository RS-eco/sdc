## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(comment = "#>", echo=T, warning=F, message=F, fig.width=8, fig.height=6)

## ----load_pkgs----------------------------------------------------------------
library(dplyr); library(sf); library(ggplot2)
library(tidyr); library(scico)

## -----------------------------------------------------------------------------
# Load shapefile of Europe & Switzerland
data("che", package="sdc")
che_laea <- sf::st_transform(che, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

## -----------------------------------------------------------------------------
# Named vector of clc colours
clc_cols <- c("Continuous urban fabric" = "#e6004d", "Discontinuous urban fabric" = "#ff0000", "Industrial or commercial units" = "#cc4df2", "Road and rail networks and associated land" = "#cc0000", "Port areas" = "#e6cccc", "Airports" = "#e6cce6", "Mineral extraction sites" = "#a600cc", "Dump sites" = "#a64d00", "Construction sites" = "#ff4dff", "Green urban areas" = "#ffa6ff", "Sport and leisure facilities" = "#ffe6ff", "Non-irrigated arable land" = "#ffffa8", "Permanently irrigated land" = "#ffff00", "Rice fields" = "#e6e600", "Vineyards" = "#e68000", "Fruit trees and berry plantations" = "#f2a64d", "Olive groves" = "#e6a600", "Pastures" = "#e6e64d", "Annual crops associated with permanent crops" = "#ffe6a6", "Complex cultivation patterns" = "#ffe64d", "Land principally occupied by agriculture with \n significant areas of natural vegetation" = "#e6cc4d", "Agro-forestry areas" = "#f2cca6", "Broad-leaved forest" = "#80ff00", "Coniferous forest" = "#00a600", "Mixed forest" = "#4dff00", "Natural grasslands" = "#ccf24d", "Moors and heathland" = "#a6ff80", "Sclerophyllous vegetation" = "#a6e64d", "Transitional woodland-shrub" = "#a6f200", "Beaches - dunes - sands" = "#e6e6e6", "Bare rocks" = "#cccccc", "Sparsely vegetated areas" = "#ccffcc", "Burnt areas" = "#000000", "Glaciers and perpetual snow" = "#a6e6cc", "Inland marshes" = "#a6a6ff", "Peat bogs" = "#4d4dff", "Salt marshes" = "#ccccff", "Salines" = "#e6e6ff", "Intertidal flats" = "#a6a6e6", "Water courses" = "#00ccf2", "Water bodies" = "#80f2e6", "Coastal lagoons" = "#00ffa6", "Estuaries" = "#a6ffe6", "Sea and ocean" = "#e6f2ff", "NODATA" = "#ffffff")

## -----------------------------------------------------------------------------
data("corine_lc_che", package="sdc")
corine_lc_che <- corine_lc_che %>% group_by(x,y) %>% 
  pivot_longer(cols=!c("x","y"), names_to = "year", values_to="clc"); gc()
corine_lc_che$present <- 1
corine_lc_che$year <- as.numeric(corine_lc_che$year)

clc_cols_che <- clc_cols[names(clc_cols) %in% corine_lc_che$clc]
corine_lc_che %>% filter(year == 2000) %>% 
  ggplot() + geom_tile(aes(x=x, y=y, fill=clc)) +
  scale_fill_manual(name="CLC2000", values=clc_cols_che) + 
  geom_sf(data=che_laea, fill=NA) + coord_sf() + theme_bw() + 
  theme(legend.position="bottom", legend.text = element_text(size=8)) + 
  labs(x="", y=""); invisible(gc())

## -----------------------------------------------------------------------------
lc_che <- corine_lc_che %>% filter(year == 2000) %>% 
  filter(clc %in% c("Coniferous forest", "Mixed forest", "Broad-leaved forest")) %>%
  pivot_wider(names_from="clc", values_from="present", values_fill = 0) %>%
  mutate(forest = `Coniferous forest` + `Mixed forest` + `Broad-leaved forest`); invisible(gc())
  
# Plot forest map
lc_che %>% ggplot() + geom_tile(aes(x=x, y=y, fill=as.factor(forest))) + 
  geom_sf(data=che_laea, fill=NA) + coord_sf() + 
  scale_fill_manual(name="Forest", values=c("black")) + 
  theme_bw() + labs(x="", y="") + 
  theme(legend.text = element_text(size=8))
rm(lc_che); invisible(gc())

## -----------------------------------------------------------------------------
corine_lc_che <- corine_lc_che %>% filter(year != 1990) %>% 
  filter(clc %in% c("Pastures", "Broad-leaved forest", "Coniferous forest", "Mixed forest", "Natural grasslands")); gc()
corine_lc_che %>% ggplot() + geom_tile(aes(x=x, y=y), fill="black") +
  facet_grid(clc~year) + geom_sf(data=che_laea, fill=NA) + coord_sf() + 
  theme_bw() + labs(x="", y="") + 
  theme(strip.background = element_blank(), axis.title = element_blank())

## -----------------------------------------------------------------------------
library(raster)
vars <- unique(corine_lc_che$clc)
years <- c(1990,2000,2006,2012,2018)
clc_dat <- lapply(vars, function(z){
  dat <- corine_lc_che %>% filter(clc == z) %>%
    dplyr::select(-clc) %>% group_by(x,y) %>%
    tidyr::pivot_wider(names_from="year", values_from="present") %>%
    raster::rasterFromXYZ() %>% 
    raster::aggregate(fact=50, fun=sum, expand=T, na.rm=T)
  dat <- dat/25
  return(dat)
}); rm(corine_lc_che); gc()
clc_dat[[1]]
raster::plot(clc_dat[[1]][[1]])

# Define CRS (WGS84)
clc_dat <- lapply(clc_dat, function(x){
  raster::crs(x) <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80
+towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  return(x)
})

# Turn into data.frame
clc_dat <- lapply(1:length(clc_dat), function(x){
  tryCatch({
    dat <- raster::rasterToPoints(clc_dat[[x]]) %>% as.data.frame()
    colnames(dat)
    dat$var <- vars[x]
    return(dat)
  }, error = function(e){
    return(NULL)
  })
}); invisible(gc())
clc_dat <- Filter(Negate(is.null), clc_dat)
clc_dat <- data.table::rbindlist(clc_dat, fill=T)
head(clc_dat)

# Re-structure data.frame & define categories
clc_dat <- clc_dat %>% group_by(x,y,var) %>% 
  tidyr::pivot_longer(names_to="year", values_to="perc_cover", -group_cols()) %>% 
  tidyr::drop_na() %>% mutate(year = as.numeric(sub("X", "", year))) %>% 
  mutate(var = factor(var, levels = c("Continuous urban fabric", "Discontinuous urban fabric", "Industrial or commercial units", "Road and rail networks and associated land", "Port areas", "Airports", "Mineral extraction sites", "Dump sites", "Construction sites", "Green urban areas", "Sport and leisure facilities", "Non-irrigated arable land", "Permanently irrigated land", "Rice fields", "Vineyards", "Fruit trees and berry plantations", "Olive groves", "Pastures", "Annual crops associated with permanent crops", "Complex cultivation patterns", "Land principally occupied by agriculture with significant areas of natural vegetation", "Agro-forestry areas", "Broad-leaved forest", "Coniferous forest", "Mixed forest", "Natural grasslands", "Moors and heathland", "Sclerophyllous vegetation", "Transitional woodland-shrub", "Beaches dunes sands", "Bare rocks", "Sparsely vegetated areas", "Burnt areas", "Glaciers and perpetual snow", "Inland marshes", "Peat bogs", "Salt marshes", "Salines", "Intertidal flats", "Water courses", "Water bodies", "Coastal lagoons", "Estuaries", "Sea and ocean", "NODATA", "NA"))) %>%
  tidyr::pivot_wider(names_from="year", values_from="perc_cover", values_fill = 0)
corine_lc_che_10km <- clc_dat[rowSums(clc_dat[,-c(1,2,3)], na.rm=T) != 0,]; rm(clc_dat); gc()

# Plot aggregated data
library(ggplot2)
corine_lc_che_10km %>% 
  filter(var %in% c("Olive groves", "Pastures", "Complex cultivation patterns",
                    "Agro-forestry areas", "Broad-leaved forest", "Coniferous forest", 
                    "Mixed forest", "Natural grasslands", "Beaches dunes sands")) %>%
  ggplot() + geom_tile(aes(x=x, y=y, fill=`2000`)) + 
  geom_sf(data=che_laea, fill=NA) + facet_wrap(var ~ .) + 
  scale_fill_scico(palette="roma") + coord_sf() + theme_bw() + labs(x="", y="") + 
  theme(legend.position=c(0.85,0.2), strip.background=element_blank())

## -----------------------------------------------------------------------------
data("corine_cha_che", package="sdc")
corine_cha_che <- corine_cha_che %>% 
  unite("9000", c("9000_90", "9000_00"), sep=" - ", na.rm=T) %>% 
  unite("0006", c("0006_00", "0006_06"), sep=" - ", na.rm=T) %>% 
  unite("0612", c("0612_06", "0612_12"), sep=" - ", na.rm=T) %>% 
  unite("1218", c("1218_12", "1218_18"), sep=" - ", na.rm=T)
head(corine_cha_che)  

#clc_cols_1990 <- clc_cols[names(clc_cols) %in% corine_lc_bav_tk4tel$`1990`]
corine_cha_che %>% ggplot() + geom_tile(aes (x=x, y=y, fill=`9000`)) +
  #scale_fill_manual(name="CLC 1990", values=clc_cols_1990) + 
  geom_sf(data=che_laea, fill=NA) + coord_sf() + theme_bw() + labs(x="", y="") + 
  theme(legend.position="bottom", legend.text = element_text(size=6))

## -----------------------------------------------------------------------------
rm(list=ls()); invisible(gc())

