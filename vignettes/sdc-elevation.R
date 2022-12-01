## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  comment = "#>", echo=T, warning=F, message=F,
  fig.width=9, fig.height=6
)

## ----load_pkgs----------------------------------------------------------------
library(dplyr); library(ggplot2); library(sf)

## -----------------------------------------------------------------------------
# Load shapefile of Switzerland
data("che", package="sdc")

## ----suisse-------------------------------------------------------------------
data("srtm_csi_che_3arc", package="sdc")
#colnames(srtm_csi_che_3arc)

# Plot SRTM CSI altitude
srtm_csi_che_3arc %>% ggplot() + 
  geom_tile(aes(x=x, y=y, fill=altitude)) +
  geom_sf(data=che, fill="NA") + 
  scale_fill_gradientn(name="Elevation (m)", colors=terrain.colors(255)) + 
  coord_sf(expand=F) + theme_bw() + labs(x="", y="")
rm(srtm_csi_che_3arc); invisible(gc())

data("srtm3_che_3arc", package="sdc")
colnames(srtm3_che_3arc)
head(srtm3_che_3arc)

# Plot SRTM3 altitude
srtm3_che_3arc %>% ggplot() + 
  geom_tile(aes(x=x, y=y, fill=altitude)) +
  geom_sf(data=che, fill="NA") + 
  scale_fill_gradientn(name="Elevation (m)", colors=terrain.colors(255)) + 
  coord_sf(expand=F) + labs(x="", y="") + theme_bw()
rm(srtm3_che_3arc); invisible(gc())

## ----suisse_rough-------------------------------------------------------------
# Turn into ~ 9 km raster
library(raster)
data("srtm_csi_che_3arc", package="sdc")
srtm_csi_che_3arc <- rasterFromXYZ(srtm_csi_che_3arc); gc()
alt_che_mean <- aggregate(srtm_csi_che_3arc, fact=100, fun=mean, expand=T, na.rm=T)
alt_che_median <- aggregate(srtm_csi_che_3arc, fact=100, fun=median, expand=T, na.rm=T)
alt_che_min <- aggregate(srtm_csi_che_3arc, fact=100, fun=min, expand=T, na.rm=T)
alt_che_max <- aggregate(srtm_csi_che_3arc, fact=100, fun=max, expand=T, na.rm=T)
alt_che_sd <- aggregate(srtm_csi_che_3arc, fact=100, fun=sd, expand=T, na.rm=T)

alt_che_9km <- stack(alt_che_mean, alt_che_median, alt_che_min, alt_che_max, alt_che_sd)
rm(srtm_csi_che_3arc); gc()

# Turn into data.frame
alt_che_9km <- as.data.frame(rasterToPoints(alt_che_9km)); gc()
colnames(alt_che_9km) <- c("x", "y", "alt_mean", "alt_median", "alt_min", "alt_max", "alt_sd")

library(ggplot2)
alt_che_9km %>% tidyr::pivot_longer(names_to="var", values_to="val", -c(x,y)) %>% 
  filter(var %in% c("alt_mean", "alt_median", "alt_min", "alt_max")) %>%
  ggplot() + geom_tile(aes(x=x, y=y, fill=val)) + 
  facet_wrap(.~var, ncol=2) + geom_sf(data=che, fill="NA") + 
  scale_fill_gradientn(name="", colors=terrain.colors(255)) + 
  coord_sf(expand=F) + labs(x="", y="") + theme_bw()
rm(list=ls()); invisible(gc())

