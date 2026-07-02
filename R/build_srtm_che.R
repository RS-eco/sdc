# Altitude data for Switzerland

####################

## Process low resolution (90m) data

# SRTM 90m Data was downloaded from: http://srtm.csi.cgiar.org/srtmdata/

# Load packages
library(raster); library(dplyr)
library(sp)

# List files
filedir <- "extdata/SRTM"
files <- list.files(filedir, pattern="cut", full.names=T)

# Load files
alt_eur_sub <- raster::stack(files[1])

# Define CRS (WGS84)
raster::crs(alt_eur_sub) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Load Switzerland outline
che <- raster::getData('GADM', country='CHE', level=0)

# Crop data by extent of Switzerland
alt_che <- raster::mask(raster::crop(alt_eur_sub, che), che)
raster::plot(alt_che)
plot(che, add=T)

# Turn into data.frame
srtm_csi_che_3arc <- as.data.frame(rasterToPoints(alt_che))
colnames(srtm_csi_che_3arc) <- c("x", "y", "altitude")
srtm3_csi_che_3arc$altitude <- round(srtm3_che_3arc$altitude, digits=0)

# Save to file
save(srtm_csi_che_3arc, file="data/srtm_csi_che_3arc.rda", compress="xz")

####################

## Process high resolution (30m) data

# SRTM 1ArcSec Data was downloaded from: https://dwtkns.com/srtm30m/

files <- list.files("extdata/SRTMGL1/", pattern="*.hgt", full.names=T)

# Load SRTM files
library(raster)
alt_suisse <- lapply(files, raster::raster)

# Load Switzerland outline
load("data/che.rda")
load("data/glarus.rda")

# Crop data by extent of Switzerland
srtm3_1arc_che <- lapply(alt_suisse, function(x) mask(crop(x, che), che)); gc()

# Merge data into one layer
srtm3_1arc_che <- do.call(raster::merge, srtm3_1arc_che); gc()
#raster::plot(srtm3_1arc_che)
#plot(che, add=T)

# Turn into data.frame
srtm3_1arc_che <- as.data.frame(rasterToPoints(srtm3_1arc_che)); gc()
colnames(srtm3_1arc_che) <- c("x", "y", "altitude")

# Save to file
save(srtm3_che_1arc, file="inst/extdata/srtm3_che_1arc.rda", compress="xz")
rm(srtm3_1arc_che); gc()

alt_suisse_coarse <- lapply(alt_suisse, function(x) raster::aggregate(x, fact=c(3,3), fun=mean, expand=T, na.rm=T)); gc()

# Crop data by extent of Switzerland
alt_suisse_coarse <- lapply(alt_suisse_coarse, function(x) mask(crop(x, che), che))

# Merge data into one layer
alt_suisse_coarse <- do.call(raster::merge, alt_suisse)
raster::plot(alt_suisse_coarse)
plot(sf::st_geometry(che), add=T)

# Crop data by extent of Switzerland
alt_suisse <- lapply(alt_suisse, function(x) mask(crop(x, che), che))
alt_suisse<- do.call(raster::merge, alt_suisse)
alt_glarus <- mask(crop(alt_suisse, glarus), glarus); rm(alt_suisse); gc()

plot(alt_glarus)
plot(sf::st_geometry(glarus), add=T)

# Turn into data.frame
srtm3_che_3arc <- as.data.frame(rasterToPoints(alt_suisse_coarse))
colnames(srtm3_che_3arc) <- c("x", "y", "altitude")
srtm3_che_3arc$altitude <- round(srtm3_che_3arc$altitude, digits=0)
head(srtm3_che_3arc)

# Save to file
save(srtm3_che_3arc, file="data/srtm3_che_3arc.rda", compress="xz")

# Turn into data.frame
srtm3_glarus_1arc <- as.data.frame(rasterToPoints(alt_glarus))
colnames(srtm3_glarus_1arc) <- c("x", "y", "altitude")
srtm3_glarus_1arc$altitude <- round(srtm3_glarus_1arc$altitude, digits=0)

head(srtm3_glarus_1arc)

# Save to file
save(srtm3_glarus_1arc, file="data/srtm3_glarus_1arc.rda", compress="xz")
