#' ## Data of Swiss Breeding Birds

#' Bird presence-absence data from the Swiss breeding bird atlas at a 1x1 km resolution. 
#' [Schmid, H., Luder, R., Naef-Daenzer, B., Graf, R. & Zbinden, N. (1998) 
#' Schweizer Brutvogelatlas. Verbreitung der Brutvögel in der Schweiz und
#' im Fürstentum Liechtenstein 1993-1996. Swiss Ornithological Institute, Sempach, Switzerland].
#' 
#' These data were recorded over a four-year period (1993-1996) in usually 
#' three visits per year (two above the treeline) using a simplified territory 
#' mapping approach. We only considered species with at least 50 presences resulting 
#' in a total number of 56 forest bird species in the study region. 
#' Environmental predictor variables include climate, topography and vegetation structure 
#' at the same resolution as the species data. Please refer to main article and 
#' its Online Supplementary Material for description of the data. 
#' Geographic coordinates were removed.

aves <- utils::read.table("https://datadryad.org/stash/downloads/file_stream/93318", 
                              sep = ",", header = TRUE, fileEncoding = "UTF-8")
bg <- raster::raster('/vsicurl/https://damariszurell.github.io/SDM-Intro/CH_mask.tif')
bg <- as.data.frame(raster::rasterToPoints(bg))

summary(aves)

#' Data of Swiss National Forest Inventory
#' 
#' Tree species presence-absence data were obtained from the Swiss National Forest Inventory (NFI) . 
#' The NFI samples Switzerland on a regular grid (spacing 1.4 km), and in case the sample falls into 
#' forest it records forest characteristics in a maximal area of 50x50 m. 
#' We aggregated the NFI presence-absence data to 100x100 m plot size to match 
#' the minimum grain of available environmental data. 
#' We only considered species with at least 50 presences resulting in a total 
#' number of 63 tree and shrub species in the study region. 
#' Environmental predictor variables include climate, topography 
#' and vegetation structure at the same resolution as the species data. 
#' Please refer to main article and its Online Supplementary Material 
#' for description of the data. Geographic coordinates were removed.

forests <- utils::read.table("https://datadryad.org/stash/downloads/file_stream/93319", 
                          sep = ",", header = TRUE, fileEncoding = "UTF-8")
