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

build_forests_che <- function(){
  library(rdryad)
  #install.packages(
  #  "rdryad",
  #  repos = c(
  #    ropensci = "https://ropensci.r-universe.dev",
  #    CRAN = "https://cloud.r-project.org"
  #  )
  #)
  
  # Inspect the file metadata
  meta <- dryad_files(ids = 93319)
  names(meta$`93319`)
  meta$`93319`$path
  
  library(httr)
  url <- paste0(
    "https://datadryad.org",
    meta$`93319`$`_links`$`stash:download`$href
  )
  r <- GET(url)
  stop_for_status(r)
  forests <- read.csv(text = content(r, "text", encoding = "UTF-8"))
  
  # Download that file
  #dryad_download(dois = "10.5061/dryad.k88v330")
  #dryad_files_download(ids = 93119)
  #summary(forests)
  #forests
}
