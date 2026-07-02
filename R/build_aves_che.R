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
build_aves_che <- function(){
  library(rdryad)
  
  # Inspect the file metadata
  meta <- dryad_files(ids = 93318)
  names(meta$`93319`)
  meta$`93319`$path
  
  # Download that file
  library(httr)
  url <- paste0(
    "https://datadryad.org",
    meta$`93318`$`_links`$`stash:download`$href
  )
  r <- GET(url)
  stop_for_status(r)
  aves <- read.csv(text = content(r, "text", encoding = "UTF-8"))
  
  #dryad_files_download(ids = 93318)
  summary(aves)
  aves
}