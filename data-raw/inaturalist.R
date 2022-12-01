rm(list=ls()); gc()

# Load rinat package
library(rinat)

# Load additional packages
library(dplyr)

# Load Switzerland shapefile
load("~/Documents/sdc/data/che.rda")
sf::st_bbox(che)

## Search by area
bounds <- c(45.75,5.9,47.85,10.55)
years <- c(1965:2019)
inat_che_1965_2019 <- lapply(years, function(x){
  dat <- try(get_inat_obs(bounds = bounds, year=x, maxresults=10000) %>%
               dplyr::select(scientific_name, datetime, place_guess, longitude, latitude,
                             common_name, species_guess, iconic_taxon_name, taxon_id)); gc()
  Sys.sleep(3)
  return(dat)
}); gc()

# Do not run in parallel with another iNat data request!!!

# Years with no data.frame
years[which(!sapply(inat_che_1965_2019, is.data.frame))]
# Filter years with data
inat_che_1965_2019 <- Filter(is.data.frame, inat_che_1965_2019)

# 2020 & 2021 have more than 10000 results => Split by month

inat_che_2020 <- lapply(1:12, function(x){
  dat <- try(get_inat_obs(bounds = bounds, year=2020, month=x, maxresults=10000) %>%
               dplyr::select(scientific_name, datetime, place_guess, longitude, latitude,
                             common_name, species_guess, iconic_taxon_name, taxon_id)); gc()
  Sys.sleep(3)
  return(dat)
}); gc()

# Months with no data.frame
month.abb[which(!sapply(inat_che_2020, is.data.frame))]

# Filter months with data
inat_che_2020 <- Filter(is.data.frame, inat_che_2020)

inat_che_2021 <- lapply(1:12, function(x){
  dat <- try(get_inat_obs(bounds = bounds, year=2021, month=x, maxresults=10000) %>%
               dplyr::select(scientific_name, datetime, place_guess, longitude, latitude,
                             common_name, species_guess, iconic_taxon_name, taxon_id)); gc()
  Sys.sleep(3)
  return(dat)
}); gc()

# Months with no data.frame
month.abb[which(!sapply(inat_che_2021, is.data.frame))]

# Filter months with data
inat_che_2021 <- Filter(is.data.frame, inat_che_2021)

# Merge lists
inat_che_1965_2021 <- c(inat_che_1965_2019, inat_che_2020, inat_che_2021)

# Turn list into data.frame
inat_che_1965_2021 <- bind_rows(inat_che_1965_2021)

# Plot data
plot(inat_che_1965_2021$longitude, inat_che_1965_2021$latitude, col="red")
sp::plot(as(che, "Spatial"), add=T)

hist(lubridate::year(inat_che_1965_2021$datetime))

# Save data to file
save(inat_che_1965_2021, file="data/inat_che_1965_2021.rda", compress="xz")
