build_inat_che_1965_2025 <- function(){
  # Load rinat package
  library(rinat)
  
  # Load additional packages
  library(dplyr)
  
  # Load Switzerland shapefile
  load("data/che.rda")
  sf::st_bbox(che)
  
  ## Search by area
  bounds <- c(45.75,5.9,47.85,10.55)
  years <- c(1965:2015)
  inat_che_1965_2015 <- lapply(years, function(x){
    dat <- try(rinat::get_inat_obs(bounds = bounds, year=x, maxresults=10000) %>%
                 dplyr::select(scientific_name, datetime, place_guess, longitude, latitude,
                               common_name, species_guess, iconic_taxon_name, taxon_id)); gc()
    Sys.sleep(3)
    return(dat)
  }); gc()
  
  # Do not run in parallel with another iNat data request!!!
  
  # Years with no data.frame
  years[which(!sapply(inat_che_1965_2015, is.data.frame))]
  # Filter years with data
  inat_che_1965_2015 <- Filter(is.data.frame, inat_che_1965_2019)
  
  #' From 2016 onwards yearly data has more than 10000 results
  
  #' Create download data by month function
  inat_permonth <- function(x=1:12, year, bounds){
    dat <- lapply(1:12, function(x){
      dat <- try(rinat::get_inat_obs(bounds = bounds, year = year, month = x, maxresults = 10000) %>%
                   dplyr::select(scientific_name, datetime, place_guess, longitude, latitude,
                                 common_name, species_guess, iconic_taxon_name, taxon_id)); gc()
      Sys.sleep(3)
      return(dat)
    }); gc()
    
    # Months with no data.frame
    month.abb[which(!sapply(dat, is.data.frame))]
    
    # Filter months with data
    dat <- Filter(is.data.frame, dat)
    dat <- bind_rows(dat)
  }
  
  # Apply monthly download to various years
  inat_che_2016 <- inat_permonth(x=1:12, year=2016, bounds=c(45.75,5.9,47.85,10.55))
  inat_che_2017 <- inat_permonth(x=1:12, year=2017, bounds=c(45.75,5.9,47.85,10.55))
  inat_che_2018 <- inat_permonth(x=1:12, year=2018, bounds=c(45.75,5.9,47.85,10.55))
  inat_che_2019 <- inat_permonth(x=1:12, year=2019, bounds=c(45.75,5.9,47.85,10.55))
  inat_che_2020 <- inat_permonth(x=1:12, year=2020, bounds=c(45.75,5.9,47.85,10.55))
  inat_che_2021 <- inat_permonth(x=1:12, year=2021, bounds=c(45.75,5.9,47.85,10.55))
  inat_che_2022 <- inat_permonth(x=1:12, year=2022, bounds=c(45.75,5.9,47.85,10.55))
  inat_che_2023 <- inat_permonth(x=1:12, year=2023, bounds=c(45.75,5.9,47.85,10.55))
  inat_che_2024 <- inat_permonth(x=1:12, year=2024, bounds=c(45.75,5.9,47.85,10.55))
  inat_che_2025 <- inat_permonth(x=1:12, year=2025, bounds=c(45.75,5.9,47.85,10.55))

  # Merge lists
  inat_che_1965_2025 <- c(inat_che_1965_2015, list(inat_che_2016, inat_che_2017, inat_che_2018, inat_che_2019, inat_che_2020, 
                                                   inat_che_2021, inat_che_2022, inat_che_2023, inat_che_2024, inat_che_2025)); rm()
  
  # Turn list into data.frame
  inat_che_1965_2025 <- dplyr::bind_rows(inat_che_1965_2025)
  
  # Plot data
  #plot(inat_che_1965_2025$longitude, inat_che_1965_2025$latitude, col="red")
  #sp::plot(as(che, "Spatial"), add=T)
  #hist(lubridate::year(inat_che_1965_2025$datetime))
  
  # Save data to file
  save(inat_che_1965_2025, file="data/inat_che_1965_2025.rda", compress="xz")
  inat_che_1965_2025
}