#' ## Extract biotime data for Switzerland
build_biotime_che <- function(filedir){
  # Available for download from here: https://biotime.st-andrews.ac.uk/download_form.php?dl=csv_download
  
  #' Please cite as detailed below when using BioTIME:
  
  #' Dornelas M, Antão LH, Moyes F, Bates, AE, Magurran, AE, et al. 
  #' BioTIME: A database of biodiversity time series for the Anthropocene. 
  #' Global Ecol Biogeogr. 2018; 27:760 - 786. https://doi.org/10.1111/geb.12729

  #download.file("https://biotime.st-andrews.ac.uk/download_form.php?dl=csv_download")
  biotime <-read.csv(paste0(filedir, "/biotime_v1/BioTIMEQuery_24_06_2021.csv")) #### use the latest version from download site ####
  #' Alternatively, you can load the query from the ZENODO repository
  #fullquery<-read.csv(url(https://zenodo.org/record/1095627))
  
  #' Maria Dornelas, Laura H. Antão, Amanda E. Bates, Viviana Brambilla, Jonathan M. Chase, Cher F. Y. Chow, 
  #' Ada Fontrodona-Eslava, Anne E. Magurran, Inês S. Martins, Faye Moyes, Alban Sagouis, et al. 
  #' BioTIME 2.0: Expanding and Improving a Database of Biodiversity Time Series. 
  #' Global Ecol Biogeogr. 2025; 34(5):e70003. https://doi.org/10.1111/geb.70003

  biotime2 <- readRDS(paste0(filedir, "/biotime_v2_full_2025/biotime_v2_full_2025.rds")); gc() 
  
  # Subset locations by extent of Switzerland
  library(dplyr)
  biotime <- biotime %>% filter(LONGITUDE >= 5 & LONGITUDE <= 11) %>% filter(LATITUDE >= 45 & LATITUDE <= 48)
  biotime2 <- biotime2 %>% filter(LONGITUDE >= 5 & LONGITUDE <= 11) %>% filter(LATITUDE >= 45 & LATITUDE <= 48); gc()
  biotime_che <- full_join(biotime, biotime2); rm(biotime, biotime2); gc()
  biotime_che
}