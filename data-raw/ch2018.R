#' ---
#' title: "Calculate CH2018 bioclim data for Switzerland"
#' author: "RS-eco"
#' ---

rm(list=ls()); gc()
library(raster); library(processNC); library(dplyr); 
library(lubridate); library(dismo); library(tidyr)

# Please also make sure the raster and dismo package are installed

# To run this code, you will also require the processNC package (available from Github.com/RS-eco/processNC) 
# and a Linux machine with cdo installed!

# Specify file directory
filedir <- "/home/matt/Documents/"
#filedir <- "D:/Data/"

# Load outline of Switzerland
che <- getData('GADM', country='CHE', level=0, path = paste0(filedir, "GADM"))
che <- sf::st_as_sf(che)

# Change filedir
filedir <- paste0(filedir, "CH2018")

####################

# Domain
domain <- "EUR11" # 0.11 degree

# Experiment
rcps <- c("RCP26", "RCP45", "RCP85")

# Driving model
gcms <- c("IPSL", "MPIESM")

# RCM Model
#rcm_models <- c("ARPEGE51", "CCLM4-8-17", "HIRHAM5", "RACMO22E", "RCA4", "REMO2009", "WRF331F")
rcm_models <- c("SMHI-RCA", "MPICSC-REMO2")

# Variable
vars <- c("pr", "tasmax", "tasmin")

# Time period
time_period <- "1981-2099"

#' ## Set-up combinations
combinations <- expand.grid(variable=vars, gcm=gcms, rcp=rcps, rcm=rcm_models, domain=domain, time_period=time_period)
unique(combinations$time_period)

#' ## Add time_period to combinations
combinations <- do.call("rbind", replicate(3, combinations, simplify = FALSE))
combinations$time_frame <- c(rep("1991-2020", 36), rep("2041-2070", 36), rep("2071-2100", 36))
unique(combinations$time_frame)

filenames <- combinations %>% rowwise() %>%
  do(filename = paste0("CH2018_", .$variable, "_", .$rcm, "_", .$gcm, "_", .$domain, "_", .$rcp, "_QMgrid_", .$time_period, ".nc"))
combinations$filename <- unlist(filenames$filename)

avail_combinations <- combinations[sapply(unlist(filenames$filename), function(x){x %in% list.files(filedir, pattern=".nc")}) == TRUE,]; rm(filenames)
avail_combinations$filename <- paste0(filedir, "/", avail_combinations$filename)
avail_combinations <- avail_combinations %>% arrange(filename)
head(avail_combinations$filename)

#' ## Load data and plot
#raster::plot(raster::stack(avail_combinations$filename[1], 
#                           varname=paste(avail_combinations$variable[1]))[[1]])
#plot(sf::st_geometry(che), add=T)

#' Create output files
avail_combinations$outfile <- sapply(1:nrow(avail_combinations), function(x) sub("1981-2099.nc", paste0(avail_combinations$time_frame[x], ".nc"), avail_combinations$filename[x]))
comb_files <- avail_combinations %>% ungroup() %>% dplyr::select(-c(domain, time_period)) %>% 
  group_by(variable, gcm, rcp, rcm, time_frame) %>% tidyr::nest()
comb_files$infiles <- sapply(1:nrow(comb_files), function(x){unlist(comb_files$data[[x]]$filename)})
comb_files$outfile <- sapply(1:nrow(comb_files), function(x){unlist(comb_files$data[[x]]$outfile)})

# Read in all files and process
lapply(1:nrow(comb_files), function(x){
  print(x)
  if(!file.exists(comb_files$outfile[x])){
    dat <- stack(comb_files$infiles[[x]])
    dat
    dat <- setZ(dat, as.Date(gsub("[.]", "-", sub("X", "", names(dat)))))
    date <- getZ(dat)
    
    # Subset date
    if(comb_files$time_frame[x] == "1991-2020"){
      dat <- dat[[which(lubridate::year(date) %in% c(1991:2020))]]
      comb_files$time_frame[x] <- "1991-2020"
      date <- date[which(lubridate::year(date) %in% c(1991:2020))]
    } else if(comb_files$time_frame[x] == "2041-2070"){
      dat <- dat[[which(lubridate::year(date) %in% c(2041:2070))]]
      comb_files$time_frame[x] <- "2041-2070"
      date <- date[which(lubridate::year(date) %in% c(2041:2070))]
    } else{
      dat <- dat[[which(lubridate::year(date) %in% c(2071:2100))]]
      comb_files$time_frame[x] <- "2071-2100"
      date <- date[which(lubridate::year(date) %in% c(2071:2100))]
    }
    
    if(comb_files$variable[x]== "pr"){
      ndays <- lubridate::days_in_month(getZ(dat))
      dat <- calc(dat, fun=function(x){x*ndays}) # Convert precipitation from mm day-1 to mm month-1
      dat <- setZ(dat, date)
      dat <- zApply(dat, by=month, fun=mean)
      #plot(dat)
      writeRaster(dat, filename=comb_files$outfile[x], overwrite=T, varname="pr")
    } else if(comb_files$variable[x]== "tasmin"){
      dat <- setZ(dat, date)
      dat <- zApply(dat, by=month, fun=mean)
      writeRaster(dat, filename=comb_files$outfile[x], overwrite=T, varname="tasmin")
    } else if(comb_files$variable[x]== "tasmax"){
      dat <- setZ(dat, date)
      dat <- zApply(dat, by=month, fun=mean)
      writeRaster(dat, filename=comb_files$outfile[x], overwrite=T, varname="tasmax")
    }
    rm(dat); gc()
  }
})
raster::plot(raster::stack(comb_files$outfile[1], varname="pr")[[1]])
plot(sf::st_geometry(che), add=T)

########################################

## Create bioclim files

comb_files_wide <- comb_files %>% dplyr::select(-c(data,infiles)) %>% spread(variable, outfile)

prec <- lapply(comb_files_wide$pr, stack)
tasmin <- lapply(comb_files_wide$tasmin, stack)
tasmax <- lapply(comb_files_wide$tasmax, stack)

bioclim <- lapply(1:length(prec), function(x){dismo::biovars(prec[[x]], tasmin[[x]], tasmax[[x]])})
rm(prec, tasmin, tasmax); invisible(gc())
ch2018_bioclim_che <- lapply(1:length(bioclim), function(x){as.data.frame(raster::rasterToPoints(bioclim[[x]]))}); invisible(gc())
ch2018_bioclim_che <- bind_rows(ch2018_bioclim_che, .id="id")
colnames(ch2018_bioclim_che) <- c("id", "x", "y", paste0("bio", 1:19))
comb_files_wide$id <- as.character(1:length(bioclim))
colnames(comb_files_wide)
ch2018_bioclim_che <- ch2018_bioclim_che %>% left_join(comb_files_wide, by="id") %>% 
  dplyr::select(x, y, gcm, rcm, rcp, time_frame, bio1, bio2, bio3, bio4, bio5, bio6, bio7,
                bio8, bio9, bio10, bio11, bio12, bio13, bio14, bio15, bio16, bio17, bio18, bio19) %>%
  dplyr::mutate_at(paste0("bio", 1:19), round, 2) # Round numbers for better storage performance
head(ch2018_bioclim_che)
summary(ch2018_bioclim_che)

#' Save to file
save(ch2018_bioclim_che, file="data/ch2018_bioclim_che.rda", compress="xz")
rm(ch2018_bioclim_che); gc()
