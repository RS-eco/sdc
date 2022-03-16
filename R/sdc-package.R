#' R Package to access Swiss climate, elevation and land-use data
#' 
#' @name sdc package
#' @aliases sdcpackage
#' @docType package
#' @title R Package to access Swiss climate, elevation and land-use data
#' @author RS-eco
#'
#' @importFrom dplyr left_join %>%
#' @keywords package
#'
NULL
#' @docType data
#' @name ch2018_bioclim_che
#' @title Bioclimatic data of Switzerland from CH2018 data
#' @description Bioclimatic data for current and future conditions in Switzerland
#' @details This dataset contains bioclimatic data for Switzerland under 
#' 2 global circulation models (GCMs; MPI-M-MPI-ESM-LR & IPSL-IPSL-CM5A-MR), 
#' two regional climate models (RCMs; MPI-CSC-REMO2009 & SMHI-RCA4) and 
#' 3 representative concentration pathways (RCPs; RCP2.6, RCP4.5, RCP8.5) 
#' for current (1991-2020) and future conditions (2041-2070, 2071-2100).
#' @format A \code{data.frame} with 198240 observations and 25 variables.
NULL
#'
#' @docType data
#' @name che
#' @title administrative boundary of Switzerland
#' @description GADM outline of Switzerland
#' @usage data(che)
#' @details Administrative boundary of Switzerland derived from GADM data.
#' @format A \code{data.frame} with 1 observation and 3 variables.
#' @source This data has been obtained from: \itemize{\item \url{https://www.gadm.org/data.html}}
NULL
#'
#' @docType data
#' @name cordex_bioclim_che
#' @title Euro-cordex bioclimatic data of Switzerland
#' @description Euro-cordex bioclimatic data of Switzerland
#' @usage data(cordex_bioclim_che)
#' @details Euro-cordex climate simulation data of Switzerland.
#' The data.frame contains bioclimatic data for 3 time periods (1991-2020, 2041-2070, 2071-2100) 
#' under 3 representative concentration pathways (rcps; RCP2.6, RCP4.5, RCP8.5), 
#' 5 global circulation models (gcms; MPI-M-MPI-ESM-LR, CNRM-CERFACS-CNRM-CM5, 
#' MOHC-HadGEM2-ES, ICHEC-EC-EARTH, IPSL-IPSL-CM5A-MR), 5 regional climate models (rcms; CLMcom-CCLM4-8-17, 
#' KNMI-RACMO22E, MPI-CSC-REMO2009, SMHI-RCA4, DMI-HIRHAM5), 
#' 4 ensemble (r1i1p1, r2i1p1, r3i1p1, r12i1p1) and 3 rs (v1, v1a, v2).
#' @format A \code{data.frame} with 8484 observations and 27 variables.
NULL
#'
#' @docType data
#' @name corine_cha_che
#' @title Corine land cover change data of Switzerland
#' @description Corine land cover change data of Switzerland
#' @usage data(corine_cha_che)
#' @details Corine land cover change data of Switzerland. 
#' Corine Land cover change are land cover change maps for Europe mapped at a resolution of 100 x 100 m.
#' @format A \code{data.frame} with 15634 observations and 10 variables.
#' @source This data has been obtained from: \itemize{\item \url{https://land.copernicus.eu/pan-european/corine-land-cover}}
NULL
#'
#' @docType data
#' @name corine_lc_che
#' @title Corine land cover data of Switzerland
#' @description Corine land cover data of Switzerland
#' @usage data(corine_lc_che)
#' @details Corine land cover data of Switzerland, Germany. 
#' Corine Land cover are land cover maps for Europe mapped at a resolution of 100 x 100 m.
#' @format A \code{data.frame} with 4124593 observations and 7 variables.
#' @source This data has been obtained from: \itemize{\item \url{https://land.copernicus.eu/pan-european/corine-land-cover}}
NULL
#'
#' @docType data
#' @name glarus
#' @title administrative boundary of Canton Glarus, Switzerland
#' @description GADM outline of Canton Glarus, Switzerland
#' @usage data(che)
#' @details Administrative boundary of Canton Glarus, Switzerland derived from GADM data.
#' @format A \code{data.frame} with 1 observation and 11 variables.
#' @source This data has been obtained from: \itemize{\item \url{https://www.gadm.org/data.html}}
NULL
#'
#' @docType data
#' @name inat_che_1965_2021
#' @title iNaturalist data of Switzerland
#' @description iNatrualist data of Switzerland for 1962 - 2022
#' @usage data(inat_che_1965_2021)
#' @details iNatrualist occurrence data of Switzerland for 1962 - 2022.
#' @format A \code{data.frame} object with xxx observations and 9 variables.
#' @source This data has been obtained through the rinat package from: 
#' \itemize{\item \url{https://www.inaturalist.org/}}
NULL
#'
#'
#' @docType data
#' @name srtm_csi_che_3arc
#' @title srtm data of Switzerland
#' @description NASA SRTM v3.0 elevation data of Switzerland
#' @usage data(srtm_csi_che_3arc)
#' @details Elevation data of Switzerland derived from NASA SRTM 3 Arc Sec v3.0 data.
#' @format A \code{data.frame} with 6999059 observations and 3 variables.
#' @source This data has been obtained from: \itemize{\item \url{http://srtm.csi.cgiar.org/srtmdata/}}
NULL
#'
#' @docType data
#' @name srtm3_che_3arc
#' @title srtm data of Switzerland
#' @description SRTM elevation data of Switzerland
#' @usage data(srtm3_che_3arc)
#' @details Elevation data of Switzerland derived from NASA SRTM 1 Arc Sec data and aggregated to 3 Arc Sec resolution.
#' @format A \code{data.frame} with 6999059 observations and 3 variables.
#' @source This data has been obtained from: \itemize{\item \url{https://dwtkns.com/srtm30m/}}
NULL
#'
#' @docType data
#' @name srtm3_glarus_1arc
#' @title srtm data of Glarus, Switzerland
#' @description NASA SRTM v3.0 elevation data of Glarus, Switzerland
#' @usage data(srtm3_glarus_1arc)
#' @details Elevation data of Glarus, Switzerland derived from NASA SRTM 1 Arc Sec data.
#' @format A \code{data.frame} with 1060719 observations and 3 variables.
#' @source This data has been obtained from: \itemize{\item \url{https://dwtkns.com/srtm30m/}}
NULL