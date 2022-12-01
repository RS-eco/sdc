## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, warning=F, message=F, fig.width=8, fig.height=6)

## -----------------------------------------------------------------------------
rm(list=ls()); gc()

# Load packages
library(dplyr); library(tidyr)
library(ggplot2); library(scico)

# Load edc package
library(sdc)

# Load shapefile of Switzerland
data("che", package="sdc")

## -----------------------------------------------------------------------------
# Install mecofun package if not available
if(!"mecofun" %in% installed.packages()[,"Package"]){
  remotes::install_git("https://gitup.uni-potsdam.de/macroecology/mecofun.git")
}

# Load mecofun package
library(mecofun)
#' This package includes the following functions:
#' predictSDM, crossvalSDM, evalSDM, TSS, expl_deviance, inflated_response
#' eo_mask, partial_response, range_size, range_centre, select07, select07_cv

