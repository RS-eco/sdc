sdc: Swiss Data Cube
================

## Installation

You can install sdc from github with:

``` r
# Install remotes if not available
if (!"remotes" %in% installed.packages()[, "Package"]) install.packages("remotes")

# Install sdc package from Github
remotes::install_github("RS-eco/sdc", build_vignettes = T)
```

After installation, simply load the sdc package:

``` r
library(sdc)
```

**If you encounter a bug or if you have any problems, please file an
[issue](https://github.com/RS-eco/sdc/issues) on Github.**

## Datasets

### GADM data

Shapefiles of Canton Glarus and Switzerland can be accessed by:

``` r
data("glarus")
data("che")
```

### CH2018 data

The CH2018 climate data for Switzerland can be accessed by:

``` r
data("ch2018_bioclim_che")
```

### Euro-Cordex Data

The bioclimatic Euro-Cordex data can be accessed by:

``` r
# Euro-Cordex data for Switzerland
data("cordex_bioclim_che")
```

**Note:** Please also have a look at the corresponding
[vignette](https://github.com/RS-eco/sdc/blob/main/vignettes/sdc-euro-cordex.Rmd)
for detailed information about the dataset and the related variables.

### Corine Land-cover data

``` r
# Corine data for Switzerland
data("corine_lc_che")
```

**Note:** Please also have a look at the corresponding
[vignette](https://github.com/RS-eco/sdc/blob/main/vignettes/sdc-landcover.Rmd)
for detailed information about the dataset and the related variables.

### SRTM data

The SRTM elevation data can be accessed by:

``` r
# SRTM data for Glarus
data("srtm3_glarus_1arc")

# SRTM data for Switzerland
data("srtm_csi_che_3arc")
data("srtm3_che_3arc")

# High resolution SRTM data for whole of Switzerland
load(system.file("extdata", "srtm3_che_1arc.rda", package = "sdc"))
```

**Note:** Please also have a look at the corresponding
[vignette](https://github.com/RS-eco/sdc/blob/main/vignettes/sdc-elevation.Rmd)
for detailed information about the dataset and the related variables.
