# Load targets library
library(targets)

# Define targets options
tar_option_set(
  packages = c("dplyr", "sf", "terra", "stringr", "rdryad")
)

# Define save_package_data function
save_package_data <- function(object, name) {
  save(
    list = "object",
    file = file.path("data", paste0(name, ".rda")),
    compress = "bzip2"
  )
  file.path("data", paste0(name, ".rda"))
}

# Load R functions
tar_source("R")

# Create list of targets
list(
  tar_target(biotime_che, build_biotime_che(filedir =  "/media/matt/Backup_2/EnvironmentalData/BioTime")),
  tar_target(ch2018_bioclim_che, build_ch2018_bioclim_che()),
  tar_target(che, build_che()),
  tar_target(cordex_bioclim_che, build_cordex_bioclim_che()),
  tar_target(corine_cha_che, build_corine_cha_che()),
  tar_target(corine_lc_che, build_corine_lc_che()),
  tar_target(glarus, build_glarus()),
  tar_target(inat_che_1965_2021, build_inat_che_1965_2021()),
  tar_target(srtm_csi_che_3arc, build_srtm_csi_che_3arc()),
  tar_target(srtm3_che_3arc, build_srtm3_che_3arc()),
  tar_target(srtm3_glarus_1arc, build_srtm3_glarus_1arc()),
  
  tar_target(biotime_che_rda, save_package_data(biotime_che, "biotime_che"), format = "file"),
  tar_target(ch2018_bioclim_che_rda, save_package_data(ch2018_bioclim_che, "ch2018_bioclim_che"), format = "file"),
  tar_target(che_rda, save_package_data(che, "che"), format = "file"),
  tar_target(cordex_bioclim_che_rda, save_package_data(cordex_bioclim_che, "cordex_bioclim_che"), format = "file"),
  tar_target(corine_cha_che_rda, save_package_data(corine_cha_che, "corine_cha_che"), format = "file"),
  tar_target(corine_lc_che_rda, save_package_data(corine_lc_che, "corine_lc_che"), format = "file"),
  tar_target(glarus_rda, save_package_data(glarus, "glarus"), format = "file"),
  tar_target(inat_che_1965_2021_rda, save_package_data(inat_che_1965_2021, "inat_che_1965_2021"), format = "file"),
  tar_target(srtm_csi_che_3arc_rda, save_package_data(srtm_csi_che_3arc, "srtm_csi_che_3arc"), format = "file"),
  tar_target(srtm3_che_3arc_rda, save_package_data(srtm3_che_3arc, "srtm3_che_3arc"), format = "file"),
  tar_target(srtm3_glarus_1arc_rda, save_package_data(srtm3_glarus_1arc, "srtm3_glarus_1arc"), format = "file")
)