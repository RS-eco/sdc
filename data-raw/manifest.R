# List data files, define dataset_type and write to manifest file
excluded_files <- c("data/che.rda", "data/glarus.rda")
available_datasets <- list.files("data", pattern = "\\.rda$", recursive = TRUE, full.names = TRUE)
available_datasets <- setdiff(available_datasets, excluded_files)
manifest <- tibble::tibble(
  file = available_datasets,
  dataset_type = ifelse(available_datasets %in% c("data/diva_rails_che.rda", "data/diva_roads_che.rda", "data/diva_water_areas_che.rda", "data/diva_water_lines_che.rda"), 
                        "sf", "terra"),
  label = sub("\\.rda$", "", sub(paste0("^", "data/"), "", file))
)
write.csv(manifest, "inst/manifest.csv")