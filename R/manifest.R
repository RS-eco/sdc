# List data files, define dataset_type and write to manifest file
excluded_files <- c("data/che.rda", "data/glarus.rda")
available_datasets <- list.files("data", pattern = "\\.rda$", recursive = TRUE, full.names = TRUE)
available_datasets <- setdiff(available_datasets, excluded_files)
available_datasets
manifest <- tibble::tibble(
  file = available_datasets,
  dataset_type = ifelse(available_datasets %in% c("data/diva_rails_che.rda", "data/diva_roads_che.rda", "data/diva_water_areas_che.rda", "data/diva_water_lines_che.rda"), 
                        "sf", "terra"),
  filter_columns = c("gcm;rcp", "gcm;rcp", NULL, NULL,NULL,NULL,NULL,NULL,NULL),
  label = sub("\\.rda$", "", sub(paste0("^", "data/"), "", file))
)
write.csv(manifest, "inst/extdata/manifest.csv")

#
library(jsonlite)
manifest <- list(
  list(
    dataset = "iris",
    file = "iris.csv",
    filters = c("Species")
  ),
  list(
    dataset = "mtcars",
    file = "mtcars.csv",
    filters = c("cyl", "gear")
  ),
  list(
    dataset = "airquality",
    file = "airquality.csv",
    filters = character(0)
  )
)
write_json(manifest, "manifest.json", pretty = TRUE, auto_unbox = TRUE)