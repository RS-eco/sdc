# ------------------------------------------------------------------------------
# Load packages
# ------------------------------------------------------------------------------

library(shiny)
library(bslib)
library(dplyr)
library(sf)
library(ggplot2)
library(cachem)
library(memoise)
library(future)
library(promises)

#options(shiny.error = browser, shiny.fullstacktrace = TRUE)
plan(multisession)

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------

manifest <- read.csv("inst/extdata/manifest.csv", stringsAsFactors=FALSE)
manifest$filter_columns <- lapply(
  manifest$filter_columns,
  function(x) {
    if (is.na(x) || x == "")
      return(NULL)
    
    strsplit(x, ";")[[1]]
  }
)

# ------------------------------------------------------------------------------
# Startup resources
# ------------------------------------------------------------------------------

source("R/standardiseColumns.R")
source("R/make_plot.R")

# ------------------------------------------------------------------------------
# Define load dataset function
# ------------------------------------------------------------------------------

load_dataset <- function(path, manifest, mtime) {
  env <- new.env(parent = emptyenv())
  objs <- load(path, envir = env)
  if (length(objs) == 0) {
    stop("No objects were loaded from: ", path)
  }
  dat <- env[[objs[[1]]]]
  dat <- standardiseColumns(dat)
  ds_type <- manifest$dataset_type[match(path, manifest$file)]
  if (ds_type == "sf" && !inherits(dat, "sf")) {
    dat <- sf::st_as_sf(dat, coords = c("x", "y"), crs = 4326)
  }
  dat
}

# ------------------------------------------------------------------------------
# Use a disk cache instead of the default memory cache
# ------------------------------------------------------------------------------

library(cachem)
dataset_cache <- cache_disk(
  dir = "cache",
  max_size = 5 * 1024^3   # 5 GB
)

cached_load_dataset <- memoise(
  load_dataset,
  cache = dataset_cache
)

plot_cache <- cache_disk("plot_cache")
cached_plot <- memoise(make_plot, cache = plot_cache)

# ------------------------------------------------------------------------------
# Define function to load outline
# ------------------------------------------------------------------------------

outline <- function() {
  env <- new.env(parent = emptyenv())
  objs <- load("data/che.rda", envir = env)
  env[[objs[[1]]]]
}

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------

ui <- page_sidebar(
  title = "Swiss Data Cube",
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#00695c"
  ),
  
  sidebar = sidebar(
    tagList(
      selectInput("dataset", "Dataset", choices = manifest$label, selected = manifest$label[1]),
      uiOutput("var_selector"),
      uiOutput("filter_selector"),
    ),
    width = 320
  ),
  
  layout_column_wrap(
    width = 1,
    
    card(
      card_header("Dataset Summary"),
      verbatimTextOutput("info")
    ),
    
    uiOutput("loading"),
    
    card(
      full_screen = TRUE,
      plotOutput("plot", height = "700px")
    )
  )
)

# ------------------------------------------------------------------------------
# Server
# ------------------------------------------------------------------------------

server <- function(input, output, session) {
  
  load_dataset_task <- ExtendedTask$new(function(path) {
    future_promise({
      fi <- file.info(path)
      cached_load_dataset(
        path,
        manifest,
        fi$mtime
      )
    }, seed = NULL)
  })
  
  dataset_info <- reactive({
    manifest[manifest$label == input$dataset, ]
  })
  
  # --------------------------------------------------------------------------
  # dataset reactive (memoise replaces bindCache)
  # --------------------------------------------------------------------------
  
  observeEvent(dataset_info(), {
    req(nrow(dataset_info()) == 1)
    load_dataset_task$invoke(dataset_info()$file)
  })
  
  # --------------------------------------------------------------------------
  # variable selector
  # --------------------------------------------------------------------------
  
  output$var_selector <- renderUI({
    dat <- req(load_dataset_task$result())
    req(ncol(dat) > 0)
    
    geom_col <- attr(dat, "sf_column")
    
    vars <- setdiff(names(dat), c("x", "y", geom_col))
    filters <- dataset_info()$filter_columns[[1]]
    vars <- vars[!vars %in% filters]
    req(length(vars) > 0)
    
    selectInput("var", "Variable", choices = vars)
  })
  
  # --------------------------------------------------------------------------
  # filter selector
  # --------------------------------------------------------------------------
  
  output$filter_selector <- renderUI({
    
    filters <- dataset_info()$filter_columns[[1]]
    if (is.null(filters))
      return(NULL)
    
    dat <- req(load_dataset_task$result())
    tagList(
      lapply(filters, function(col) {
        selectInput(
          inputId = paste0("filter_", col),
          label = col,
          choices = sort(unique(dat[[col]])),
          selected = dat[[col]][1]
        )
      })
    )
  })
  
  # -------------------------------------------------------------------------- 
  # loading indicator 
  # -------------------------------------------------------------------------- 
  
  output$loading <- renderUI({ 
    if (load_dataset_task$status() == "running") { 
      div( class = "alert alert-info", 
           "Loading dataset..." ) 
    } 
  })
  
  # -------------------------------------------------------------------------- 
  # filter data
  # -------------------------------------------------------------------------- 
  
  filtered_data <- reactive({
    dat <- req(load_dataset_task$result())
    req(input$var)
    cols <- dataset_info()$filter_columns[[1]]
    if (is.null(cols))
      return(dat)
    
    for (col in cols) {
      values <- input[[paste0("filter_", col)]]
      
      if (length(values) > 0)
        dat <- dat[dat[[col]] %in% values, ]
    }
    
    dat
  })
  
  # --------------------------------------------------------------------------
  # info panel
  # --------------------------------------------------------------------------
  
  output$info <- renderPrint({
    dat <- req(load_dataset_task$result())
    req(input$var)
    
    cat("Rows:", nrow(dat), "\n")
    cat("Columns:", ncol(dat), "\n\n")
    
    str(dplyr::select(dat, any_of(c("x", "y", input$var))))
  })
  
  # --------------------------------------------------------------------------
  # plot
  # --------------------------------------------------------------------------
  
  output$plot <- renderPlot({ 
    req(input$var)
    cached_plot(dat = filtered_data(), var = input$var, outline = outline) 
  })
}

shinyApp(ui, server)