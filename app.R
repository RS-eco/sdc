library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(arrow)
library(memoise)

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------

manifest <- read.csv("inst/manifest.csv")

# ------------------------------------------------------------------------------
# Startup resources
# ------------------------------------------------------------------------------

source("R/standardiseColumns.R")
source("R/make_plot.R")

# ------------------------------------------------------------------------------
# Utilities
# ------------------------------------------------------------------------------

load_file <- function(path) {
  env <- new.env(parent = emptyenv())
  objs <- load(path, envir = env)
  env[[objs[1]]]
}

che <- load_file("data/che.rda")

# ------------------------------------------------------------------------------
# Memoised dataset loader
# ------------------------------------------------------------------------------

load_dataset <- function(path, manifest, mtime) {
  dat <- load_file(path)
  dat <- standardiseColumns(dat)
  ds_type <- manifest$dataset_type[match(path, manifest$file)]
  if (ds_type == "sf" && !inherits(dat, "sf")) {
    dat <- sf::st_as_sf(dat, coords = c("x", "y"), crs = 4326)
  }
  dat
}

# memoise cache (in-memory; fast + safe)
cached_load_dataset <- memoise(load_dataset)

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
      selectInput("dataset", "Dataset", choices = manifest$file, selected = ""),
      uiOutput("var_selector"),
    ),
    width = 320
  ),
  
  layout_column_wrap(
    width = 1,
    
    card(
      card_header("Dataset Summary"),
      verbatimTextOutput("info")
    ),
    
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
  
  # --------------------------------------------------------------------------
  # dataset reactive (memoise replaces bindCache)
  # --------------------------------------------------------------------------
  
  data <- reactive({
    req(input$dataset)
    
    fi <- file.info(input$dataset)
    req(!is.na(fi$mtime))
    
    cached_load_dataset(
      input$dataset,
      manifest,
      fi$mtime   # important: ensures invalidation when file changes
    )
  })
  
  # --------------------------------------------------------------------------
  # variable selector
  # --------------------------------------------------------------------------
  
  output$var_selector <- renderUI({
    dat <- data()
    req(ncol(dat) > 0)
    
    geom_col <- attr(dat, "sf_column")
    
    vars <- setdiff(names(dat), c("x", "y", geom_col))
    req(length(vars) > 0)
    
    selectInput("var", "Variable", choices = vars)
  })
  
  # --------------------------------------------------------------------------
  # info panel
  # --------------------------------------------------------------------------
  
  output$info <- renderPrint({
    req(input$var)
    
    dat <- data()
    
    cat("Rows:", nrow(dat), "\n")
    cat("Columns:", ncol(dat), "\n\n")
    
    str(dplyr::select(dat, any_of(c("x", "y", input$var))))
  })
  
  # --------------------------------------------------------------------------
  # plot
  # --------------------------------------------------------------------------
  
  output$plot <- renderPlot({
    req(input$dataset, input$var)
    
    make_plot(
      dat = data(),
      var = input$var,
      outline = che
    )
  })
}

shinyApp(ui, server)