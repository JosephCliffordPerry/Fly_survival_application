library(shiny)
library(shinyFiles)
library(reticulate)
library(sf)
library(ggplot2)
library(bslib)   
source("avi_ui.R")
source("inference_ui.R")
source("Loading_datasets.R")
source("python_inference.R")
source("avi_converter.R")
source("graph_ui.R")
source("browse_ui.R")
source("largest_box_panel.R")

# ---- Make Sub-directories ----
folders <- c("statsdir", "avi_frames", "analysisdir")
for (folder in folders) {
  if (!dir.exists(folder)) {
    dir.create(folder)
    message(paste("Created folder:", folder))
  } else {
    message(paste("Folder already exists:", folder))
  }
}

# ---- Define Theme ----
app_theme <- bs_theme(
  version = 5,
  bootswatch = "darkly",  
  primary = "#FF6600",    
)

ui <- navbarPage(
  "Fly Survival Tools",
  theme = app_theme,  
  
  # Group 1: Data Processing
  navbarMenu("Data Processing",
             avi_to_tiff_panel()$ui,
             inference_panel()$ui,
             data_load_panel()$ui
  ),
  
  # Group 2: Analysis & Browsing
  navbarMenu("Analysis & Browsing",
             browse_panel()$ui,
             largest_box_panel()$ui,
             graph_panel()$ui
  )
)

server <- function(input, output, session) {
  # Shared reactive values across modules
  df_analysis <- reactiveVal(NULL)        # holds the analysis dataframe
  frame_paths <- reactiveVal(NULL)        # holds paths to frames
  
  # ---- Module loading ----
  
  # Data processing panels
  avi_to_tiff_panel()$server(input, output, session)
  inference_panel()$server(input, output, session)
  
  # Data & Images panel gets df_analysis reactive
  data_load_panel(df_analysis,frame_paths)$server(input, output, session)
  
  # Analysis & Browsing panels
  browse_panel(df_analysis = df_analysis, frame_paths = frame_paths)$server(input, output, session)
  largest_box_panel(df_analysis = df_analysis, frame_paths = frame_paths)$server(input, output, session)
  graph_panel(df_analysis = df_analysis, frame_paths = frame_paths)$server(input, output, session)
  
  #update frame_paths when user selects a folder in data_load_panel
  observe({
    req(input$frame_folder)
    folder <- file.path("avi_frames", input$frame_folder)
    files <- list.files(folder, pattern="\\.tif$", full.names = TRUE)
    frame_paths(files[order(as.numeric(gsub(".*_(\\d+)\\.tif$", "\\1", files)))])
  })
}

shinyApp(ui, server)

