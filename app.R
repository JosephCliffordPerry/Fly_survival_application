library(shiny)
library(shinyFiles)
library(reticulate)
library(sf)
library(ggplot2)

source("avi_ui.R")
source("inference_ui.R")
source("python_inference.R")
source("avi_converter.R")
source("graph_ui.R")   
source("browse_ui.R")
source("largest_box_panel.R")

# make Sub directories 
folders <- c("statsdir", "avi_frames", "analysisdir")
for (folder in folders) {
  if (!dir.exists(folder)) {
    dir.create(folder)
    message(paste("Created folder:", folder))
  } else {
    message(paste("Folder already exists:", folder))
  }
}

# instantiate panels
avi_panel <- avi_to_tiff_panel()
inf_panel <- inference_panel()
browse_panel <- browse_panel()
largest_panel <- largest_box_panel()
graph_panel <- graph_panel()  
# full UI
ui <- navbarPage(
  "Fly Survival Tools",
  avi_panel$ui,
  inf_panel$ui,
  browse_panel$ui,
  largest_panel$ui,
  graph_panel$ui
)

# full server
server <- function(input, output, session) {
  avi_panel$server(input, output, session)
  inf_panel$server(input, output, session)
  browse_panel$server(input, output, session)
  largest_panel$server(input, output, session)
  graph_panel$server(input, output, session)
  }



shinyApp(ui, server)

