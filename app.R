library(shiny)
library(shinyFiles)
library(reticulate)
library(sf)

source("avi_ui.R")
source("inference_ui.R")
source("python_inference.R")
source("avi_converter.R")
avi_panel <- avi_to_tiff_panel()
inf_panel <- inference_panel()

ui <- navbarPage(
  "Fly Survival Tools",
  avi_panel$ui,
  inf_panel$ui
)

server <- function(input, output, session) {
  avi_panel$server(input, output, session)
  inf_panel$server(input, output, session)
}

shinyApp(ui, server)

#horrendusly slow but functional inferences 

