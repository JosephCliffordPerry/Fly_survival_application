library(shiny)
library(shinyFiles)
library(reticulate)
library(sf)
library(ggplot2)
library(DT)

source("avi_ui.R")
source("inference_ui.R")
source("python_inference.R")
source("avi_converter.R")
source("graph_ui.R")   
source("browse_ui.R")

# instantiate panels
avi_panel <- avi_to_tiff_panel()
inf_panel <- inference_panel()
graph_panel <- graph_panel()  
browse_panel<-browse_panel()
# full UI
ui <- navbarPage(
  "Fly Survival Tools",
  avi_panel$ui,
  inf_panel$ui,
  graph_panel$ui,
  browse_panel$ui
)

# full server
server <- function(input, output, session) {
  avi_panel$server(input, output, session)
  inf_panel$server(input, output, session)
  graph_panel$server(input, output, session)
  browse_panel$server(input, output, session)
}

shinyApp(ui, server)

