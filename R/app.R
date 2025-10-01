#' Load Fly App
#'
#' Initializes and launches the Fly data Shiny application.
#'
#' This function sets up the user interface and server logic for the Fly application,
#' including theming, navigation, and reactive behavior.
#'
#' @return A Shiny application object. Typically launched with `shiny::runApp()` when
#'   called interactively.
#'
#' @examples
#' \dontrun{
#' Load_fly_app()
#' }
#'
#' @export
Load_fly_app <- function() {

  # # ---- Source module scripts ----
  # source("R/avi_ui.R")
  # source("R/inference_ui.R")
  # source("R/Loading_datasets.R")
  # source("R/python_inference.R")
  # source("R/avi_converter.R")
  # source("R/graph_ui.R")
  # source("R/browse_ui.R")
  # source("R/largest_box_panel.R")

  # ---- Make Sub-directories ----
  folders <- c("statsdir", "avi_frames")
  for (folder in folders) {
    if (!dir.exists(folder)) {
      dir.create(folder)
      message(paste("Created folder:", folder))
    } else {
      message(paste("Folder already exists:", folder))
    }
  }

  # ---- Define Theme ----
  app_theme <- bslib::bs_theme(
    version = 5,
    bootswatch = "darkly",
    primary = "#FF6600"
  )

  # ---- Define UI ----
  ui <- shiny::navbarPage(
    "Fly Survival Tools",
    theme = app_theme,

    # Group 1: Data Processing
    shiny::navbarMenu("Data Processing",
                      avi_to_tiff_panel()$ui,
                      inference_panel()$ui,
                      data_load_panel()$ui
    ),

    # Group 2: Analysis & Browsing
    shiny::navbarMenu("Analysis & Browsing",
                      browse_panel()$ui,
                      largest_box_panel()$ui,
                      graph_panel()$ui
    )
  )

  # ---- Define server ----
  server <- function(input, output, session) {
    # Shared reactive values across modules
    df_analysis <- shiny::reactiveVal(NULL)
    frame_paths <- shiny::reactiveVal(NULL)

    # ---- Module loading ----
    avi_to_tiff_panel()$server(input, output, session)
    inference_panel()$server(input, output, session)
    data_load_panel(df_analysis, frame_paths)$server(input, output, session)
    browse_panel(df_analysis = df_analysis, frame_paths = frame_paths)$server(input, output, session)
    largest_box_panel(df_analysis = df_analysis, frame_paths = frame_paths)$server(input, output, session)
    graph_panel(df_analysis = df_analysis, frame_paths = frame_paths)$server(input, output, session)

    # Update frame_paths when user selects a folder in data_load_panel
    shiny::observe({
      shiny::req(input$frame_folder)
      folder <- file.path("avi_frames", input$frame_folder)
      files <- list.files(folder, pattern = "\\.tif$", full.names = TRUE)
      frame_paths(files[order(as.numeric(gsub(".*_(\\d+)\\.tif$", "\\1", files)))])
    })
  }

  # ---- Launch app ----
  shiny::shinyApp(ui, server)
}
