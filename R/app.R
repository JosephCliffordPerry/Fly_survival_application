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

  # ---- Helper functions ----

  # Get path to a manual file
  # Safely finds manual files whether the package is installed or run from source.
  get_manual <- function(file) {
    path <- system.file("manuals", file, package = "flySurvivalApp")
    if (path == "" || !file.exists(path)) {
      path <- file.path("inst", "manuals", file)
    }
    if (!file.exists(path)) {
      message("Manual file not found: ", path)
      return(NULL)
    }
    return(path)
  }

  # Safe wrapper for including markdown
  safe_include_markdown <- function(path) {
    if (is.null(path) || !file.exists(path)) {
      return(shiny::HTML("<p><em>Manual not available.</em></p>"))
    }
    if (!requireNamespace("markdown", quietly = TRUE)) {
      return(shiny::HTML("<p><em>The <code>markdown</code> package is missing. Manuals cannot be displayed.</em></p>"))
    }
    shiny::includeMarkdown(path)
  }

  # ---- Make Sub-directories ----
  folders <- c("flySurvivalApp_output", "avi_frames")
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
    ),

    # Group 3: Help / Manuals
    shiny::navbarMenu("Help",
                      shiny::tabPanel("AVI to TIFF Converter", safe_include_markdown(get_manual("help_avi_to_tiff.md"))),
                      shiny::tabPanel("Inference", safe_include_markdown(get_manual("help_inference.md"))),
                      shiny::tabPanel("Data Loading", safe_include_markdown(get_manual("help_data_load.md"))),
                      shiny::tabPanel("Browsing", safe_include_markdown(get_manual("help_browse.md"))),
                      shiny::tabPanel("Largest Box Detection", safe_include_markdown(get_manual("help_largest_box.md"))),
                      shiny::tabPanel("Graph Analysis", safe_include_markdown(get_manual("help_graph.md")))
    )
  )

  # ---- Define server ----
  server <- function(input, output, session) {
    df_analysis <- shiny::reactiveVal(NULL)
    frame_paths <- shiny::reactiveVal(NULL)

    # ---- Module loading ----
    avi_to_tiff_panel()$server(input, output, session)
    inference_panel()$server(input, output, session)
    data_load_panel(df_analysis, frame_paths)$server(input, output, session)
    browse_panel(df_analysis = df_analysis, frame_paths = frame_paths)$server(input, output, session)
    largest_box_panel(df_analysis = df_analysis, frame_paths = frame_paths)$server(input, output, session)
    graph_panel(df_analysis = df_analysis, frame_paths = frame_paths)$server(input, output, session)

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
