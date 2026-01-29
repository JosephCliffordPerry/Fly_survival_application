#' Load Fly App
#'
#' Initializes and launches the Fly data Shiny application.
#'
#' This function first launches a small Shiny app that allows the user
#' to set or keep the current working directory, then launches the main
#' Fly Survival Tools application using that directory.
#'
#' @return Launches the Fly Survival Tools Shiny application.
#' @export
#'
#' @examples
#' \dontrun{
#' Load_fly_app()
#' }
Load_fly_app <- function() {
  # ---- Theme ----
  app_theme <- bslib::bs_theme(
    version = 5,
    bootswatch = "darkly",
    primary = "#FF6600"
  )
  # ================================
  # ---- STEP 1: Working Directory Selector ----
  # ================================
  get_wd_app <- function() {
    wdui <- shiny::fluidPage(
      theme = app_theme,
      tags$head(
        tags$style(HTML("
      .container-fluid {
        max-width: 600px;
      }
    "))
      ),
      shiny::titlePanel("Set Working Directory"),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::textInput("new_wd", "Enter new working directory:", value = getwd()),
          shiny::actionButton("set_wd", "Set WD and Close", class = "btn-primary"),
          shiny::actionButton("keep_wd", "Keep Previous Path and Close", class = "btn-secondary")
        ),
        shiny::mainPanel(
          shiny::verbatimTextOutput("current_wd")
        )
      )
    )
    wdserver <- function(input, output, session) {

      # Show current directory
      output$current_wd <- shiny::renderText({
        paste("Current working directory:\n", getwd())
      })

      # User chooses to set new WD
      shiny::observeEvent(input$set_wd, {
        new_dir <- input$new_wd
        if (dir.exists(new_dir)) {
          setwd(new_dir)
          shiny::showNotification(paste("Working directory set to:", new_dir), type = "message")
          shiny::stopApp(new_dir)  # Return chosen directory
        } else {
          shiny::showNotification("Directory does not exist!", type = "error")
        }
      })

      # User keeps existing WD
      shiny::observeEvent(input$keep_wd, {
        shiny::showNotification("Keeping previous working directory.", type = "message")
        shiny::stopApp(getwd())  # Return current directory
      })
    }

    shiny::shinyApp(wdui, wdserver)
  }

  # ---- Run WD selector first ----
  chosen_wd <- shiny::runApp(get_wd_app())
  setwd(chosen_wd)
  message("Working directory now set to: ", chosen_wd)

  # ================================
  # ---- STEP 2: Main Fly App ----
  # ================================

  # ---- Helper functions ----
  get_manual <- function(file) {
    path <- system.file("manuals", file, package = "flySurvivalApp")
    if (path == "" || !file.exists(path)) path <- file.path("inst", "manuals", file)
    if (!file.exists(path)) {
      message("Manual file not found: ", path)
      return(NULL)
    }
    path
  }

  safe_include_markdown <- function(path) {
    if (is.null(path) || !file.exists(path)) {
      return(shiny::HTML("<p><em>Manual not available.</em></p>"))
    }
    if (!requireNamespace("markdown", quietly = TRUE)) {
      return(shiny::HTML("<p><em>The <code>markdown</code> package is missing. Manuals cannot be displayed.</em></p>"))
    }
    shiny::includeMarkdown(path)
  }

  # ---- Create folders ----
  folders <- c("flySurvivalApp_output", "avi_frames")
  for (folder in folders) {
    if (!dir.exists(folder)) {
      dir.create(folder)
      message(paste("Created folder:", folder))
    } else {
      message(paste("Folder already exists:", folder))
    }
  }



  # ---- UI ----
  ui <- shiny::navbarPage(
    "Fly Survival Tools",
    theme = app_theme,

    shiny::navbarMenu("Data Processing",
                      avi_to_tiff_panel()$ui,
                      inference_panel()$ui,
                      data_load_panel()$ui
    ),

    shiny::navbarMenu("Analysis & Browsing",
                      browse_panel()$ui,
                      largest_box_panel()$ui,
                      graph_panel()$ui
    ),

    shiny::navbarMenu("Help",
                      shiny::tabPanel("Pipeline overview", safe_include_markdown(get_manual("help_pipeline_overview.md"))),
                      shiny::tabPanel("AVI to TIFF Converter", safe_include_markdown(get_manual("help_avi_to_tiff.md"))),
                      shiny::tabPanel("Inference", safe_include_markdown(get_manual("help_inference.md"))),
                      shiny::tabPanel("Data Loading", safe_include_markdown(get_manual("help_data_load.md"))),
                      shiny::tabPanel("Browsing", safe_include_markdown(get_manual("help_browse.md"))),
                      shiny::tabPanel("Single Pupa browsing", safe_include_markdown(get_manual("help_largest_box.md"))),
                      shiny::tabPanel("Export", safe_include_markdown(get_manual("help_graph.md"))),
                      r_console_panel()$ui
                      )
  )

  # ---- Server ----
  server <- function(input, output, session) {
    df_analysis <- shiny::reactiveVal(NULL)
    frame_paths <- shiny::reactiveVal(NULL)

    avi_to_tiff_panel()$server(input, output, session)
    inference_panel()$server(input, output, session)
    data_load_panel(df_analysis, frame_paths)$server(input, output, session)
    browse_panel(df_analysis = df_analysis, frame_paths = frame_paths)$server(input, output, session)
    largest_box_panel(df_analysis = df_analysis, frame_paths = frame_paths)$server(input, output, session)
    graph_panel(df_analysis = df_analysis, frame_paths = frame_paths)$server(input, output, session)
    r_console_panel()$server(input, output, session)

    shiny::observe({
      shiny::req(input$frame_folder)
      folder <- file.path("avi_frames", input$frame_folder)
      files <- list.files(folder, pattern = "\\.tif$", full.names = TRUE)
      frame_paths(files[order(as.numeric(gsub(".*_(\\d+)\\.tif$", "\\1", files)))])
    })
  }
  tags$script(HTML("
  Shiny.addCustomMessageHandler('reload', function(message) {
    location.reload();
  });
"))

  cat("App loaded\n")

  # ---- Launch main app ----
  return(shiny::shinyApp(ui, server))

  cat("App loaded\n")
  message("Data pipeline initialized")

}


