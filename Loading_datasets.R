data_load_panel <- function(df_analysis, frame_paths) {
  list(
    ui = tabPanel(
      "Data & Images",
      fluidPage(
        titlePanel("Load Dataset and Frames"),
        sidebarLayout(
          sidebarPanel(
            checkboxInput("custom_paths", "Browse anywhere on computer", FALSE),
            uiOutput("frame_folder_ui"),
            uiOutput("stats_file_ui"),
            conditionalPanel(
              "input.custom_paths == true",
              shinyFiles::shinyDirButton("custom_frame_dir", "Choose Image Folder", "Select folder with .tif frames"),
              shinyFiles::shinyFilesButton("custom_stats_file", "Choose Stats File", "Select analysis .txt", multiple = FALSE)
            ),
            actionButton("reload_stats_btn", "Load / Reload Dataset", class = "btn-primary"),
            width = 4
          ),
          mainPanel(
            h5("Choose a frame subfolder and an analysis file, or tick 'Browse anywhere' to pick from the whole PC.")
          )
        )
      )
    ),
    
    server = function(input, output, session) {
      # Define roots for shinyFiles: user home and filesystem root
      roots <- c(Home = normalizePath("~"), Root = "/")
      
      shinyFiles::shinyDirChoose(input, "custom_frame_dir", roots = roots, session = session)
      shinyFiles::shinyFileChoose(input, "custom_stats_file", roots = roots, filetypes = c("txt"))
      
      .nz <- function(x) if (length(x)) x else ""
      
      # ---- UI for internal folders ----
      output$frame_folder_ui <- renderUI({
        req(!isTRUE(input$custom_paths))
        subs <- list.dirs("avi_frames", full.names = FALSE, recursive = FALSE)
        selectInput("frame_folder", "Frame Subfolder:", choices = .nz(subs))
      })
      
      output$stats_file_ui <- renderUI({
        req(!isTRUE(input$custom_paths))
        stats_files <- list.files("statsdir", pattern = ".txt$", full.names = FALSE)
        selectInput("stats_file", "Analysis File:", choices = .nz(stats_files))
      })
      
      # ---- Load stats helper ----
      load_stats <- function(file) {
        req(file)
        df <- read.table(file, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
        showNotification("Dataset loaded", type = "message")
        df
      }
      
      # ---- React to Reload button ----
      observeEvent(input$reload_stats_btn, {
        if (isTRUE(input$custom_paths)) {
          # Use paths chosen with shinyFiles
          frame_dir  <- shinyFiles::parseDirPath(roots, input$custom_frame_dir)
          stats_file <- shinyFiles::parseFilePaths(roots, input$custom_stats_file)$datapath
        } else {
          frame_dir  <- file.path("avi_frames", input$frame_folder)
          stats_file <- file.path("statsdir", input$stats_file)
        }
        
        req(frame_dir, stats_file)
        
        # Collect frame files and update frame_paths
        frame_files <- list.files(frame_dir, pattern = "\\.tif$", full.names = TRUE)
        frame_files <- frame_files[order(as.numeric(gsub(".*_(\\d+)\\.tif$", "\\1", frame_files)))]
        frame_paths(frame_files)
        
        # Load and assign analysis data
        df_analysis(load_stats(stats_file))
      })
    }
  )
}
