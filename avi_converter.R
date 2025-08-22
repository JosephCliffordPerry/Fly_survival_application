Avi_to_tiff <- function(video_folder, log_fun = NULL) {
  get_ffmpeg_path <- function() {
    sysname <- Sys.info()[["sysname"]]
    if (sysname == "Windows") {
      return(file.path("ffmpeg_windows","bin","ffmpeg.exe"))
    } else if (sysname == "Darwin") {
      return(file.path("ffmpeg_mac", "ffmpeg"))
    } else {
      stop("Unsupported OS for bundled ffmpeg")
    }
  }
  
  ffmpeg_path <- get_ffmpeg_path()
  if (!file.exists(ffmpeg_path)) stop("FFmpeg not found at: ", ffmpeg_path)
  
  avi_files <- list.files(video_folder, pattern = "\\.AVI$", full.names = TRUE)
  if (length(avi_files) == 0) {
    if (!is.null(log_fun)) log_fun("No AVI files found in folder")
    return()
  }
  
  for (avi_file in avi_files) {
    base_name <- tools::file_path_sans_ext(basename(avi_file))
    output_folder <- file.path("avi_frames", paste0(base_name, "_frames"))
    dir.create(output_folder, showWarnings = FALSE)
    output_pattern <- file.path(output_folder, paste0(base_name, "_%04d.tif"))
    
    # Run ffmpeg and capture exit status
    exit_status <- system2(ffmpeg_path,
                           args = c("-i", avi_file, "-pix_fmt", "rgb24", "-color_range", "pc", output_pattern))
    
    msg <- if (exit_status == 0) {
      paste("Finished converting:", basename(avi_file))
    } else {
      paste("Failed to convert:", basename(avi_file))
    }
    
    # Send message to Shiny log function if provided
    if (!is.null(log_fun)) log_fun(msg)
  }
}
# 
# ui <- fluidPage(
#   titlePanel("Run Function on a Folder"),
#   
#   sidebarLayout(
#     sidebarPanel(
#       shinyDirButton("folder", "Choose Folder", "Select a folder from your PC"),
#       actionButton("run_btn", "Run Function", class = "btn-primary")
#     ),
#     mainPanel(
#       verbatimTextOutput("result")
#     )
#   )
# )
# 
# server <- function(input, output, session) {
#   # Reactive value to store log messages
#   log_messages <- reactiveVal("")
#   
#   # Detect OS and set roots
#   if (.Platform$OS.type == "windows") {
#     drives <- paste0(LETTERS, ":/")
#     drives <- drives[file.exists(drives)]
#     roots <- setNames(drives, drives)
#   } else {
#     roots <- c(root = "/")
#   }
#   
#   shinyDirChoose(input, "folder", roots = roots, session = session)
#   
#   output$result <- renderText({
#     log_messages()
#   })
#   
#   observeEvent(input$run_btn, {
#     req(input$folder)
#     folder_path <- parseDirPath(roots, input$folder)
#     
#     # Clear previous log
#     log_messages("")
#     
#     # Function to append log messages
#     append_log <- function(msg) {
#       old <- log_messages()
#       log_messages(paste(old, msg, sep = "\n"))
#     }
#     
#     # Run conversion
#     Avi_to_tiff(folder_path, log_fun = append_log)
#   })
# }
# 
# shinyApp(ui, server)

