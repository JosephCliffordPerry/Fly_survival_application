avi_to_tiff_panel <- function() {
  list(
    ui = tabPanel(
      "AVI → TIFF",
      fluidPage(
        titlePanel("Convert AVI to TIFF Frames"),
        
        sidebarLayout(
          sidebarPanel(
            shinyDirButton("avi_folder", "Choose AVI Folder", "Select a folder with AVI files"),
            actionButton("run_avi_to_tiff", "Run Conversion", class = "btn-primary")
          ),
          mainPanel(
            h4("Logs"),
            verbatimTextOutput("avi_log")
          )
        )
      )
    ),
    
    server = function(input, output, session) {
      log_messages <- reactiveVal("")
      
      # Detect OS drives for shinyFiles
      if (.Platform$OS.type == "windows") {
        drives <- paste0(LETTERS, ":/")
        drives <- drives[file.exists(drives)]
        roots <- setNames(drives, drives)
      } else {
        roots <- c(root = "/")
      }
      
      shinyDirChoose(input, "avi_folder", roots = roots, session = session)
      
      append_log <- function(msg) {
        old <- log_messages()
        log_messages(paste(old, msg, sep = "\n"))
      }
      
      output$avi_log <- renderText({
        log_messages()
      })
      
      observeEvent(input$run_avi_to_tiff, {
        req(input$avi_folder)
        folder_path <- parseDirPath(roots, input$avi_folder)
        
        log_messages("🚀 Starting AVI → TIFF conversion...")
        
        future::future({
          Avi_to_tiff(folder_path, log_fun = append_log)
        }) %...>% {
          append_log("✅ Conversion finished")
        } %...!% {
          append_log(paste("❌ Error:", .))
        }
      })
    }
  )
}
