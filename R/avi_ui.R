avi_to_tiff_panel <- function() {
  list(
    ui = shiny::tabPanel(
      "AVI -> TIFF",
      shiny::fluidPage(
        shiny::titlePanel("Convert AVI to TIFF Frames"),

        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shinyFiles::shinyDirButton("avi_folder", "Choose AVI Folder", "Select a folder with AVI files"),
            shiny::actionButton("run_avi_to_tiff", "Run Conversion", class = "btn-primary")
          ),
          shiny::mainPanel(
            shiny::h4("Logs"),
            shiny::verbatimTextOutput("avi_log")
          )
        )
      )
    ),

    server = function(input, output, session) {
      log_messages <- shiny::reactiveVal("")

      # Detect OS drives for shinyFiles
      if (.Platform$OS.type == "windows") {
        drives <- paste0(LETTERS, ":/")
        drives <- drives[file.exists(drives)]
        roots <- setNames(drives, drives)
      } else {
        roots <- c(root = "/")
      }

      shinyFiles::shinyDirChoose(input, "avi_folder", roots = roots, session = session)

      append_log <- function(msg) {
        old <- log_messages()
        log_messages(paste(old, msg, sep = "\n"))
      }

      output$avi_log <- shiny::renderText({
        log_messages()
      })

      shiny::observeEvent(input$run_avi_to_tiff, {
        shiny::req(input$avi_folder)
        folder_path <- shinyFiles::parseDirPath(roots, input$avi_folder)

        append_log(" Starting AVI -> TIFF conversion...")

        tryCatch({
          Avi_to_tiff(folder_path, log_fun = append_log)
          append_log("Conversion finished")
        }, error = function(e) {
          append_log(paste("Error:", e$message))
        })
      })
    }
  )
}
