inference_panel <- function() {
  list(
    ui = shiny::tabPanel(
      "Inference",
      shiny::fluidPage(
        shiny::titlePanel("Run Multi-Model Inference"),

        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shinyFiles::shinyDirButton("image_dir", "Choose Image Subfolder", "Select a folder inside avi_frames"),
            shiny::textInput("stats_filename", "Output File Name (without extension):", "inference_results"),
            shiny::numericInput("iou", "IoU Threshold:", 0.5, min = 0, max = 1, step = 0.05),
            shiny::actionButton("run_inference", "Run Inference", class = "btn-success")
          ),
          shiny::mainPanel(
            shiny::h4("Logs"),
            shiny::verbatimTextOutput("inference_log")
          )
        )
      )
    ),

    server = function(input, output, session) {
      roots <- c(avi_frames = base::normalizePath(base::file.path(getwd(), "avi_frames")))
      shinyFiles::shinyDirChoose(input, "image_dir", roots = roots, session = session)

      output$inference_log <- shiny::renderText({
        "Click 'Run Inference' to start. Progress will be shown above."
      })

      shiny::observeEvent(input$run_inference, {
        shiny::req(input$image_dir)

        image_dir <- shinyFiles::parseDirPath(roots, input$image_dir)
        stats_dir <- base::file.path(getwd(), "statsdir")

        # Ensure all filenames end with _raw.txt
        base_name <- tools::file_path_sans_ext(input$stats_filename)
        stats_file <- file.path(stats_dir, paste0(base_name, "_raw.txt"))

        tryCatch({
          run_inference_multi(
            image_dir, stats_file,
            iou_threshold = input$iou
          )
          output$inference_log <- shiny::renderText("Inference completed successfully!")
        }, error = function(e) {
          output$inference_log <- shiny::renderText(base::paste("Error:", e$message))
        })
      })
    }
  )
}
