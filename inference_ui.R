inference_panel <- function() {
  list(
    ui = tabPanel(
      "Inference",
      fluidPage(
        titlePanel("Run Multi-Model Inference"),
        
        sidebarLayout(
          sidebarPanel(
            shinyDirButton("image_dir", "Choose Image Subfolder", "Select a folder inside avi_frames"),
            textInput("stats_filename", "Output File Name:", "inference_results.txt"),
            numericInput("iou", "IoU Threshold:", 0.5, min = 0, max = 1, step = 0.05),
            actionButton("run_inference", "Run Inference", class = "btn-success")
          ),
          mainPanel(
            h4("Logs"),
            verbatimTextOutput("inference_log")
          )
        )
      )
    ),
    
    server = function(input, output, session) {
      roots <- c(avi_frames = normalizePath(file.path(getwd(), "avi_frames")))
      shinyDirChoose(input, "image_dir", roots = roots, session = session)
      
      output$inference_log <- renderText({
        "Click 'Run Inference' to start. Progress will be shown above."
      })
      
      observeEvent(input$run_inference, {
        req(input$image_dir)
        
        image_dir <- parseDirPath(roots, input$image_dir)
        stats_dir <- file.path(getwd(), "statsdir")
        stats_file <- file.path(stats_dir, input$stats_filename)
        
        tryCatch({
          run_inference_multi(
            image_dir, stats_file,
            iou_threshold = input$iou
          )
          output$inference_log <- renderText("✅ Inference completed successfully!")
        }, error = function(e) {
          output$inference_log <- renderText(paste("❌ Error:", e$message))
        })
      })
    }
  )
}
