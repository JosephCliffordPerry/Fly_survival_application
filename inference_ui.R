inference_panel <- function() {
  list(
    ui = tabPanel(
      "Inference",
      fluidPage(
        titlePanel("Run Multi-Model Inference"),
        
        sidebarLayout(
          sidebarPanel(
            # Only allow browsing inside avi_frames/
            shinyDirButton("image_dir", "Choose Image Subfolder", "Select a folder inside avi_frames"),
            
            # Filename only; results always go into statsdir/
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
      log_messages <- reactiveVal("")
      
      # Restrict roots to avi_frames/
      roots <- c(avi_frames = normalizePath(file.path(getwd(), "avi_frames")))
      
      shinyDirChoose(input, "image_dir", roots = roots, session = session)
      
      append_log <- function(msg) {
        old <- log_messages()
        log_messages(paste(old, msg, sep = "\n"))
      }
      
      output$inference_log <- renderText({
        log_messages()
      })
      
      observeEvent(input$run_inference, {
        req(input$image_dir)
        log_messages("")  # clear previous log
        
        image_dir <- parseDirPath(roots, input$image_dir)
        stats_dir <- file.path(getwd(), "statsdir")
        if (!dir.exists(stats_dir)) dir.create(stats_dir)
        stats_file <- file.path(stats_dir, input$stats_filename)
        
        model_dir <- file.path(getwd(), "models")
        model_paths <- list.files(model_dir, pattern = "\\.pt$", full.names = TRUE)
        
        append_log("🚀 Starting inference...")
        append_log(paste("Using models:", paste(basename(model_paths), collapse = ", ")))
        
        tryCatch({
          # Pass the Shiny logging function to the inference
          run_inference_multi(model_paths, image_dir, stats_file, iou_threshold = input$iou, log_fun = append_log)
        }, error = function(e) {
          append_log(paste("❌ Error:", e$message))
        })
      })
      })}
      
  

