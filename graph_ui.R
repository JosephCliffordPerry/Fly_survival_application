library(shiny)
library(tiff)
library(openxlsx)
library(lubridate)

graph_panel <- function() {
  list(
    ui = tabPanel(
      "Graph Inferences",
      fluidPage(
        titlePanel("Analyze Bounding Boxes from Analysis File"),
        sidebarLayout(
          sidebarPanel(
            uiOutput("frame_folder_ui3"),           # select folder
            uiOutput("analysis_file_ui3"),          # select _analysis file
            sliderInput("chamber_split", "Chamber Split:", min = 0, max = 1000, value = 500, step = 1, width = "100%"),
            
            h4("Left Side Info"),
            textInput("left_treatment", "Treatment", ""),
            numericInput("left_dose", "Dose", 0),
            textInput("left_dose_unit", "Dose Unit", "mM"),
            textInput("left_genotype", "Genotype", "wild-type"),
            numericInput("left_replicate", "Replicate", 1),
            
            h4("Right Side Info"),
            textInput("right_treatment", "Treatment", ""),
            numericInput("right_dose", "Dose", 0),
            textInput("right_dose_unit", "Dose Unit", "mM"),
            textInput("right_genotype", "Genotype", "wild-type"),
            numericInput("right_replicate", "Replicate", 1),
            
            actionButton("render_frame", "Render Frame", class = "btn-success"),
            actionButton("export_excel", "Export Excel", class = "btn-primary"),
            width = 4
          ),
          mainPanel(
            h4("Frame Preview"),
            plotOutput("frame_plot2", height = "600px")
          )
        )
      )
    ),
    
    server = function(input, output, session) {
      
      # --- Frame folder selection ---
      output$frame_folder_ui3 <- renderUI({
        req(dir.exists("avi_frames"))
        folders <- list.dirs("avi_frames", full.names = FALSE, recursive = FALSE)
        selectInput("frame_folder", "Frame Folder:", choices = folders)
      })
      
      # --- Select only _analysis files ---
      output$analysis_file_ui3 <- renderUI({
        req(input$frame_folder)
        stats_files <- list.files("statsdir", pattern = "_analysis\\.txt$", full.names = FALSE)
        selectInput("analysis_file", "Select Analysis File:", choices = stats_files)
      })
      
      # --- Read selected analysis file ---
      df_analysis <- reactive({
        req(input$analysis_file)
        df <- read.table(file.path("statsdir", input$analysis_file), header = TRUE, sep = "\t")
        df$frame_num <- as.numeric(df$frame_num)
        df
      })
      
      # --- Reactive trigger for frame rendering ---
      frame_trigger <- reactiveVal(0)
      observeEvent(input$render_frame, {
        frame_trigger(frame_trigger() + 1)
      })
      
      # --- Render frame ---
      output$frame_plot2 <- renderPlot({
        req(df_analysis())
        frame_trigger()  # reactive to button click
        
        # Get first TIFF file in folder
        folder_path <- file.path("avi_frames", input$frame_folder)
        frame_file <- list.files(folder_path, full.names = TRUE, pattern = "\\.tif$", ignore.case = TRUE)[1]
        req(!is.null(frame_file) && file.exists(frame_file))
        
        img <- tiff::readTIFF(frame_file, as.is = TRUE)
        h <- dim(img)[1]; w <- dim(img)[2]
        
        par(xaxs="i", yaxs="i")
        plot(NA, xlim=c(0,w), ylim=c(h,0), xlab="", ylab="", axes=FALSE, asp=1)
        rasterImage(img, 0, h, w, 0, interpolate=FALSE)
        
        # Draw vertical line live with slider
        abline(v=input$chamber_split, col="blue", lwd=2, lty=2)
      })
      
      # --- Export Excel ---
      observeEvent(input$export_excel, {
        req(df_analysis())
        df <- df_analysis()
        
        # Only first appearance of each ID
        first_appearance <- aggregate(frame_num ~ id, data = df, FUN = min)
        df_first <- df[df$frame_num %in% first_appearance$frame_num, ]
        
        # Assign side based on chamber_split
        vline_x <- input$chamber_split
        df_first$side <- ifelse(
          (df_first$x1 + df_first$x2 + df_first$x3 + df_first$x4)/4 <= vline_x,
          "Left", "Right"
        )
        
        # Assign user inputs per side
        df_first$Treatment <- ifelse(df_first$side=="Left", input$left_treatment, input$right_treatment)
        df_first$Dose <- ifelse(df_first$side=="Left", input$left_dose, input$right_dose)
        df_first$Dose_unit <- ifelse(df_first$side=="Left", input$left_dose_unit, input$right_dose_unit)
        df_first$Genotype <- ifelse(df_first$side=="Left", input$left_genotype, input$right_genotype)
        df_first$Replicate <- ifelse(df_first$side=="Left", input$left_replicate, input$right_replicate)
        
        # Compute Date/Time from frame_num (5-min blocks)
        start_datetime <- ymd_hm("2025-09-10 08:00") # change as needed
        df_first$datetime <- start_datetime + minutes((df_first$frame_num - min(df$frame_num)) * 5)
        df_first$Date <- format(df_first$datetime, "%d.%m.%Y")
        df_first$Time <- format(df_first$datetime, "%H:%M")
        
        # Event = 1 for all first appearances
        df_first$Event <- 1
        
        # Prepare final tables
        left_table <- df_first[df_first$side=="Left", c("Date","Time","Event","Treatment","Dose","Dose_unit","Genotype","Replicate")]
        right_table <- df_first[df_first$side=="Right", c("Date","Time","Event","Treatment","Dose","Dose_unit","Genotype","Replicate")]
        
        # Write Excel
        save_path <- file.path(getwd(), paste0(tools::file_path_sans_ext(input$analysis_file), "_events.xlsx"))
        wb <- createWorkbook()
        addWorksheet(wb, "Left")
        addWorksheet(wb, "Right")
        writeData(wb, sheet="Left", left_table)
        writeData(wb, sheet="Right", right_table)
        saveWorkbook(wb, save_path, overwrite = TRUE)
        
        showNotification(paste("Excel exported to:", save_path), type="message")
      })
      
    }
  )
}
